(ns phlegyas.frames
  (:require [taoensso.timbre :as log
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]
            [primitive-math :as math
             :refer [int->uint short->ushort
                     uint->int ushort->short
                     long->ulong ulong->long]]
            [clojure.core.async :as async]
            [manifold.stream :as s]
            [phlegyas.types :refer :all]
            [phlegyas.transformer :refer :all]
            [phlegyas.util :refer :all]))

(defmacro flength
  "Check reported frame length."
  [x]
  `(if (< (.remaining ~x) 4)
     0
     (.getInt ~x)))

(defmacro ftype
  [x]
  `((keywordize (.get ~x)) '~reverse-message-type))

(defn disassemble-packet
  [frame]
  (let [len (flength frame)
        typ (ftype frame)
        layout (typ frame-layouts)]
    (into {:frame typ} (for [msg layout] {msg ((msg type-resolvers) frame)}))))

(defn assemble
  "Takes in a frame and frame type, calculates the final size of the frame
  by adding 5, 4 bytes for the size and 1 byte for the type, looking up `type-bytes`
  from the types namespace, allocating `buf` as a byte-array, wrapping it as `x`,
  and using ByteBuffer operations to populate `buf`, finally returning the assembled
  output."
  [frame typ]
  (let [frame-size (int (+ 5 (count frame)))
        type-bytes (typ message-type)
        buf (byte-array frame-size)
        x (wrap-buf buf)]
    (doto x
      (.putInt frame-size)
      (.put (byte type-bytes))
      (.put frame))
    buf))

(defn assemble-packet
  "`frame` is a map consisting of all the data required to assemble the final byte-array.
  The keys required are found in the types namespace, and we get the frame layout by looking
  up either in Tframe or Rframe, depending on the type of frame we are constructing. The layout
  is a list of ordered items representing each element of the frame, which is optionally a pair.
  When it is a pair, the second element of the pair is a STATIC value which is always used.
  We feed `frame` into `transform`, which takes a frame and layout, then flatten, pack, and
  finally assemble."
  [frame]
  (let [typ (:frame frame)
        layout (typ frame-layouts)
        data (into frame (for [x (typ frame-layouts)] (assoc {} x (x frame))))]
    (-> data (transform layout) flatten pack (assemble typ))))

(defn dispatch-frame
  [packet out]
  (loop [x packet]
    (if (< (count x) 4)
      x
      (let [l (-> x (subvec 0 4) byte-array wrap-buf flength)]
        (if (< (count x) l)
          x
          (do
            (s/put! out (-> x (subvec 0 l) byte-array wrap-buf disassemble-packet))
            (recur (subvec x l))))))))

(defn frame-assembler
  [in out]
  (async/thread
    (loop [packet (vec @(s/take! in))]
      (let [partial (dispatch-frame packet out)]
        (if (s/closed? in)
            (s/close! out)
          (recur (vec (mapcat seq [partial @(s/take! in)]))))))))
