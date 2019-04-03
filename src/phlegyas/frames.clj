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
            [phlegyas.transformers :refer :all]
            [phlegyas.util :refer :all]))

(defn flength
  "Check reported frame length."
  [x]
  (if (< (.remaining x) 4)
    0
    (-> x .getInt int->uint)))

(defn ftype
  [x]
  (let [typ (.get x)
        trait (if (= (rem (int typ) 2) 0) :T :R)]
    [trait ((-> typ str keyword) message-type-r)]))

(defn disassemble-packet
  [frame]
  (log/debug "frame:" frame)
  (let [len (flength frame)
        [trait typ] (ftype frame)
        frame-lookup-table (if (= trait :T) Tframe Rframe)
        layout (map first (typ frame-lookup-table))]
    (log/debug "lookup table:" frame-lookup-table)
    (log/debug "len:" len)
    (log/debug "typ:" typ)
    (log/debug "Layout to decode:" layout)
    (into {:trait trait :frame typ} (for [msg layout] {msg ((msg type-resolvers) frame)}))))

(defn assemble
  "Takes in a frame and frame type, calculates the final size of the frame
  by adding 5, 4 bytes for the size and 1 byte for the type, looking up `type-bytes`
  from the types namespace, allocating `buf` as a byte-array, wrapping it as `x`,
  and using ByteBuffer operations to populate `buf`, finally returning the assembled
  output."
  [frame trait typ]
  (log/debug "Assembling:")
  (log/debug "Frame type:" typ)
  (let [frame-size (int (+ 5 (count frame)))
        type-bytes (+ (if (= :T trait) 0 1) (typ message-type))
        buf (byte-array frame-size)
        x (wrap-buf buf)]
    (log/debug "Size:" frame-size)
    (doto x
      (.putInt frame-size)
      (.put (byte type-bytes))
      (.put frame))
    buf))

(defn construct
  "Takes in data and frame and assembles it into a byte-array.
  If the data requires a transformer, it looks this up in the transformer table
  as defined in the transformers namespace, and executes the function on the data.
  Otherwise, it lokos up the buffer operator in the type-bufop table as defined in
  the types namespace, and executes this operation on the data to write to the given
  byte-array by wrapping it first in a ByteBuffer."
  [data layout]
  (log/debug "In construct.")
  (log/debug data)
  (for [k layout]
    (if (some? (k transformer))
      ((k transformer) (k data))
      (let [buf (byte-array (k type-size))]
        ((k type-bufop) (wrap-buf buf) (k data))
        (log/debug "type:" k "buf:")
        buf))))

(defn construct-packet
  "Takes in a trait and a frame.

  `frame` is a map consisting of all the data required to assemble the final byte-array.
  The keys required are found in the types namespace, and we get the frame layout by looking
  up either in Tframe or Rframe, depending on the type of frame we are constructing. The layout
  is a list of ordered items representing each element of the frame, which is optionally a pair.
  When it is a pair, the second element of the pair is a STATIC value which is always used.
  We feed `frame` into `construct`, which takes a frame and layout, then flatten, pack, and
  finally assemble."
  [trait frame]
  (log/debug "In construct-packet.")
  (log/debug "Trait:" trait "Data:" frame)
  (let [typ (:frame frame)
        frame-lookup-table (if (= trait :T) Tframe Rframe)
        layout (map first (typ frame-lookup-table))
        data (into frame (for [x (typ frame-lookup-table)] (assoc {} (first x) (or (second x) ((first x) frame)))))]
    (log/debug "Target output layout:")
    (-> data (construct layout) flatten pack (assemble trait typ))))

(defn dispatch-frame
  [packet out]
  (log/debug "In dispatch-frame")
  (loop [x packet]
    (if (< (count x) 4)
      x
      (let [l (-> x (subvec 0 4) byte-array wrap-buf flength)]
        (log/debug "inner loop")
        (log/debug "l:" l)
        (log/debug "count:" (count x))
        (if (< (count x) l)
          x
          (do
            (log/debug "putting!")
            (s/put! out (-> x (subvec 0 l) byte-array wrap-buf disassemble-packet))
            (recur (subvec x l))))))))

(defn frame-assembler
  [in out]
  (async/thread
    (log/debug "Started frame assembly thread.")
    (loop [packet (vec @(s/take! in))]
      (log/debug "frame assembler inner loop")
      (log/debug "packet:" packet)
      (if (empty? packet)
        true
        (recur (vec (mapcat seq [(dispatch-frame packet out) @(s/take! in)])))))))
