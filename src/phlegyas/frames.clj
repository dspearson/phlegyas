(ns phlegyas.frames
  (:require [phlegyas.util :refer :all]
            [phlegyas.types :refer :all]
            [clojure.core.async :as async]
            [manifold.deferred :as d]
            [manifold.stream :as s]
            [primitive-math :as math
             :refer [uint->int]]))

(defmacro frame-length
  "Check reported frame length."
  [buffer]
  `(if (< (^Integer .remaining ^java.nio.ByteBuffer ~buffer) 4)
     0
     (^Integer .getInt ^java.nio.ByteBuffer ~buffer)))

(defmacro frame-type
  "Look up frame type in the reverse lookup table `reverse-frame-byte`,
  defined in the `phlegyas.types` namespace, and `keywordizes` it for us."
  [buffer]
  `((keywordize (^Byte .get ^java.nio.ByteBuffer ~buffer)) '~reverse-frame-byte))

(defn disassemble-packet
  "Takes in a byte-array, and attempts to decode it. Produces a map, matching that
  of the message type found in the `phlegyas.types` namespace."
  [packet]
  (let [^java.nio.ByteBuffer frame (wrap-buffer packet)
        len (uint->int (frame-length frame))
        ftype (frame-type frame)
        layout (get frame-layouts ftype)]
    (into {:frame ftype} (for [field layout] {field ((get get-operation field) frame)}))))

(defn assemble
  "Takes in a frame and frame type, calculates the final size of the frame
  by adding 5, 4 bytes for the size and 1 byte for the type, looking up `type-bytes`
  from the `phlegyas.types` namespace, and adding the size and type to the sequence."
  [frame ftype]
  (let [frame-size (+ 5 (apply + (map count frame)))
        type-bytes (get frame-byte ftype)]
    (cons ((:fsize put-operation) frame-size) (cons ((:ftype put-operation) type-bytes) frame))))

(defn assemble-packet
  "Takes in a map representing a frame (see `frame-layouts` in the `phlegyas.types` namespace, and
  intro(9P) manual), and encodes it."
  [frame]
  (let [ftype (:frame frame)
        layout (get frame-layouts ftype)]
    (-> (for [typ layout] ((get put-operation typ) (get frame typ))) flatten (assemble ftype) pack)))

(defn dispatch-frame
  "Cuts frames along their boundaries, returning any partially assembled packets back to
  the frame loop."
  [packet out]
  (loop [x packet]
    (if (< (count x) 4)
      (vec x)
      (let [l (-> (subvec x 0 4) byte-array ^java.nio.ByteBuffer wrap-buffer frame-length)]
        (if (< (count x) l)
          (vec x)
          (do
            (s/put! out (-> x (subvec 0 l) byte-array disassemble-packet))
            (recur (vec (subvec x l)))))))))

(defn frame-assembler
  "Spawn a thread to recursively read from an incoming stream."
  [in out]
  (async/thread
    (loop [packet (vec @(s/take! in))]
      (let [partial (dispatch-frame packet out)]
        (if (s/closed? in)
          (s/close! out)
          (recur (vec (mapcat seq [partial @(s/take! in)]))))))))
