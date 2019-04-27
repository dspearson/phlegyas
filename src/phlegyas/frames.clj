(ns phlegyas.frames
  (:require [phlegyas.util :refer :all]
            [phlegyas.types :refer :all]
            [phlegyas.transformers :refer :all]
            [clojure.core.async :as async]
            [manifold.deferred :as d]
            [taoensso.timbre :as log]
            [manifold.stream :as s]
            [taoensso.timbre :as log]
            [primitive-math :as math
             :refer [ubyte->byte
                     uint->int
                     ushort->short
                     ulong->long]]))

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
  [packet uuid]
  (let [start (System/nanoTime)
        ^java.nio.ByteBuffer frame (wrap-buffer packet)
        len (uint->int (frame-length frame))
        frame-typ (frame-type frame)
        layout (get frame-layouts frame-typ)
        data (into {:frame frame-typ :transaction-id uuid} (for [typ layout] {typ ((get buffer-functions typ) frame)}))
        end (System/nanoTime)]
    (log/debug uuid "disassemble-packet")
    (if (> (/ (- end start) 1000) 1000)
      (log/debug uuid "disassemble time:" (float (/ (- end start) 100000000)) "seconds" ":" frame))
    data))

(defn assemble
  "Takes in a frame and frame type, calculates the final size of the frame
  by adding 5, 4 bytes for the size and 1 byte for the type, looking up `type-bytes`
  from the `phlegyas.types` namespace, allocating `frame-byte` as a byte-array, wrapping
  it as `buffer`, and using ByteBuffer operations to populate `frame-byte`, finally
  returning the assembled output."
  [frame ftype]
  (let [frame-size (uint->int (+ 5 (count frame)))
        type-bytes (get frame-byte ftype)
        frame-byte (byte-array frame-size)
        ^java.nio.ByteBuffer buffer (wrap-buffer frame-byte)]
    (doto buffer
      (^Integer .putInt frame-size)
      (.put (byte type-bytes))
      (.put ^bytes frame))
    frame-byte))

(defn assemble-packet
  "`frame` is a map consisting of all the data required to assemble the final byte-array.
  The keys required are found in the `phlegyas.types` namespace. The layout is a list of
  ordered items representing each element of the frame.

  We feed `frame` into `transform`, which takes a frame and layout, then `flatten`, `pack`,
  and finally `assemble`."
  [frame]
  (let [start (System/nanoTime)
        uuid (:transaction-id frame)
        ftype (:frame frame)
        layout (get frame-layouts ftype)
        data (-> frame (transform layout) flatten pack (assemble ftype))
        end (System/nanoTime)]
    (log/debug uuid "assemble-packet")
    (if (> (/ (- end start) 1000) 1000)
      (log/debug uuid "assemble time:" (float (/ (- end start) 100000000)) "seconds" ":" frame))
    data))

(defn dispatch-frame
  "Cuts frames along their boundaries, returning any partially assembled packets back to
  the frame loop."
  [packet out uuid]
  (log/debug uuid "dispatch-frame")
  (loop [x packet]
    (if (< (count x) 4)
      (vec x)
      (let [l (-> (subvec x 0 4) byte-array ^java.nio.ByteBuffer wrap-buffer frame-length)]
        (if (< (count x) l)
          (vec x)
          (do
            (s/put! out (-> x (subvec 0 l) byte-array (disassemble-packet uuid)))
            (recur (vec (subvec x l)))))))))

(defn frame-assembler
  "Spawn a thread to recursively read from an incoming stream."
  [in out]
  (async/thread
    (loop [packet (vec @(s/take! in))]
      (let [start (System/nanoTime)
            uuid (uuid!)
            partial (dispatch-frame packet out uuid)
            end (System/nanoTime)]
        (log/debug uuid "frame assembler")
        (if (> (/ (- end start) 1000) 1000)
          (log/debug uuid "dispatch-frame time:" (float (/ (- end start) 100000000)) "seconds"))
        (if (s/closed? in)
          (s/close! out)
          (recur (vec (mapcat seq [partial @(s/take! in)]))))))))
