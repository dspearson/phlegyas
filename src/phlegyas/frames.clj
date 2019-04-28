(ns phlegyas.frames
  (:require [phlegyas.util :refer :all]
            [phlegyas.types :refer :all]
            [phlegyas.buffers :refer :all]
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

(set! *warn-on-reflection* true)

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
  [packet & [uuid]]
  (log/debug uuid "disassemble-packet")
  (let [start (System/nanoTime)
        ^java.nio.ByteBuffer frame (wrap-buffer packet)
        len (uint->int (frame-length frame))
        frame-typ (frame-type frame)
        layout (get frame-layouts frame-typ)
        data (into {:frame frame-typ :transaction-id uuid} (for [typ layout] {typ ((get get-operation typ) frame)}))
        end (System/nanoTime)]
    (if (> (/ (- end start) 1000) 1000)
      (log/debug uuid "disassemble time:" (float (/ (- end start) 1000000)) "msecs" ":" frame))
    data))

(defn assemble
  "Takes in a frame and frame type, calculates the final size of the frame
  by adding 5, 4 bytes for the size and 1 byte for the type, looking up `type-bytes`
  from the `phlegyas.types` namespace, allocating `frame-byte` as a byte-array, wrapping
  it as `buffer`, and using ByteBuffer operations to populate `frame-byte`, finally
  returning the assembled output."
  [frame ftype]
  (let [frame-size (+ 5 (apply + (map count frame)))
        type-bytes (get frame-byte ftype)]
    (cons (int->bytes frame-size) (cons (byte->bytes type-bytes) frame))))

(defn assemble-packet
  [frame]
  (log/trace (:transaction-id frame) "assemble-packet")
  (log/trace (:transaction-id frame) frame)
  (let [start (System/nanoTime)
        uuid (:transaction-id frame)
        ftype (:frame frame)
        layout (get frame-layouts ftype)
        data (-> (for [typ layout] ((get put-operation typ) (get frame typ))) flatten (assemble ftype) pack)
        end (System/nanoTime)]
    (if (> (/ (- end start) 1000000) 100)
      (log/debug uuid "slow assemble time:" (float (/ (- end start) 1000000)) "msecs" ":" frame))
    data))

(defn dispatch-frame
  "Cuts frames along their boundaries, returning any partially assembled packets back to
  the frame loop."
  [packet out uuid]
  (log/trace uuid "dispatch-frame")
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
        (log/trace uuid "frame assembler")
        (if (> (/ (- end start) 1000000) 100)
          (log/debug uuid "slow dispatch-frame time:" (float (/ (- end start) 1000000)) "msecs"))
        (if (s/closed? in)
          (s/close! out)
          (recur (vec (mapcat seq [partial @(s/take! in)]))))))))
