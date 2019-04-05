(ns phlegyas.frames
  (:require [phlegyas.util :refer :all]
            [phlegyas.types :refer :all]
            [phlegyas.transformers :refer :all]))

(defmacro frame-length
  "Check reported frame length."
  [buffer]
  `(if (< (.remaining ~buffer) 4)
     0
     (.getInt ~buffer)))

(defmacro frame-type
  "Look up frame type in the reverse lookup table `reverse-message-type`,
  defined in the `phlegyas.types` namespace, and `keywordizes` it for us."
  [buffer]
  `((keywordize (.get ~buffer)) '~reverse-frame-byte))

(defn disassemble-packet
  "Takes in a byte-array, and attempts to decode it. Produces a map, matching that
  of the message type found in the `phlegyas.types` namespace."
  [packet]
  (let [frame (wrap-buffer packet)
        len (frame-length frame)
        frame-typ (frame-type frame)
        layout (get frame-layouts frame-typ)]
    (into {:frame frame-typ} (for [typ layout] {typ ((get buffer-functions typ) frame)}))))

(defn assemble
  "Takes in a frame and frame type, calculates the final size of the frame
  by adding 5, 4 bytes for the size and 1 byte for the type, looking up `type-bytes`
  from the `phlegyas.types` namespace, allocating `frame-bytes` as a byte-array, wrapping
  it as `buffer`, and using ByteBuffer operations to populate `frame-bytes`, finally
  returning the assembled output."
  [frame ftype]
  (let [frame-size (+ 5 (count frame))
        type-bytes (get frame-byte ftype)
        frame-bytes (byte-array frame-size)
        buffer (wrap-buffer frame-bytes)]
    (doto buffer
      (.putInt frame-size)
      (.put (byte type-bytes))
      (.put frame))
    frame-bytes))

(defn assemble-packet
  "`frame` is a map consisting of all the data required to assemble the final byte-array.
  The keys required are found in the `phlegyas.types` namespace. The layout is a list of
  ordered items representing each element of the frame.

  We feed `frame` into `transform`, which takes a frame and layout, then `flatten`, `pack`,
  and finally `assemble`."
  [frame]
  (let [ftype (:frame frame)
        layout (get frame-layouts ftype)]
    (-> frame (transform layout) flatten pack (assemble ftype))))
