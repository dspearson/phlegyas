(ns phlegyas.util
  (:import java.nio.ByteBuffer))

(defn wrap-buffer
  "Wraps a byte-array in a Java ByteBuffer, using little-endian
  byte order as required by the 9P2000 protocol."
  [byte-array']
  (if (nil? byte-array')
    (ByteBuffer/wrap (byte-array 0))
    (let [buffer (ByteBuffer/wrap byte-array')]
      (.order buffer java.nio.ByteOrder/LITTLE_ENDIAN))))

(defn pack
  "Pack a sequence into a byte array."
  [coll]
  (byte-array (mapcat seq coll)))

(defmacro keywordize
  "Turn argument into a string, then a keyword."
  [x]
  `(-> ~x str keyword))

(defmacro reverse-map
  "Reverses a map, keywordizing the value."
  [table]
  `(into {} (for [[k# v#] ~table] [(keywordize v#) k#])))
