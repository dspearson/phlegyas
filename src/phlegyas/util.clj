(ns phlegyas.util
  (:import java.nio.ByteBuffer))

(defmacro with-frame-bindings
  [data body]
  `(let [frame# ~data
         ~'frame-type (:frame frame#)
         ~'frame-tag (:tag frame#)
         ~'frame-qid-type (:qid-type frame#)
         ~'frame-qid-vers (:qid-vers frame#)
         ~'frame-qid-path (:qid-path frame#)
         ~'frame-nwqids (:nwqids frame#)
         ~'frame-iounit (:iounit frame#)
         ~'frame-count (:count frame#)
         ~'frame-ssize (:ssize frame#)
         ~'frame-size (:size frame#)
         ~'frame-type (:type frame#)
         ~'frame-mode (:mode frame#)
         ~'frame-atime (:atime frame#)
         ~'frame-mtime (:mtime frame#)
         ~'frame-length (:length frame#)
         ~'frame-name (:name frame#)
         ~'frame-uid (:uid frame#)
         ~'frame-gid (:gid frame#)
         ~'frame-muid (:muid frame#)
         ~'frame-data (:data frame#)
         ~'frame-offset (:offset frame#)
         ~'frame-fid (:fid frame#)
         ~'frame-ename (:ename frame#)
         ~'frame-version (:version frame#)
         ~'frame-msize (:msizez frame#)]
       ~@body))

(defn wrap-buffer
  "Wraps a byte-array in a Java ByteBuffer, using little-endian
  byte order as required by the 9P2000 protocol."
  [x]
  (if (nil? x)
    (ByteBuffer/wrap (byte-array 0))
    (let [buffer (ByteBuffer/wrap x)]
      (.order buffer java.nio.ByteOrder/LITTLE_ENDIAN))))

(defn uuid!
  []
  (.toString (java.util.UUID/randomUUID)))

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

(defn sizeof-string
  "Count the number of bytes in a string."
  [s]
  (count (^Bytes .getBytes ^String s "UTF-8")))
