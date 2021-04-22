(ns phlegyas.util
  (:require [buddy.core.hash :as hash]
            [buddy.core.codecs :as codecs])
  (:import java.nio.ByteBuffer))

(defmacro defn-frame-binding
  "A wrapper around defn which wraps the body forms in `with-frame-bindings`,
  which is an anaphoric macro that creates a lexical environment and defines
  a number of useful variables for us."
  ([name args body]
   `(defn ~name ~args (do (with-frame-bindings ~body))))
  ([name docstr args body]
   `(defn ~name ~docstr ~args (do (with-frame-bindings ~body)))))

(defmacro with-frame-bindings
  ([body]
   `(with-frame-bindings ~'frame ~body))
  ([frame body]
   `(let [frame# ~frame
          ~'state (:state ~'connection)
          ~'current-state (if (instance? clojure.lang.Atom ~'state) ~'@state {})
          ~'frame-ftype (:frame frame#)
          ~'frame-tag (:tag frame#)
          ~'frame-qid-type (:qid-type frame#)
          ~'frame-qid-vers (:qid-vers frame#)
          ~'frame-qid-path (:qid-path frame#)
          ~'frame-nwqids (:nwqids frame#)
          ~'frame-wnames (:wnames frame#)
          ~'frame-iounit (:iounit frame#)
          ~'frame-iomode (:iomode frame#)
          ~'frame-count (:count frame#)
          ~'frame-ssize (:ssize frame#)
          ~'frame-size (:size frame#)
          ~'frame-type (:type frame#)
          ~'frame-mode (:mode frame#)
          ~'frame-atime (:atime frame#)
          ~'frame-mtime (:mtime frame#)
          ~'frame-length (:length frame#)
          ~'frame-name (:name frame#)
          ~'frame-uname (:uname frame#)
          ~'frame-muid (:muid frame#)
          ~'frame-data (:data frame#)
          ~'frame-offset (:offset frame#)
          ~'frame-fid (:fid frame#)
          ~'frame-ename (:ename frame#)
          ~'frame-version (:version frame#)
          ~'frame-afid (:afid frame#)
          ~'frame-aname (:aname frame#)
          ~'frame-oldtag (:oldtag frame#)
          ~'frame-newfid (:newfid frame#)
          ~'frame-msize (:msize frame#)
          ~'mapping (or (get (:mapping ~'current-state) (keywordize ~'frame-fid)) {})
          ~'fs-name (:filesystem ~'mapping)
          ~'fs (get (:fs-map ~'current-state) ~'fs-name)
          ~'fsid (:id ~'fs)
          ~'path (:path ~'mapping)]
      (~@body))))

(defmacro with-server
  "Helper macro for tests and one-off connections."
  [server & body]
  `(let [server# ~server]
     (try
       ~@body
       (finally
         (.close ^java.io.Closeable server#)))))

(defn conj-val
  "Remove a value from an atomic set."
  [a val]
  (swap! a (fn [x] (conj x val))))

(defn disj-val
  "Remove a value from an atomic set."
  [a val]
  (swap! a (fn [x] (disj x val))))

(defn assoc-val
  "Associate a key with the value in an atomic map."
  [a key val]
  (swap! a assoc key val))

(defn dissoc-val
  "Remove a key from an atomic map."
  [a key]
  (swap! a (fn [x] (dissoc x key))))

(defn wrap-buffer
  "Wraps a byte-array in a Java ByteBuffer, using little-endian
  byte order as required by the 9P2000 protocol."
  [x]
  (if (nil? x)
    (ByteBuffer/wrap (byte-array 0))
    (let [buffer (ByteBuffer/wrap x)]
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

(defn sizeof-string
  "Count the number of bytes in a string."
  [s]
  (count (#^bytes .getBytes ^String s "UTF-8")))

(defn parse-int
  "Coerce a string to integer."
  [s]
  (Integer/parseInt (re-find #"\d+" s)))

(defn sha-str
  [s]
  (codecs/bytes->hex (hash/sha256 s)))
