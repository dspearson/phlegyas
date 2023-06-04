(ns phlegyas.util
  (:require
   [clojure.string :refer [join]]
   [clojure.java.io :as io])
  (:import
   (java.nio ByteBuffer)
   (java.security MessageDigest)
   (java.util UUID)))

(defn epoch!
  []
  (int (/ (System/currentTimeMillis) 1000)))

(defn uuid!
  []
  (let [uuid (UUID/randomUUID)
        buf (-> (java.nio.ByteBuffer/allocate 16)
                (.order java.nio.ByteOrder/LITTLE_ENDIAN))]
    (.putLong buf (.getMostSignificantBits uuid))
    (.putLong buf (.getLeastSignificantBits uuid))
    (.array buf)))

(defn uuid=
  ([x] true)
  ([x y]
   (java.util.Arrays/equals x y))
  ([x y & more]
   (if (uuid= x y)
     (if (next more)
       (recur y (first more) (next more))
       (uuid= y (first more)))
     false)))

(defn octal->int
  "Convert an octal string to an integer."
  [s]
  (Integer/parseInt s 8))

(defn str->file
  [filename]
  (when-let [f (io/file filename)]
    (when (and (.exists f)
               (.isFile f))
      f)))

(defmacro defn-frame-binding
  "A wrapper around defn which wraps the body forms in `with-frame-bindings`,
  which is an anaphoric macro that creates a lexical environment and defines
  a number of useful variables for us."
  ([name args body]
   `(defn ~name ~args (with-frame-bindings ~body)))
  ([name docstr args body]
   `(defn ~name ~docstr ~args (with-frame-bindings ~body))))

(defmacro with-frame-bindings
  ([body]
   `(with-frame-bindings ~'frame ~body))
  ([frame body]
   `(let [frame#                    ~frame
          {~'state :state}          ~'connection
          ~'current-state           (if (instance? clojure.lang.Atom ~'state) ~'@state {})
          {~'frame-ftype    :frame
           ~'frame-tag      :tag
           ~'frame-qid-type :qid-type
           ~'frame-qid-vers :qid-vers
           ~'frame-qid-path :qid-path
           ~'frame-nwqids   :nwqids
           ~'frame-wnames   :wnames
           ~'frame-iounit   :iounit
           ~'frame-iomode   :iomode
           ~'frame-count    :count
           ~'frame-size     :size
           ~'frame-type     :type
           ~'frame-perm     :perm
           ~'frame-atime    :atime
           ~'frame-mtime    :mtime
           ~'frame-length   :length
           ~'frame-name     :name
           ~'frame-uname    :uname
           ~'frame-muid     :muid
           ~'frame-data     :data
           ~'frame-offset   :offset
           ~'frame-fid      :fid
           ~'frame-ename    :ename
           ~'frame-version  :version
           ~'frame-afid     :afid
           ~'frame-aname    :aname
           ~'frame-oldtag   :oldtag
           ~'frame-newfid   :newfid
           ~'frame-msize    :msize} frame#
          ~'mapping                 (or ((keywordize ~'frame-fid) (:mapping ~'current-state)) {})
          {~'fs-name :filesystem}   ~'mapping
          ~'fs                      ((:fs-map ~'current-state) ~'fs-name)
          {~'fsid :id}              ~'fs
          {~'path :path}            ~'mapping]
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
  `(into {} (map (fn [[k# v#]] [(keywordize v#) k#]) ~table)))

(defn sizeof-string
  "Count the number of bytes in a string."
  [s]
  (count (.getBytes s "UTF-8")))

(defn parse-int
  "Coerce a string to integer."
  [s]
  (Integer/parseInt (re-find #"\d+" s)))

(defn sha-str
  [s]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes ^String s "UTF-8"))]
    (join (map (partial format "%02x") digest))))
