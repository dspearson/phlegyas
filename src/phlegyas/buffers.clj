(ns phlegyas.buffers
  "Raw get and put operations for 9P buffers. See INTRO(9P) for more information."
  (:require [phlegyas.util :refer [wrap-buffer]]
            [primitive-math :as math
             :refer [uint->int
                     short->ushort
                     ushort->short
                     ulong->long
                     ubyte->byte]]))

(defn get-short
  "Read a short from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (.getShort buffer))

(defn get-int
  "Read an integer from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (.getInt buffer))

(defn get-long
  "Read a long from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (.getLong buffer))

(defn get-string
  "Read 9P string[s] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (let [string-size (.getShort buffer)]
    (String. (byte-array (map byte (for [_ (range string-size)] (^Byte .get buffer)))) "UTF-8")))

(defn get-byte
  "Read a byte from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (.get buffer))

(defn get-wnames
  "Read nwname[2] of wname[s] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (let [nwname (-> buffer .getShort short->ushort)]
    (if (zero? nwname)
      []
      (loop [wnames []
             count  nwname]
        (if (zero? count)
          wnames
          (recur (conj wnames (get-string buffer)) (dec count)))))))

(defn get-nwqids
  "Read nqwid[2] of qid[13] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (let [nwqid (-> buffer .getShort short->ushort)]
    (if (zero? nwqid)
      []
      (loop [qids  []
             count nwqid]
        (if (zero? count)
          qids
          (recur (conj qids {:qid-type (get-byte buffer) :qid-vers (get-int buffer) :qid-path (get-long buffer)})
                 (dec count)))))))

(defn get-data
  "Read data[count] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (let [size (get-int buffer)
        data (byte-array size)]
    (.get buffer data 0 size)
    data))

(defn put-short
  "Wrap a short in a byte-array."
  [x]
  (let [data                        (byte-array 2)
        ^java.nio.ByteBuffer buffer (wrap-buffer data)]
    (.putShort buffer (ushort->short x))
    data))

(defn put-int
  "Wrap an int in a byte-array."
  [x]
  (let [data                        (byte-array 4)
        ^java.nio.ByteBuffer buffer (wrap-buffer data)]
    (.putInt buffer (uint->int x))
    data))

(defn put-long
  "Wrap a long in a byte-array."
  [x]
  (let [data                        (byte-array 8)
        ^java.nio.ByteBuffer buffer (wrap-buffer data)]
    (.putLong buffer (ulong->long x))
    data))

(defn put-string
  "Wrap 9P string[s] in a byte-array."
  [x]
  (let [string-bytes                (.getBytes ^String x "UTF-8")
        string-size                 (count string-bytes)
        data                        (byte-array (+ 2 string-size))
        ^java.nio.ByteBuffer buffer (wrap-buffer data)]
    (doto buffer
      (.putShort (ushort->short string-size))
      (.put string-bytes))
    data))

(defn put-byte
  "Wrap a byte in a byte-array."
  [x]
  (let [data                        (byte-array 1)
        ^java.nio.ByteBuffer buffer (wrap-buffer data)]
    (doto buffer
      (.put (ubyte->byte x)))
    data))

(defn put-wname
  "Wrap nwname*wname in a byte-array. Prepends the data with a short,
  representing the number of elements in the data."
  [x]
  (let [buffer-size                 (+ 2 (apply + (map (fn [x] (+ 2 (count (.getBytes ^String x "UTF-8")))) x)))
        num-of-elements             (count x)
        data                        (byte-array buffer-size)
        ^java.nio.ByteBuffer buffer (wrap-buffer data)]
    (.putShort buffer (ushort->short num-of-elements))
    (dotimes [n num-of-elements]
      (let [string-bytes (.getBytes ^String (get x n) "UTF-8")]
        (.putShort buffer (ushort->short (count string-bytes)))
        (.put buffer string-bytes)))
    data))

(defn put-qid
  "Wrap nwqid*wqid in a byte-array. Prepends the data with a short,
  representing the number of elements in the data."
  [x]
  (let [num-of-elements             (count x)
        data                        (byte-array (+ 2 (* 13 num-of-elements)))
        ^java.nio.ByteBuffer buffer (wrap-buffer data)]
    (^Short .putShort buffer num-of-elements)
    (dotimes [n num-of-elements]
      (let [qid (nth x n)]
        (.put buffer (ubyte->byte (:qid-type qid)))
        (.putInt buffer (uint->int (:qid-vers qid)))
        (.putLong buffer (ulong->long (:qid-path qid)))))
    data))

(defn put-bytecoll
  "Wrap a 9P byte collection in byte-array. Prepends the data with an integer representing
  the byte count of the data."
  [x]
  (let [size                        (count x)
        data                        (byte-array 4)
        ^java.nio.ByteBuffer buffer (wrap-buffer data)]
    (doto buffer
      (.putInt size))
    [data x]))
