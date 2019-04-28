(ns phlegyas.buffers
  (:require [phlegyas.util :refer :all]
            [taoensso.timbre :as log]
            [primitive-math :as math
             :refer [int->uint
                     uint->int
                     short->ushort
                     ushort->short
                     long->ulong
                     ulong->long
                     byte->ubyte
                     ubyte->byte]]))

(set! *warn-on-reflection* true)

(defn get-short
  "Read short from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer .getShort))

(defn get-int
  "Read integer from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer .getInt))

(defn get-long
  "Read long from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer .getLong))

(defn get-string
  "Read string[s] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (let [string-size (-> buffer .getShort)]
    (String. (byte-array (map byte (for [i (range string-size)] (^Byte .get buffer)))) "UTF-8")))

(defn get-byte
  "Read byte from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer .get))

(defn get-wnames
  "Read nwname[2] of wname[s] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (let [nwname (-> buffer .getShort short->ushort)]
    (if (= nwname 0)
      []
      (loop [wnames []
             count nwname]
        (if (= count 0)
          wnames
          (recur (conj wnames (get-string buffer)) (- count 1)))))))

(defn get-nwqids
  "Read nqwid[2] of qid[13] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (let [nwqid (-> buffer .getShort short->ushort)]
    (if (= nwqid 0)
      []
      (loop [qids []
             count nwqid]
        (if (= count 0)
          qids
          (recur (conj qids {:qid-type (get-byte buffer) :qid-vers (get-int buffer) :qid-path (get-long buffer)})
                 (- count 1)))))))

(defn get-data
  [^java.nio.ByteBuffer buffer]
  (let [size (get-int buffer)
        data (byte-array size)]
    (.get buffer data 0 size)
    data))

(defn put-short
  [x]
  (let [data (byte-array 2)
        ^java.nio.ByteBuffer buffer (wrap-buffer data)]
    (.putShort buffer (ushort->short x))
    data))

(defn put-int
  [x]
  (let [data (byte-array 4)
        ^java.nio.ByteBuffer buffer (wrap-buffer data)]
    (.putInt buffer (uint->int x))
    data))

(defn put-long
  [x]
  (let [data (byte-array 8)
        ^java.nio.ByteBuffer buffer (wrap-buffer data)]
    (.putLong buffer (ulong->long x))
    data))

(defn put-string
  [x]
  (let [string-bytes (.getBytes ^String x "UTF-8")
        string-size (count string-bytes)
        data (byte-array (+ 2 string-size))
        ^java.nio.ByteBuffer buffer (wrap-buffer data)]
    (doto buffer
      (.putShort (ushort->short string-size))
      (.put string-bytes))
    data))

(defn put-byte
  [x]
  (let [data (byte-array 1)
        ^java.nio.ByteBuffer buffer (wrap-buffer data)]
    (doto buffer
      (.put (ubyte->byte x)))
    data))

(defn put-wname
  [x]
  (let [buffer-size (+ 2 (apply + (map (fn [x] (+ 2 (count (.getBytes ^String x "UTF-8")))) x)))
        num-of-elements (count x)
        data (byte-array buffer-size)
        ^java.nio.ByteBuffer buffer (wrap-buffer data)]
    (.putShort buffer (ushort->short num-of-elements))
    (dotimes [n num-of-elements]
      (let [string-bytes (.getBytes ^String (get x n) "UTF-8")]
        (.putShort buffer (ushort->short (count string-bytes)))
        (.put buffer string-bytes)))
    data))

(defn put-qid
  [x]
  (let [num-of-elements (count x)
        data (byte-array (+ 2 (* 13 num-of-elements)))
        ^java.nio.ByteBuffer buffer (wrap-buffer data)]
    (^Short .putShort buffer num-of-elements)
    (dotimes [n num-of-elements]
      (let [qid (get x n)]
        (.put buffer (ubyte->byte (:qid-type qid)))
        (.putInt buffer (uint->int (:qid-vers qid)))
        (.putLong buffer (ulong->long (:qid-path qid)))))
    data))

(defn put-bytecoll
  [x]
  (let [size (count x)
        data (byte-array 4)
        ^java.nio.ByteBuffer buffer (wrap-buffer data)]
    (doto buffer
      (.putInt size))
    [data x]))
