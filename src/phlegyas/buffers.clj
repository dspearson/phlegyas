(ns phlegyas.buffers
  (:require [phlegyas.util :refer :all]
            [taoensso.timbre :as log]
            [primitive-math :as math
             :refer [int->uint
                     short->ushort
                     long->ulong
                     ubyte->byte]]))

(defn get-short
  "Read short from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Short .getShort short->ushort))

(defn get-int
  "Read integer from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Integer .getInt int->uint))

(defn get-long
  "Read long from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Long .getLong long->ulong))

(defn get-string
  "Read string[s] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (let [string-size (-> buffer ^Short .getShort short->ushort)]
    (String. (byte-array (map byte (for [i (range string-size)] (^Byte .get buffer)))) "UTF-8")))

(defn get-byte
  "Read byte from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Byte .get))

(defn get-wnames
  "Read nwname[2] of wname[s] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (let [nwname (-> buffer ^Short .getShort short->ushort)]
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
  (let [nwqid (-> buffer ^Short .getShort short->ushort)]
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

(defn short->bytes
  [x]
  (let [data (byte-array 2)
        buffer (wrap-buffer data)]
    (.putShort buffer x)
    data))

(defn int->bytes
  [x]
  (let [data (byte-array 4)
        buffer (wrap-buffer data)]
    (.putInt buffer x)
    data))

(defn long->bytes
  [x]
  (let [data (byte-array 8)
        buffer (wrap-buffer data)]
    (.putLong buffer x)
    data))

(defn string->bytes
  [x]
  (let [string-bytes (.getBytes x "UTF-8")
        string-size (count string-bytes)
        data (byte-array (+ 2 string-size))
        buffer (wrap-buffer data)]
    (doto buffer
      (.putShort string-size)
      (.put string-bytes))
    data))

(defn byte->bytes
  [x]
  (let [data (byte-array 1)
        buffer (wrap-buffer data)]
    (doto buffer
      (.put (byte x)))
    data))

(defn wname->bytes
  [x]
  (let [buffer-size (+ 2 (apply + (map (fn [x] (+ 2 (count (.getBytes x "UTF-8")))) x)))
        num-of-elements (count x)
        data (byte-array buffer-size)
        buffer (wrap-buffer data)]
    (.putShort buffer num-of-elements)
    (dotimes [n num-of-elements]
      (let [string-bytes (.getBytes (get x n) "UTF-8")]
        (.putShort buffer (count string-bytes))
        (.put buffer string-bytes)))
    data))

(defn qid->bytes
  [x]
  (let [num-of-elements (count x)
        data (byte-array (+ 2 (* 13 num-of-elements)))
        buffer (wrap-buffer data)]
    (.putShort buffer num-of-elements)
    (dotimes [n num-of-elements]
      (let [qid (get x n)]
        (.put buffer (:qid-type qid))
        (.putInt buffer (:qid-vers qid))
        (.putLong buffer (:qid-path qid))))
    data))

(defn bytecoll->bytes
  [x]
  (let [size (count x)
        data (byte-array 4)
        buffer (wrap-buffer data)]
    (doto buffer
      (.putInt size))
    [data x]))

