(ns phlegyas.util
  (:require [primitive-math :as math
             :refer [int->uint short->ushort
                     uint->int ushort->short]])
  (:import java.nio.ByteBuffer))

(defn sizeof-stringbuf
  [s]
  (+ 2 (count (.getBytes s "UTF-8"))))

(defn sizeof-string
  [s]
  (count (.getBytes s "UTF-8")))

(defn current-seconds
  []
  (-> (java.util.Date.) .getTime (quot 1000)))

(defn new-tag
  []
  (unchecked-short (rand-int 65535)))

(defn lookup
  [x table]
  ((keyword (str x)) table))

(defn wrap-buf
  [buf]
  (if (nil? buf)
    (ByteBuffer/wrap (byte-array 0))
    (let [buffer (ByteBuffer/wrap buf)]
      (.order buffer java.nio.ByteOrder/LITTLE_ENDIAN))))

(defn pack
  [x]
  (byte-array (mapcat seq x)))

(defmacro keywordize
  [x]
  `(-> ~x str keyword))

(defmacro gen-lookup
  [table]
  `(into {} (for [[k# v#] ~table] [(keyword (str v#)) k#])))
