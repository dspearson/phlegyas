(ns phlegyas.reader
  (:require [primitive-math :as math
             :refer [int->uint short->ushort
                     uint->int ushort->short
                     long->ulong ulong->long]]
            [taoensso.timbre :as log]))

(defn reader-tag
  [buf]
  (-> buf .getShort))

(defn reader-msize
  [buf]
  (-> buf .getInt int->uint))

(defn reader-string
  [buf]
  (log/debug "In reader-string.")
  (let [string-size (-> buf .getShort short->ushort)]
    (String. (byte-array (map byte (for [i (range string-size)] (.get buf)))) "UTF-8")))

(defn reader-version
  [buf]
  (reader-string buf))

(defn reader-uname
  [buf]
  (reader-string buf))

(defn reader-aname
  [buf]
  (reader-string buf))

(defn reader-qid
  [buf]
  (byte-array (map byte (for [i (range 0 13)] (.get buf)))))

(defn reader-fid
  [buf]
  (-> buf .getInt))

(defn reader-newfid
  [buf]
  (-> buf .getInt))

(defn reader-afid
  [buf]
  (-> buf .getInt))

(defn reader-wname
  [buf]
  (let [nwname (-> buf .getShort short->ushort)]
    (log/debug "In reader-wname, nwname:" nwname)
    (if (= nwname 0)
      []
      (loop [wnames []
             count nwname]
        (if (= count 0)
          wnames
          (recur (conj wnames (reader-string buf)) (- count 1)))))))

(defn reader-mode
  [buf]
  (-> buf .get))

(defn reader-offset
  [buf]
  (-> buf .getLong))

(defn reader-count
  [buf]
  (-> buf .getInt))

(defn reader-size
  [buf]
  (-> buf .getShort))

(defn reader-ssize
  [buf]
  (-> buf .getShort))

(defn reader-type
  [buf]
  (-> buf .getShort))

(defn reader-dev
  [buf]
  (-> buf .getInt))

(defn reader-qtype
  [buf]
  (-> buf .get))

(defn reader-qvers
  [buf]
  (-> buf .getInt))

(defn reader-qpath
  [buf]
  (-> buf .getLong))

(defn reader-mode
  [buf]
  (-> buf .getInt))

(defn reader-atime
  [buf]
  (-> buf .getInt))

(defn reader-mtime
  [buf]
  (-> buf .getInt))

(defn reader-mtime
  [buf]
  (-> buf .getInt))

(defn reader-len
  [buf]
  (-> buf .getLong))

(defn reader-name
  [buf]
  (reader-string buf))

(defn reader-uid
  [buf]
  (reader-string buf))

(defn reader-gid
  [buf]
  (reader-string buf))

(defn reader-muid
  [buf]
  (reader-string buf))

(defn reader-iomode
  [buf]
  (-> buf .get))
