(ns phlegyas.reader
  (:require [primitive-math :as math
             :refer [int->uint short->ushort
                     uint->int ushort->short
                     long->ulong ulong->long]]
            [taoensso.timbre :as log]))

(defn reader-tag
  [buf]
  (-> buf .getShort short->ushort))

(defn reader-oldtag
  [buf]
  (-> buf .getShort short->ushort))

(defn reader-msize
  [buf]
  (-> buf .getInt int->uint))

(defn reader-string
  [buf]
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

(defn reader-ename
  [buf]
  (reader-string buf))

(defn reader-qid
  [buf]
  (byte-array (map byte (for [i (range 0 13)] (.get buf)))))

(defn reader-fid
  [buf]
  (-> buf .getInt int->uint))

(defn reader-newfid
  [buf]
  (-> buf .getInt int->uint))

(defn reader-afid
  [buf]
  (-> buf .getInt int->uint))

(defn reader-wname
  [buf]
  (let [nwname (-> buf .getShort short->ushort)]
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

(defn reader-perm
  [buf]
  (-> buf .getInt int->uint))

(defn reader-offset
  [buf]
  (-> buf .getLong long->ulong))

(defn reader-count
  [buf]
  (-> buf .getInt int->uint))

(defn reader-size
  [buf]
  (-> buf .getShort short->ushort))

(defn reader-ssize
  [buf]
  (-> buf .getShort short->ushort))

(defn reader-type
  [buf]
  (-> buf .getShort short->ushort))

(defn reader-dev
  [buf]
  (-> buf .getInt int->uint))

(defn reader-qtype
  [buf]
  (-> buf .get))

(defn reader-qvers
  [buf]
  (-> buf .getInt int->uint))

(defn reader-qpath
  [buf]
  (-> buf .getLong long->ulong))

(defn reader-mode
  [buf]
  (-> buf .getInt int->uint))

(defn reader-atime
  [buf]
  (-> buf .getInt int->uint))

(defn reader-mtime
  [buf]
  (-> buf .getInt int->uint))

(defn reader-mtime
  [buf]
  (-> buf .getInt int->uint))

(defn reader-len
  [buf]
  (-> buf .getLong long->ulong))

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

(defn reader-iounit
  [buf]
  (-> buf .getInt int->uint))

(defn reader-data
  [buf]
  (let [data-size (-> buf .getInt int->uint)]
    (byte-array (map byte (for [i (range data-size)] (.get buf))))))

(defn reader-nwqids
  [buf]
  (let [nwqid (-> buf .getShort short->ushort)]
    (if (= nwqid 0)
      []
      (loop [qids []
             count nwqid]
        (if (= count 0)
          qids
          (recur (conj qids {:qtype (reader-qtype buf) :qvers (reader-qvers buf) :qpath (reader-qpath buf)})
                 (- count 1)))))))
