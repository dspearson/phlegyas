(ns phlegyas.buffers
  (:require [primitive-math :as math
             :refer [int->uint
                     short->ushort
                     long->ulong]]))

(defn get-tag
  "Read tag[2] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Short .getShort short->ushort))

(defn get-oldtag
  "Read oldtag[2] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Short .getShort short->ushort))

(defn get-msize
  "Read msize[4] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Integer .getInt int->uint))

(defn get-string
  "Read string[s] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (let [string-size (-> buffer ^Short .getShort short->ushort)]
    (String. (byte-array (map byte (for [i (range string-size)] (^Byte .get buffer)))) "UTF-8")))

(defn get-version
  "Read version[s] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (get-string buffer))

(defn get-uname
  "Read uname[s] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (get-string buffer))

(defn get-aname
  "Read aname[s] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (get-string buffer))

(defn get-ename
  "Read ename[s] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (get-string buffer))

(defn get-fid
  "Read fid[4] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Integer .getInt int->uint))

(defn get-newfid
  "Read newfid[4] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Integer .getInt int->uint))

(defn get-afid
  "Read afid[4] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Integer .getInt int->uint))

(defn get-perm
  "Read perm[4] from the byte buffer"
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Integer .getInt int->uint))

(defn get-offset
  "Read offset[8] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Long .getLong long->ulong))

(defn get-count
  "Read count[4] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Integer .getInt int->uint))

(defn get-size
  "Read size[2] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Short .getShort short->ushort))

(defn get-ssize
  "Read size[2] from the byte buffer. Rstat and Twstat have repeated
  size field, with our ssize being +2 more than size.

  See BUGS section of stat(9) manual for more information."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Short .getShort short->ushort))

(defn get-type
  "Read type[2] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Short .getShort short->ushort))

(defn get-dev
  "Read dev[4] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Integer .getInt int->uint))

(defn get-qid-type
  "Read qid.type[1] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Byte .get))

(defn get-qid-vers
  "Read qid.vers[4] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Integer .getInt int->uint))

(defn get-qid-path
  "Read qid.path[8] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Long .getLong long->ulong))

(defn get-mode
  "Read mode[4] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Integer .getInt int->uint))

(defn get-atime
  "Read atime[4] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Integer .getInt int->uint))

(defn get-mtime
  "Read mtime[4] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Integer .getInt int->uint))

(defn get-length
  "Read length[8] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Long .getLong long->ulong))

(defn get-name
  "Read name[s] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (get-string buffer))

(defn get-uid
  "Read uid[s] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (get-string buffer))

(defn get-gid
  "Read gid[s] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (get-string buffer))

(defn get-muid
  "Read muid[s] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (get-string buffer))

(defn get-iomode
  "Read mode[1] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Byte .get))

(defn get-iounit
  "Read iounit[4] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (-> buffer ^Integer .getInt int->uint))

(defn get-data
  "Read count[4] bytes of data from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (let [data-size (-> buffer ^Integer .getInt int->uint)]
    (byte-array (map byte (for [i (range data-size)] (^Byte .get buffer))))))

(defn get-wnames
  "Read nwname[2] of wname[s] from the byte buffer."
  [^java.nio.ByteBuffer buffer]
  (let [nwname (-> buffer ^Short .getShort short->ushort)]
    (if (= nwname 0)
      []
      (loop [wnames '()
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
          (recur (conj qids {:qid-type (get-qid-type buffer) :qid-vers (get-qid-vers buffer) :qid-path (get-qid-path buffer)})
                 (- count 1)))))))
