(ns phlegyas.transformers
  (:require [phlegyas.util :refer :all]
            [phlegyas.types :refer :all]))

(declare transformer) ;; required for forward declarations

(defn transform
  "Takes in data and layout and assembles it into a byte-array.
  If the data requires a transformer, it looks this up in the transformer map
  and executes the function on the data.

  Otherwise, it looks up the buffer operator in the `buffer-operator` map, as
  defined in the `phlegyas.types` namespace, and executes this operation on
  the data to write to the given byte-array by wrapping it first in a ByteBuffer."
  [frame layout]
  (for [typ layout]
    (if (some? (get transformer typ))
      ((get transformer typ) (get frame typ))
      (let [buffer (byte-array (typ type-size))]
        ((get buffer-operator typ) (wrap-buffer buffer) (get frame typ))
        buffer))))

(defn transform-string
  "Encodes a string. Strings in 9P2000 are UTF-8, with a short field of
  their length prefixing them."
  [s]
  (let [string-bytes (.getBytes s "UTF-8")
        string-size (count string-bytes)
        size-bytes (byte-array 2)
        buffer (wrap-buffer size-bytes)]
    (doto buffer
      (.putShort string-size))
    [size-bytes string-bytes]))

(defn transform-wnames
  "Takes in a vector of wnames, for the `:Twalk` message, and encodes
  them, prefixing with a short count field as required."
  [coll]
  (let [size-array (byte-array 2)
        buffer (wrap-buffer size-array)
        size (count coll)]
    (.putShort buffer size)
    [size-array (for [elem coll] (transform-string elem))]))

(defn transform-nwqids
  "Takes in a vector of QID structures, prefixes them with a short
  count field, and encodes."
  [coll]
  (let [count-bytes (byte-array 2)
        buffer (wrap-buffer count-bytes)]
    (doto buffer
      (.putShort (count coll)))
    (if (empty? coll)
      count-bytes
      [count-bytes (for [elem coll]
                      (let [qid-bytes (byte-array 13)
                            buffer (wrap-buffer qid-bytes)]
                        (doto buffer
                          (.put (:qid-type elem))
                          (.putInt (:qid-vers elem))
                          (.putLong (:qid-path elem)))
                        qid-bytes))])))

(defn transform-directory
  "Instrumented directory encoder. Takes in a vector of stat structures,
  and produces a byte collection suitable for use in `Rread`."
  [coll]
  (let [calculated-size (apply + (map (fn [x] (:ssize x)) coll))
        layout (subvec (:Rstat frame-layouts) 2)
        size-bytes (byte-array 4)
        buffer (wrap-buffer size-bytes)]
    (doto buffer
      (.putInt calculated-size))
    [size-bytes (for [elem coll] (transform elem layout))]))

(defn transform-raw-data
  "Takes in a byte-array, and pads it with the size, as required by the
  `Rread` and `Twrite` messages."
  [data-bytes]
  (let [size-bytes (byte-array 4)
        buffer (wrap-buffer size-bytes)]
    (doto buffer
      (.putInt (count data-bytes)))
    [size-bytes data-bytes]))

(defn transform-data
  "This is a special function. If you are encoding a Rread of a directory,
  you can pass `:data {:type :directory :data [stat1 ... statn]}}` into
  `construct-packet`, and it will also automatically encode your directory
  stat structures properly. Otherwise, just pass a raw byte-array to `:data`
  for it to get packed regularly."
  [coll]
  (let [type' (:type coll)  ;; these fields are for the instrumented
        data (:data coll)]  ;; directory encoder.
    (if (= type' :directory)
      (flatten (transform-directory data))
      (flatten (transform-raw-data coll)))))

(def transformer {:version #'transform-string
                  :name    #'transform-string
                  :uname   #'transform-string
                  :aname   #'transform-string
                  :muid    #'transform-string
                  :uid     #'transform-string
                  :gid     #'transform-string
                  :ename   #'transform-string
                  :nwqids  #'transform-nwqids
                  :wnames  #'transform-wnames
                  :data    #'transform-data})
