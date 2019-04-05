(ns phlegyas.transformer
  (:require [phlegyas.util :refer :all]
            [phlegyas.types :refer :all]
            [taoensso.timbre :as log]
            [primitive-math :as math
             :refer [int->uint short->ushort
                     uint->int ushort->short
                     ubyte->byte byte->ubyte
                     ulong->long long->ulong]]))

(declare transformer) ;; required for forward declarations

(defn transform
  "Takes in data and layout and assembles it into a byte-array.
  If the data requires a transformer, it looks this up in the transformer table
  as defined in the transformer namespace, and executes the function on the data.
  Otherwise, it lokos up the buffer operator in the type-bufop table as defined in
  the types namespace, and executes this operation on the data to write to the given
  byte-array by wrapping it first in a ByteBuffer."
  [data layout]
  (for [k layout]
    (if (some? (k transformer))
      ((k transformer) (k data))
      (let [buf (byte-array (k type-size))]
        ((k type-bufop) (wrap-buf buf) (k data))
        buf))))

(defn transform-string
  [msg]
  (let [string-bytes (.getBytes msg "UTF-8")
        msg-size (count string-bytes)
        size-buf (byte-array 2)
        x (wrap-buf size-buf)]
    (doto x
      (.putShort msg-size))
    [size-buf string-bytes]))

(defn transform-wname
  [msg-array]
  (let [size-buf (byte-array 2)
        x (wrap-buf size-buf)
        size (count msg-array)]
    (.putShort x size)
    [size-buf (for [elem msg-array]
                (transform-string elem))]))

(defn transform-nwqids
  [msg]
  (let [size-buf (byte-array 2)
        y (wrap-buf size-buf)]
    (doto y
      (.putShort (count msg)))
    (if (empty? msg)
      size-buf
      [size-buf (for [elem msg]
           (let [buf (byte-array 13)
                 x (wrap-buf buf)]
             (doto x
               (.put (:qtype elem))
               (.putInt (:qvers elem))
               (.putLong (:qpath elem)))
             buf))])))

(defn transform-raw-data
  [data]
  (let [size-buf (byte-array 4)
        y (wrap-buf size-buf)]
    (doto y
      (.putInt (count data)))
    [size-buf data]))

(defn transform-directory
  [data]
  (let [total-size (apply + (map (fn [x] (:ssize x)) data))
        layout (subvec (:Rstat frame-layouts) 2)
        size-buf (byte-array 4)
        y (wrap-buf size-buf)]
    (doto y
      (.putInt total-size))
    [size-buf (for [entry data] (transform entry layout))]))

(defn transform-data
  [msg]
  (let [typ (:type msg)
        data (:data msg)]
    (if (= typ :directory)
      (flatten (transform-directory data)) ;instrumented directory encoder
      (flatten (transform-raw-data msg)))))

(defn transform-wdata
  [msg]
  (let [data-size (count msg)
        size-buf (byte-array 4)
        y (wrap-buf size-buf)]
    (doto y
      (.putInt data-size)
      size-buf)))

(def transformer {:version #'transform-string
                  :name    #'transform-string
                  :uname   #'transform-string
                  :aname   #'transform-string
                  :muid    #'transform-string
                  :uid     #'transform-string
                  :gid     #'transform-string
                  :ename   #'transform-string
                  :wname   #'transform-wname
                  :nwqids  #'transform-nwqids
                  :data    #'transform-data
                  :wdata   #'transform-wdata})
