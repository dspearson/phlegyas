(ns phlegyas.transformers
  (:require [phlegyas.util :refer :all]
            [phlegyas.types :refer :all]
            [taoensso.timbre :as log]
            [primitive-math :as math
             :refer [int->uint short->ushort
                     uint->int ushort->short
                     ubyte->byte byte->ubyte
                     ulong->long long->ulong]]))

(defn transform-string
  [msg]
  (log/debug "In transform-string, string:" msg)
  (let [string-bytes (.getBytes msg "UTF-8")
        msg-size (count string-bytes)
        size-buf (byte-array 2)
        x (wrap-buf size-buf)]
    (doto x
      (.putShort msg-size))
    [size-buf string-bytes]))

(defn transform-wname
  [msg-array]
  (for [msg msg-array]
    (transform-string msg)))

(defn transform-nwqids
  [msg]
  (if (empty? msg)
    (byte-array 0)
    (for [elem msg]
      (let [buf (byte-array 13)
            x (wrap-buf buf)]
        (doto x
          (.put (:qtype elem))
          (.putInt (:qvers elem))
          (.putLong (:qpath elem)))
        buf))))

(defn transform-raw-data
  [data]
  (let [size-buf (byte-array 4)
        y (wrap-buf size-buf)]
    (doto y
      (.putInt (count data)))
    [size-buf data]))

(defn transform-directory
  [data]
  (log/debug "In transform-directory")
  (log/debug data)
  (let [total-size (apply + (map (fn [x] (:size x)) data))
        layout '(:ssize :type :dev :qtype :qvers :qpath :mode :atime :mtime :len :name :uid :gid :muid)
        size-buf (byte-array 4)
        y (wrap-buf size-buf)]
    (doto y
      (.putInt total-size))
    (let [output
          [size-buf
           (for [entry data]
             (for [x layout]
               (if (or (= x :name) (= x :uid) (= x :gid) (= x :muid))
                 (transform-string (x entry))
                 (let [buf (byte-array (x type-size))]
                   ((x type-bufop) (wrap-buf buf) (x entry))
                   buf))))]]
      output)))

(defn transform-data
  [msg]
  (log/debug "In transform-data")
  (let [typ (:type msg)
        data (:data msg)]
    (case typ
      :directory (flatten (transform-directory data))
      :raw (flatten (transform-raw-data data))
      :error (let [size-buf (byte-array 4)
                   y (wrap-buf size-buf)]
               (doto y
                 (.putInt 0))
               size-buf))))

(def transformer {:version #'transform-string
                  :name #'transform-string
                  :muid #'transform-string
                  :uid #'transform-string
                  :gid #'transform-string
                  :ename #'transform-string
                  :wname #'transform-wname
                  :nwqids #'transform-nwqids
                  :data #'transform-data})
