(ns phlegyas.frames-test
  (:use [clojure test])
  (:require [clojure.java.io :as io]
            [phlegyas.util :refer :all]
            [phlegyas.types :refer :all]
            [phlegyas.frames :refer :all]))

(def Tversion-packet [19 0 0 0 100 0 0 24 32 0 0 6 0 57 80 50 48 48 48])
(def Tauth-packet [25 0 0 0 102 0 0 0 0 0 0 3 0 100 115 112 7 0 102 97 107 101 45 102 115])
(def Tflush-packet [9 0 0 0 108 1 0 0 0])
(def Tattach-packet [22 0 0 0 104 0 0 1 0 0 0 2 0 0 0 3 0 100 115 112 0 0])
(def Twalk-packet [29 0 0 0 110 4 0 6 0 0 0 8 0 0 0 3 0 1 0 97 2 0 98 99 3 0 100 101 102])
(def Topen-packet [12 0 0 0 112 5 0 7 0 0 0 0])

(def Tversion-frame {:frame :Tversion :tag notag :msize 8216 :version "9P2000"})
(def Tauth-frame {:frame :Tauth :tag 0 :afid 0 :uname "dsp" :aname "fake-fs"})
(def Tflush-frame {:frame :Tflush :tag 1 :oldtag 0})
(def Tattach-frame {:frame :Tattach :tag 0 :fid 1 :afid nofid :uname "dsp" :aname ""})
(def Twalk-frame {:frame :Twalk :tag 4 :fid 6 :newfid 8 :wname ["a" "bc" "def"]})
(def Topen-frame {:frame :Topen :tag 5 :fid 7 :iomode (byte 0)})

;; encoders
(deftest test-Tversion-packet-encode
  (is (= Tversion-packet (vec (assemble-packet Tversion-frame)))))

(deftest test-Tauth-packet-encode
  (is (= Tauth-packet (vec (assemble-packet Tauth-frame)))))

(deftest test-Tflush-packet-encode
  (is (= Tflush-packet (vec (assemble-packet Tflush-frame)))))

(deftest test-Tattach-packet-encode
  (is (= Tattach-packet (vec (assemble-packet Tattach-frame)))))

(deftest test-Twalk-packet-encode
  (is (= Twalk-packet (vec (assemble-packet Twalk-frame)))))

(deftest test-Topen-packet-encode
  (is (= Topen-packet (vec (assemble-packet Topen-frame)))))

;; decoders
(deftest test-Tversion-packet-decode
  (is (= Tversion-frame (disassemble-packet (wrap-buf (byte-array Tversion-packet))))))

(deftest test-Tauth-packet-decode
  (is (= Tauth-frame (disassemble-packet (wrap-buf (byte-array Tauth-packet))))))

(deftest test-Tflush-packet-decode
  (is (= Tflush-frame (disassemble-packet (wrap-buf (byte-array Tflush-packet))))))

(deftest test-Tattach-packet-decode
  (is (= Tattach-frame (disassemble-packet (wrap-buf (byte-array Tattach-packet))))))

(deftest test-Twalk-packet-decode
  (is (= Twalk-frame (disassemble-packet (wrap-buf (byte-array Twalk-packet))))))

(deftest test-Topen-packet-decode
  (is (= Topen-frame (disassemble-packet (wrap-buf (byte-array Topen-packet))))))
