(ns phlegyas.frames-test
  (:use [clojure test])
  (:require [clojure.java.io :as io]
            [phlegyas.util :refer :all]
            [phlegyas.types :refer :all]
            [phlegyas.frames :refer :all]))

(defn read-vector
  [^java.io.File v]
  (with-open [^java.io.BufferedInputStream in (io/input-stream v)]
    (let [buf (byte-array (.length v))]
      (.read in buf)
      buf)))

(def vectors (vec (map (fn [^java.io.File x] [(.getName x) (read-vector x)]) (-> "vectors" io/resource .getPath io/file file-seq rest))))

(defn add-test
  [name ns test-fn & [metadata]]
  (intern ns (with-meta (symbol name) (merge metadata {:test #(test-fn)})) (fn [])))

(dotimes [i (count vectors)]
  (let [v (get vectors i)]
    (add-test (first v) 'phlegyas.frames-test
              #(is (= (vec (second v)) (-> v second disassemble-packet assemble-packet vec))))))

(defn test-ns-hook
  "Run tests in a sorted order."
  []
  (test-vars (->> (ns-interns 'phlegyas.frames-test) vals (sort-by str))))
