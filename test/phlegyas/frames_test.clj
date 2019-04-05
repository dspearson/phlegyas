(ns phlegyas.frames-test
  (:use [clojure test])
  (:require [clojure.java.io :as io]
            [phlegyas.util :refer :all]
            [phlegyas.types :refer :all]
            [phlegyas.frames :refer :all]))

(defn test-ns-hook
  "Run tests in a sorted order."
  []
  (test-vars (->> (ns-interns 'phlegyas.frames-test) vals (sort-by str))))

(defn add-test
  "Add tests programmatically to the namespace."
  [name test-fn]
  (intern (ns-name *ns*) (with-meta (symbol name) {:test #(test-fn)}) (fn [])))

(defn read-vector
  "Read vector from file and produce a byte-array."
  [coll]
  (with-open [in (io/input-stream coll)]
    (let [buf (byte-array (.length coll))]
      (.read in buf)
      buf)))

;; fetch vectors
(def vectors (vec (map (fn [vector] [(.getName vector) (read-vector vector)]) (-> "vectors" io/resource .getPath io/file file-seq rest))))

;; register tests.
(dotimes [i (count vectors)]
  (let [[test-name vector] (get vectors i)]
    (add-test test-name #(is (= (vec vector) (-> vector disassemble-packet assemble-packet vec))))))
