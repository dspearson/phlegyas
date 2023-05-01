(ns phlegyas.frames-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is test-vars]]
            [phlegyas.frames :refer [assemble-packet disassemble-packet]]))

(defn test-ns-hook
  "Run tests in a sorted order."
  []
  (test-vars (->> (ns-interns 'phlegyas.frames-test) vals (sort-by str))))

(defn read-vector
  [^java.io.File v]
  (with-open [^java.io.BufferedInputStream in (io/input-stream v)]
    (let [buf (byte-array (.length v))]
      (.read in buf)
      buf)))

(defn add-test
  [name ns test-fn & [metadata]]
  (intern ns (with-meta (symbol name) (merge metadata {:test #(test-fn)})) (fn [])))

(def vectors (vec (map (fn [^java.io.File x] [(.getName x) (read-vector x)]) (-> "vectors" io/resource io/file file-seq rest))))

(dotimes [i (count vectors)]
  (let [v (get vectors i)]
    (add-test (first v) 'phlegyas.frames-test
              #(is (= (vec (second v)) (-> v second disassemble-packet assemble-packet vec))))))
