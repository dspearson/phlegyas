(ns phlegyas.tags
  (:require [clojure.core.async :refer :all]))

(defn- next-available-tag
  [coll]
  (loop [tag 0]
    (cond
      (= tag 65536) 'exhausted
      (not (some #(= tag %) coll)) tag
      :else (recur (+ 1 tag)))))

(defn- remove-tag
  [coll clunk-tag]
  (remove #(= clunk-tag %) coll))

(defn- mutate-tags
  [port coll message]
  (if (= (:fn message) 'alloc)
    (let [tag (next-available-tag coll)]
      (>!! port tag)
      (if (= tag 'exhausted)
        coll
        (cons tag coll)))
    (remove-tag coll (:tag message))))

(defn- tag-allocator
  [port]
  (thread
    (loop [coll '()]
      (let [message (<!! port)]
        (if (= (:fn message) nil)
          nil
          (recur (mutate-tags port coll message)))))))

(defn tag-allocator!
  []
  (let [channel (chan)
        thread (tag-allocator channel)]
    channel))

(defn new-tag
  [port]
  (>!! port {:fn 'alloc})
  (<!! port))

(defn clunk-tag
  [port tag]
  (>!! port {:fn 'dealloc :tag tag}))

(defn destroy-tag-allocator
  [port]
  (>!! port {}))
