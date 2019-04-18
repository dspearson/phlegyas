(ns phlegyas.client
  (:require [clojure.set :as sets]
            [manifold.stream :as s]
            [phlegyas.util :refer :all]
            [phlegyas.types :refer :all]
            [phlegyas.frames :refer :all]))

(defn next-available-value
  "Function for use with atomic swap, to find next value not
  belonging to the set."
  [p]
  (loop [i 0]
    (if (not (contains? p i))
      i
      (recur (inc i)))))

(defn numeric-pool
  "Returns a function that acts as an allocator/deallocator.
  Call with no arguments to return the next available number, and
  call with `:clunk n` or `:clunk #{n m n}`, to remove values from
  use."
  []
  (let [vals (atom #{})]
    (fn
      [& {:keys [clunk] :or {clunk nil}}]
      (if clunk
        (swap! vals (fn [x] (disj x clunk)))
        (let [[old new] (swap-vals! vals (fn [x] (conj x (next-available-value x))))]
          (first (sets/difference new old)))))))

(defn client!
  [in out]
  (let [state (atom {:fidpool (numeric-pool)
                     :tagpool (numeric-pool)})
        incoming-frame-stream (s/stream)
        outgoing-frame-stream (s/stream)
        uuid (keyword (uuid!))
        _ (frame-assembler in incoming-frame-stream)]
    (s/connect-via outgoing-frame-stream #(s/put! out (assemble-packet %)) out)))

(defn handshake
  [tag requested-message-size]
  (assemble-packet {:frame :Tversion :tag tag :msize requested-message-size :version protocol-version}))
