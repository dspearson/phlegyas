(ns phlegyas.client
  (:require [clojure.set :as sets]
            [manifold.stream :as s]
            [taoensso.timbre :as log]
            [manifold.deferred :as d]
            [aleph.tcp :as tcp]
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

(defn tag-and-assemble
  [x in-flight tagpool]
  (let [frame (:frame x)
        response (:response x)
        tag (tagpool)]
    (swap! in-flight (fn [x] (assoc x (keywordize tag) response)))
    (-> frame (assoc :tag tag) assemble-packet)))

(defn transactional-actor
  [x]
  (fn [y]
    (let [response (d/deferred)]
      (s/put! x {:response response :frame y})
      response)))

(defn client!
  [in]
  (let [tagpool (numeric-pool)
        incoming-frame-stream (s/stream)
        outgoing-frame-stream (s/stream)
        in-flight (atom {})
        _ (frame-assembler in incoming-frame-stream)]
    (s/consume (fn [x] (let [tag (:tag x)
                            response ((keywordize tag) @in-flight)]
                        (swap! in-flight dissoc tag)
                        (tagpool :clunk tag)
                        (d/success! response x))) incoming-frame-stream)
    (s/connect-via outgoing-frame-stream #(s/put! in (tag-and-assemble % in-flight tagpool)) in)
    (transactional-actor outgoing-frame-stream)))

(defn dial
  [host port]
  (tcp/client {:host host :port port}))

(defn connect
  [host port]
  (let [client @(dial host port)]
    (client! client)))
