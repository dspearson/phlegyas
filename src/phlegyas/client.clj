(ns phlegyas.client
  (:require [clojure.set :as sets]
            [manifold.stream :as s]
            [taoensso.timbre :as log]
            [manifold.deferred :as d]
            [aleph.tcp :as tcp]
            [phlegyas.util :refer :all]
            [phlegyas.types :refer :all]
            [phlegyas.frames :refer :all]))

;; an example implementation of a client.

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
  "Returns a function that takes in frames and returns deferreds
  representing the response over the network. Frames do not
  require tags."
  [x]
  (fn [y]
    (let [response (d/deferred)]
      (s/put! x {:response response :frame y})
      response)))

(defn client!
  "This example client returns a function that will take in frames (without tags)
  and will automatically handle matching responses for you. Calls to the returned
  function are a deferred that is delivered when the response is received."
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

(defn connect
  [host port]
  {:connection (client! @(tcp/client {:host host :port port}))
   :state (atom {})})

(defn attach-filesystem
  "Takes a connection and a username, and optionally a filesystem name."
  ([x username]
   (attach-filesystem x username ""))

  ([x username fs-name]
   (let [state (:state x)
         fid-pool (:fid-pool @state)
         attach-fid (fid-pool)
         connection (:connection x)
         response @(connection {:frame :Tattach :uname username :aname fs-name :fid attach-fid :afid nofid})]
     (if (= (:frame response) :Rattach)
       (do
         (swap! state (fn [x] (assoc x :fids (assoc (:fids x) attach-fid {:name "/" :filesystem fs-name}))))
         attach-fid)
       (do
         (fid-pool :clunk attach-fid)
         false)))))

(defn negotiate-version
  [x]
  (let [connection (:connection x)
        state (:state x)
        response @(connection {:frame :Tversion :msize max-message-size :version protocol-version})]
    (reset! state {}) ;; a version request restarts a connection.
    (if (= (:frame response) :Rversion)
      (do
        (swap! state assoc :msize (:msize response) :version (:version response) :fid-pool (numeric-pool))
        (:version response))
      false)))

(defn clone-fid
  [x fs-handle]
  (let [connection (:connection x)
        state (:state x)
        fid-pool (:fid-pool @state)
        requested-fid (fid-pool)
        response @(connection {:frame :Twalk :fid fs-handle :newfid requested-fid :wnames []})]
    (if (= (:frame response) :Rwalk)
      (do
        (swap! state (fn [x] (assoc x :fids (assoc (:fids x) requested-fid (get (:fids x) fs-handle)))))
        requested-fid)
      (do
        (fid-pool :clunk requested-fid)
        false))))

(defn walk-fid
  [x fs-handle paths]
  (let [connection (:connection x)
        state (:state x)
        fid-pool (:fid-pool @state)
        requested-fid (fid-pool)
        response @(connection {:frame :Twalk :fid fs-handle :newfid requested-fid :wnames paths})]
    (if (and (= (:frame response) :Rwalk) (= (count (:nwqids response)) (count paths)))
      (do
        (swap! state (fn [y] (assoc y :fids (assoc (:fids y) requested-fid
                                                  {:name (let [parent (:name (get (:fids @state) fs-handle))]
                                                               (str (if (= parent "/") "" parent)
                                                                    "/"
                                                                    (clojure.string/join "/" paths)))
                                                   :filesystem (:filesystem (get (:fids @state) fs-handle))}))))
        requested-fid)
      (do
        (fid-pool :clunk requested-fid)
        false))))

(defn open-fid
  [x fid iomode]
  (let [connection (:connection x)
        response @(connection {:frame :Topen :fid fid :iomode iomode})]
    (:iounit response)))

(defn read-fid-partial
  [x fid offset iounit]
  (let [connection (:connection x)
        response @(connection {:frame :Tread :fid fid :offset offset :count iounit})]
    (:data response)))

(defn clunk-fid
  [x fid]
  (let [connection (:connection x)
        state (:state x)
        response @(connection {:frame :Tclunk :fid fid})]
    (if (= (:frame response) :Rclunk)
      (do
        (swap! state (fn [y] (assoc y :fids (dissoc (:fids y) fid))))
        true)
      false)))

(defn read-fid
  [x fid iounit]
  (loop [offset 0
         buf []]
    (let [data (read-fid-partial x fid offset iounit)]
      (if (empty? data)
        (-> buf flatten pack)
        (recur (+ offset (count data)) (conj buf data))))))

(defn read-dir
  [x fid]
  (let [fid-clone (clone-fid x fid)
        iounit (open-fid x fid-clone 0)
        data (read-fid x fid-clone iounit)
        _ (clunk-fid x fid-clone)
        layout (subvec (:Rstat frame-layouts) 2)
        buf (wrap-buffer data)]
    (loop [stats {}]
      (if (= (.remaining buf) 0)
        stats
        (let [y (into {} (for [elem layout] {elem ((elem buffer-functions) buf)}))]
          (recur (assoc stats (:qid-path y) y)))))))

(defn open-and-read
  [x fid]
  (let [fid-clone (clone-fid x fid)
        io-unit (open-fid x fid-clone 0)
        data (read-fid x fid-clone io-unit)
        _ (clunk-fid x fid-clone)]
    data))

(defn lsdir
  [x fid]
  (let [data (read-dir x fid)]
    (for [k (keys data)]
      (:name (get data k)))))
