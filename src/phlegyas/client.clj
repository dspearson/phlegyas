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

(defn next-available
  [p]
  (loop [i 0]
    (if (not (contains? p i))
      i
      (recur (inc i)))))

(defn alloc-val
  [pool]
  (let [[old new] (swap-vals! pool (fn [x] (conj x (next-available x))))]
    (first (sets/difference new old))))

(defn clunk-val
  [pool value]
  (swap! pool (fn [x] (disj x value))))

(defn tag-and-assemble
  [x in-flight tagpool]
  (let [frame (:frame x)
        response (:response x)
        tag (alloc-val tagpool)]
    (swap! in-flight (fn [x] (assoc x (keywordize tag) response)))
    (-> frame (assoc :tag tag) assemble-packet)))

(defn transact
  "Returns a function that takes in frames and returns deferreds
  representing the response over the network. Frames do not
  require tags."
  [connection transaction]
  (let [response (d/deferred)]
    (s/put! (:outgoing-frame-stream connection) {:response response :frame transaction})
    response))

(defn client!
  "This example client returns a function that will take in frames (without tags)
  and will automatically handle matching responses for you. Calls to the returned
  function are a deferred that is delivered when the response is received."
  [in]
  (let [tagpool (atom #{})
        incoming-frame-stream (s/stream)
        outgoing-frame-stream (s/stream)
        in-flight (atom {})
        frame-assembler-thread (frame-assembler in incoming-frame-stream)]
    (s/consume (fn [x] (let [tag (:tag x)
                            response ((keywordize tag) @in-flight)]
                        (swap! in-flight dissoc tag)
                        (clunk-val tagpool tag)
                        (d/success! response x))) incoming-frame-stream)
    (s/connect-via outgoing-frame-stream #(s/put! in (tag-and-assemble % in-flight tagpool)) in)
    {:tag-pool tagpool
     :incoming-frame-stream incoming-frame-stream
     :outgoing-frame-stream outgoing-frame-stream
     :in-flight in-flight
     :frame-assembler frame-assembler-thread}))

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
         attach-fid (alloc-val fid-pool)
         connection (:connection x)
         response @(transact connection {:frame :Tattach :uname username :aname fs-name :fid attach-fid :afid nofid})]
     (if (= (:frame response) :Rattach)
       (do
         (swap! state (fn [x] (assoc x :fids (assoc (:fids x) attach-fid {:name "/" :filesystem fs-name}))))
         attach-fid)
       (do
         (clunk-val fid-pool attach-fid)
         false)))))

(defn negotiate-version
  [x]
  (let [connection (:connection x)
        state (:state x)
        response @(transact connection {:frame :Tversion :msize max-message-size :version protocol-version})]
    (reset! state {}) ;; a version request restarts a connection.
    (if (= (:frame response) :Rversion)
      (do
        (swap! state assoc :msize (:msize response) :version (:version response) :fid-pool (atom #{}))
        (:version response))
      false)))

(defn clone-fid
  [x fs-handle]
  (let [connection (:connection x)
        state (:state x)
        fid-pool (:fid-pool @state)
        requested-fid (alloc-val fid-pool)
        response @(transact connection {:frame :Twalk :fid fs-handle :newfid requested-fid :wnames []})]
    (if (= (:frame response) :Rwalk)
      (do
        (swap! state (fn [x] (assoc x :fids (assoc (:fids x) requested-fid (get (:fids x) fs-handle)))))
        requested-fid)
      (do
        (clunk-val fid-pool requested-fid)
        false))))

(defn walk-fid
  [x fs-handle paths]
  (let [connection (:connection x)
        state (:state x)
        fid-pool (:fid-pool @state)
        requested-fid (alloc-val fid-pool)
        response @(transact connection {:frame :Twalk :fid fs-handle :newfid requested-fid :wnames paths})]
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
        (clunk-val fid-pool requested-fid)
        false))))

(defn open-fid
  [x fid iomode]
  (let [connection (:connection x)
        response @(transact connection {:frame :Topen :fid fid :iomode iomode})]
    (:iounit response)))

(defn read-fid-partial
  [x fid offset iounit]
  (let [connection (:connection x)
        response @(transact connection {:frame :Tread :fid fid :offset offset :count iounit})]
    (:data response)))

(defn clunk-fid
  [x fid]
  (let [connection (:connection x)
        state (:state x)
        response @(transact connection {:frame :Tclunk :fid fid})]
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

(defn open-and-read-fid
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
