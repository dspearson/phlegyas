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

(defn next-val
  "Find the smallest integer not currently in the atomic set, add it to the
  atomic set, and return the added value."
  [a]
  (let [next-available (fn [p]
                         (loop [i 0]
                           (if (not (contains? p i))
                             i
                             (recur (inc i)))))
        [old new] (swap-vals! a (fn [x] (conj x (next-available x))))]
    (first (sets/difference new old))))

(defn disj-val
  "Remove a value from the atomic set."
  [a val]
  (swap! a (fn [x] (disj x val))))

(defn assoc-val
  "Associate a key with the value in the atomic map."
  [a key val]
  (swap! a assoc key val))

(defn dissoc-val
  "Remove a key from the atomic map."
  [a key]
  (swap! a (fn [x] (dissoc x key))))

(defn add-child-mapping
  "Adds a new fid that results from a successful walk to the atomic map."
  [mapping fid newfid paths]
  (swap! mapping (fn [y] (let [parent (get y fid)
                              parent-name (:name parent)
                              parent-uname (:uname parent)
                              path-prefix (if (= parent "/") "" parent)]
                          (assoc y newfid
                                 {:name (str parent "/" (clojure.string/join "/" paths))
                                  :uname parent-uname})))))

(defn tag-and-assemble
  [x in-flight-requests tagpool]
  (let [frame (:frame x)
        response (:response x)
        tag (next-val tagpool)]
    (assoc-val in-flight-requests (keywordize tag) response)
    (-> frame (assoc :tag tag) assemble-packet)))

(defn transact
  "Returns a function that takes in frames and returns deferreds
  representing the response over the network. Frames do not
  require tags."
  [connection transaction]
  (let [response (d/deferred)]
    (s/put! (:outgoing-frame-stream connection) {:response response :frame transaction})
    response))

(defn reset-connection
  [connection]
  (reset! (:mapping connection) {})
  (reset! (:open-fids connection) {})
  (reset! (:fid-pool connection) #{})
  (reset! (:tag-pool connection) #{})
  (reset! (:in-flight-requests connection) {})
  (reset! (:maximum-message-size connection) nil)
  (reset! (:protocol-version connection) nil))

(defn negotiate-version
  [connection]
  (reset-connection connection) ;; a version request restarts a connection.
  (let [response @(transact connection {:frame :Tversion :msize max-message-size :version protocol-version})]
    (if (= (:frame response) :Rversion)
      (do
        (reset! (:maximum-message-size connection) (:msize response))
        (reset! (:protocol-version connection) (:version response))
        (:version response))
      false)))

(defn attach-filesystem
  ([connection]
   (attach-filesystem connection "nobody" ""))

  ([connection uname]
   (attach-filesystem connection uname ""))

  ([connection uname aname]
   (let [fid-pool (:fid-pool connection)
         attach-fid (next-val fid-pool)
         response @(transact connection {:frame :Tattach :uname uname :aname aname :fid attach-fid :afid nofid})]
     (if (= (:frame response) :Rattach)
       (do
         (assoc-val (:mapping connection) attach-fid {:name "/" :uname uname})
         attach-fid)
       (do
         (disj-val fid-pool attach-fid)
         false)))))

(defn clone-fid
  [connection fs-handle]
  (let [fid-pool (:fid-pool connection)
        requested-fid (next-val fid-pool)
        response @(transact connection {:frame :Twalk :fid fs-handle :newfid requested-fid :wnames []})]
    (if (= (:frame response) :Rwalk)
      (do
        (assoc-val (:mapping connection) requested-fid (get (:mapping connection) fs-handle))
        requested-fid)
      (do
        (disj-val fid-pool requested-fid)
        false))))

(defn walk-fid
  [connection fs-handle paths]
  (let [fid-pool (:fid-pool connection)
        requested-fid (next-val fid-pool)
        response @(transact connection {:frame :Twalk :fid fs-handle :newfid requested-fid :wnames paths})]
    (if (and (= (:frame response) :Rwalk) (= (count (:nwqids response)) (count paths)))
      (do
        (add-child-mapping (:mapping connection) fs-handle requested-fid paths)
        requested-fid)
      (do
        (disj-val fid-pool requested-fid)
        false))))

(defn open-fid
  [connection fid iomode]
  (let [response @(transact connection {:frame :Topen :fid fid :iomode iomode})]
    (assoc-val (:open-fids connection) fid {:iomode iomode :iounit (:iounit response)})
    (:iounit response)))

(defn read-fid-partial
  [connection fid offset iounit]
  (let [response @(transact connection {:frame :Tread :fid fid :offset offset :count iounit})]
    (:data response)))

(defn clunk-fid
  [connection fid]
  (let [response @(transact connection {:frame :Tclunk :fid fid})]
    (if (= (:frame response) :Rclunk)
      (do
        (disj-val (:fid-pool connection) fid)
        (dissoc-val (:open-fids connection) fid)
        (dissoc-val (:mapping connection) fid)
        true)
      false)))

(defn read-fid
  [connection fid iounit]
  (loop [offset 0
         buf []]
    (let [data (read-fid-partial connection fid offset iounit)]
      (if (empty? data)
        (-> buf flatten pack)
        (recur (+ offset (count data)) (conj buf data))))))

(defn read-dir
  [connection fid]
  (let [fid-clone (clone-fid connection fid)
        iounit (open-fid connection fid-clone 0)
        data (read-fid connection fid-clone iounit)
        _ (clunk-fid connection fid-clone)
        layout (subvec (:Rstat frame-layouts) 2)
        buf (wrap-buffer data)]
    (loop [stats {}]
      (if (= (.remaining buf) 0)
        stats
        (let [y (into {} (for [elem layout] {elem ((elem buffer-functions) buf)}))]
          (recur (assoc stats (:qid-path y) y)))))))

(defn open-and-read-fid
  [connection fid]
  (let [fid-clone (clone-fid connection fid)
        io-unit (open-fid connection fid-clone 0)
        data (read-fid connection fid-clone io-unit)
        _ (clunk-fid connection fid-clone)]
    data))

(defn lsdir
  [connection fid]
  (let [data (read-dir connection fid)]
    (for [k (keys data)]
      (:name (get data k)))))

(defn client!
  "This example client returns a function that will take in frames (without tags)
  and will automatically handle matching responses for you. Calls to the returned
  function are a deferred that is delivered when the response is received."
  [in]
  (let [tagpool (atom #{})
        fidpool (atom #{})
        incoming-frame-stream (s/stream)
        outgoing-frame-stream (s/stream)
        in-flight-requests (atom {})
        frame-assembler-thread (frame-assembler in incoming-frame-stream)]
    (s/consume (fn [x] (let [tag (:tag x)
                            response ((keywordize tag) @in-flight-requests)]
                        (dissoc-val in-flight-requests (keywordize tag))
                        (disj-val tagpool tag)
                        (d/success! response x))) incoming-frame-stream)
    (s/connect-via outgoing-frame-stream #(s/put! in (tag-and-assemble % in-flight-requests tagpool)) in)
    {:tag-pool tagpool
     :fid-pool fidpool
     :open-fids (atom {})
     :mapping (atom #{})
     :protocol-version (atom nil)
     :maximum-message-size (atom nil)
     :incoming-frame-stream incoming-frame-stream
     :outgoing-frame-stream outgoing-frame-stream
     :in-flight-requests in-flight-requests
     :frame-assembler frame-assembler-thread}))

(defn connect
  ([host port]
   (connect host port "nobody" ""))

  ([host port uname]
   (connect host port uname ""))

  ([host port uname aname]
   (let [connection (client! @(tcp/client {:host host :port port}))
         _ (negotiate-version connection)
         _ (attach-filesystem connection uname aname)]
     connection)))
