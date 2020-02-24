(ns phlegyas.client
  (:require [clojure.set :as sets]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [clojure.string :as cs]
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
                           (if-not (contains? p i)
                             i
                             (recur (inc i)))))
        [old new] (swap-vals! a (fn [x] (conj x (next-available x))))]
    (first (sets/difference new old))))

(defn add-child-mapping
  "Adds a new fid that results from a successful walk to the atomic map."
  [mapping fid newfid paths]
  (swap! mapping (fn [y] (let [parent (get y fid)
                               parent-name (:name parent)
                               parent-uname (:uname parent)
                               path-prefix (if (= parent "/") "" parent)]
                           (assoc y newfid
                                  {:name (str parent "/" (cs/join "/" paths))
                                   :uname parent-uname})))))

(defn tag-and-assemble
  "Add a tag to the request, and assemble it into a byte-array."
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
  "Reset the connection atom to an empty state."
  [connection]
  (reset! (:mapping connection) {})
  (reset! (:open-fids connection) {})
  (reset! (:fid-pool connection) #{})
  (reset! (:tag-pool connection) #{})
  (reset! (:in-flight-requests connection) {})
  (reset! (:maximum-message-size connection) nil)
  (reset! (:protocol-version connection) nil))

(defn negotiate-version
  "9P version negotiation. Takes a connection map and modifies it appropriately."
  [connection]
  (reset-connection connection) ;; a version request restarts a connection.
  (let [response @(transact connection {:frame :Tversion :msize max-message-size :version protocol-version})]
    (if (= (:frame response) :Rversion)
      (do
        (reset! (:maximum-message-size connection) (:msize response))
        (reset! (:protocol-version connection) (:version response))
        (:version response))
      (throw (Exception. "protocol negotiation failed.")))))

(defn attach-filesystem
  "Attach a filesystem. Requires a version to be negotiated first."
  ([connection]
   (attach-filesystem connection "nobody" ""))

  ([connection uname]
   (attach-filesystem connection uname ""))

  ([connection uname aname]
   (if (nil? @(:protocol-version connection))
     (throw (Exception. "no protocol negotiated yet."))
     (let [fid-pool (:fid-pool connection)
           attach-fid (next-val fid-pool)
           response @(transact connection {:frame :Tattach :uname uname :aname aname :fid attach-fid :afid nofid})]
       (if (= (:frame response) :Rattach)
         (do
           (assoc-val (:mapping connection) attach-fid {:name "/" :uname uname})
           attach-fid)
         (do
           (disj-val fid-pool attach-fid)
           false))))))

(defn clone-fid
  "Takes a connection and a fid, and clones it. Returns the new fid."
  [connection fs-handle]
  (let [fid-pool (:fid-pool connection)
        requested-fid (next-val fid-pool)
        response @(transact connection {:frame :Twalk :fid fs-handle :newfid requested-fid :wnames []})]
    (if (= (:frame response) :Rwalk)
      (do
        (assoc-val (:mapping connection) requested-fid (get @(:mapping connection) fs-handle))
        requested-fid)
      (do
        (disj-val fid-pool requested-fid)
        (throw (Exception. "unable to clone fid."))))))

(defn walk-fid
  "Takes a connection, fid, and a vector of paths to walk. If successful, returns the new fid."
  [connection fs-handle paths]
  (let [fid-pool (:fid-pool connection)
        requested-fid (next-val fid-pool)
        response @(transact connection {:frame :Twalk :fid fs-handle :newfid requested-fid :wnames paths})]
    (cond
      (not= (:frame response) :Rwalk)
      (throw (Exception. "unable to walk."))

      (and (= (:frame response) :Rwalk) (= (count (:nwqids response)) (count paths)))
      (do
        (add-child-mapping (:mapping connection) fs-handle requested-fid paths)
        requested-fid)

      :else
      (do
        (disj-val fid-pool requested-fid)
        false))))

(defn open-fid
  "Takes a connection, fid, and iomode. Returns the iounit for reading."
  [connection fid iomode]
  (let [response @(transact connection {:frame :Topen :fid fid :iomode iomode})]
    (if (= (:frame response) :Ropen)
      (do
        (assoc-val (:open-fids connection) fid {:iomode iomode :iounit (:iounit response)})
        (:iounit response))
      (throw (Exception. "unable to open fid.")))))

(defn read-fid-partial
  "Takes a connection, fid, offset, and iounit. Returns data."
  [connection fid offset iounit]
  (let [response @(transact connection {:frame :Tread :fid fid :offset offset :count iounit})]
    (if (= (:frame response) :Rread)
      (:data response)
      (throw (Exception. "unable to read fid.")))))

(defn clunk-fid
  "Takes a connection and fid. If successful, returns true."
  [connection fid]
  (let [response @(transact connection {:frame :Tclunk :fid fid})]
    (if (= (:frame response) :Rclunk)
      (do
        (disj-val (:fid-pool connection) fid)
        (dissoc-val (:open-fids connection) fid)
        (dissoc-val (:mapping connection) fid)
        true)
      (throw (Exception. "unable to clunk fid.")))))

(defn remove-fid
  "Takes a connection and a fid. If successful, returns true."
  [connection fid]
  (let [response @(transact connection {:frame :Tremove :fid fid})]
    (if (= (:frame response) :Rremove)
      true
      (throw (Exception. "unable to remove fid.")))))

(defn read-fid
  "Takes a connection, fid, and iounit. If successful, returns the read data."
  [connection fid iounit]
  (loop [offset 0
         buf []]
    (let [data (read-fid-partial connection fid offset iounit)]
      (if (empty? data)
        (-> buf flatten pack)
        (recur (+ offset (count data)) (conj buf data))))))

(defn read-dir-contents
  "Takes a connection, and a fid representing a directory. Returns a map of stats,
  with keys representing the qid-path of the stat."
  [connection fid]
  (let [fid-clone (clone-fid connection fid)
        iounit (open-fid connection fid-clone 0)
        data (read-fid connection fid-clone iounit)
        _ (clunk-fid connection fid-clone)
        layout (subvec (:Rstat frame-layouts) 2)
        ^java.nio.ByteBuffer buf (wrap-buffer data)]
    (loop [stats {}]
      (if (zero? (.remaining buf))
        stats
        (let [y (into {} (for [elem layout] {elem ((elem get-operation) buf)}))]
          (recur (assoc stats (:qid-path y) y)))))))

(defn open-and-read-fid
  "Takes a connection and a fid. Clones the fid, opens for reading, fetches the data,
  and clunks. Returns the data read."
  [connection fid]
  (let [fid-clone (clone-fid connection fid)
        io-unit (open-fid connection fid-clone 0)
        data (read-fid connection fid-clone io-unit)
        _ (clunk-fid connection fid-clone)]
    data))

(defn lsdir
  "Takes a connection and a fid. Returns all children within the directory
  represented by fid."
  [connection fid]
  (let [data (read-dir-contents connection fid)]
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
  "Connect to a host on a given port, optionally specifying the user and
  attaching filesystem. Negotiates the protocol version and attaches the
  filesystem. Returns a connection map."
  ([host port]
   (connect host port "nobody" ""))

  ([host port uname]
   (connect host port uname ""))

  ([host port uname aname]
   (let [connection (client! @(tcp/client {:host host :port port}))
         _ (negotiate-version connection)
         _ (attach-filesystem connection uname aname)]
     connection)))
