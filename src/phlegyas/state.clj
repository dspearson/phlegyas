(ns phlegyas.state
  (:require [phlegyas.types :refer :all]
            [phlegyas.vfs :refer :all]
            [phlegyas.frames :refer :all]
            [phlegyas.util :refer :all]
            [clojure.string :as string]
            [clojure.core.async :as async]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [taoensso.timbre :as log]))

;; an example state machine

(defmacro iounit!
  []
  `(- (:msize ~'@state) 24))

(defmacro error!
  [ermsg]
  `(into ~'frame {:frame :Rerror :ename ~ermsg}))

(defmacro state!
  [data]
  `(let [changed-state# (:update ~data)
         reply-typ# ((keywordize (+ 1 ((:frame ~'frame) ~'frame-byte))) ~'reverse-frame-byte)
         next-frame# (assoc (:reply ~data) :frame reply-typ#)]
     (if changed-state#
       (swap! ~'state (fn [x# y#] (into x# y#)) changed-state#))
     (into ~'frame next-frame#)))

(defn Tversion
  [frame state]
  (let [requested-message-size (:msize frame)
        version-string (:version frame)]
    (cond
      (not (string/starts-with? version-string protocol-version)) (state! {:reply {:version "unknown"}})
      (<= requested-message-size max-message-size) (state! {:update {:msize requested-message-size}
                                                            :reply {:version protocol-version}})
      :else (state! {:update {:msize max-message-size}
                     :reply {:version protocol-version
                             :msize max-message-size}}))))

(defn Tauth
  [frame state]
  (error! "no authentication required"))

(defn Tattach
  [frame state]
  (let [current-state @state
        fid (:fid frame)
        fs ((:root-filesystem current-state))
        fs-map (assoc (:fs-map current-state) (:id fs) fs)
        fids (set (conj (:fids current-state) fid))
        mapping (assoc (:mapping current-state) fid {:filesystem (:id fs) :path (:root-path fs)})
        role (assoc (:user current-state) (:id fs) {:uid (:uname frame) :gid (:uname frame)})]
    (state! {:update {:fs-map fs-map
                      :fids fids
                      :root-filesystem-name (:id fs)
                      :mapping mapping
                      :role role}
             :reply (path->qid fs (:root-path fs))})))

(defn Tflush
  [frame state]
  (state! {}))

(defn Twalk
  [frame state]
  (let [current-state @state
        fid (:fid frame)
        newfid (:newfid frame)
        wnames (:wnames frame)
        mapping (get (:mapping current-state) fid)
        fs-name (:filesystem mapping)
        fs (fs-name (:fs-map current-state))
        path (:path mapping)]
    (if (= (count wnames) 0)
      (state! {:update (assoc-fid current-state fid newfid)
               :reply {:nwqids []}})
      (let [wname-paths (walk-path fs path wnames)
            qids (for [p wname-paths] (stat->qid (path->stat fs p)))]
        (if (empty? wname-paths)
          (error! "path cannot be walked")
          (state! {:update {:fids (conj (:fids current-state) newfid)
                            :mapping (assoc (:mapping current-state) newfid {:filesystem fs-name :path (last wname-paths)})}
                   :reply {:nwqids qids}}))))))

(defn Topen
  [frame state]
  (let [current-state @state
        fid (:fid frame)
        mapping (get (:mapping current-state) fid)
        fs-name (:filesystem mapping)
        fs (fs-name (:fs-map current-state))
        path (:path mapping)
        role (fid->role fid current-state)
        stat (path->stat fs path)
        qid (stat->qid stat)]
    (if (not (permission-check stat role :oread))
      (error! "no read permission")
      (state! {:reply {:iounit (iounit!)
                       :qid-type (:qid-type qid)
                       :qid-vers (:qid-vers qid)
                       :qid-path (:qid-path qid)}}))))

(defn Tcreate
  [frame state]
  (error! "not implemented"))

(defn Tread
  [frame state]
  (let [offset (:offset frame)
        byte-count (:count frame)
        mapping (fid->mapping (:fid frame) @state)
        fs ((:filesystem mapping) (:fs-map @state))
        stat (path->stat fs (:path mapping))
        typ (stat-type stat)]
    (case typ
      :dir (if (> offset 0)
             (state! {:reply {:data nil}})
             (state! {:reply {:data {:type :directory :data (into [] (for [x (:children stat)] (path->stat fs x)))}}})) ;; FIXME: see [1]
      :file (if (>= offset (:length stat))
              (state! {:reply {:data nil}})
              (state! {:reply {:data ((:contents stat) {:stat stat :offset offset :count byte-count})}})))))

(defn Twrite
  [frame state]
  (error! "not implemented"))

(defn Tclunk
  [frame state]
  (let [current-state @state
        fid (:fid frame)]
    (state! {:update {:fids (disj (:fids current-state) fid)
                      :mapping (dissoc (:mapping current-state) fid)}})))

(defn Tremove
  [frame state]
  (error! "not implemented"))

(defn Tstat
  [frame state]
  (let [current-state @state
        fid (:fid frame)
        mapping (get (:mapping current-state) fid)
        fs-name (:filesystem mapping)
        fs (fs-name (:fs-map current-state))
        path (:path mapping)
        stat (stat-file fs path)]
    (state! {:reply stat})))

(defn Twstat
  [frame state]
  (error! "not implemented"))

(def state-handlers ((fn [] (into {} (for [[k v] frame-byte] [k (-> k name symbol resolve)])))))

(defn state-handler
  [frame state out]
  (s/put! out (((:frame frame) state-handlers) frame state)))

(defn ->state
  "Send state change to the state thread via the stream, and then block waiting for the state
  change to take place."
  [mutation stream]
  (let [reply (d/deferred)]
    (s/put! stream [mutation reply])
    @reply))

(defn state-machine
  "Start a thread which will hold the state for the connection. This takes in a stream which
  will be operated on by `->state`, and the state atom. This design allows for blocking by the
  sending thread to verify that the state change actually occurred. State changes will happen
  in their own thread, to avoid blocking. Messages sent in to the stream shall be a vector.
  The first position is typically a map, the second is a deferred which is delivered after the
  change is made, and the third position is optionally a mutation function which can be passed
  in when the change is more involved."
  [in state]
  (async/thread
    (loop []
      (let [[mut done? optional-fn] @(s/take! in)]
        (if (nil? mut)
          nil
          (do
            (async/go
              (if optional-fn
                (optional-fn mut state)
                (swap! state (fn [x y] (into x y)) mut))
              (d/success! done? true))
            (recur)))))))

(defn consume-with-state [in out state f]
  (d/loop []
    (d/chain (s/take! in ::drained)
             ;; if we got a message, run it through `f`
             (fn [frame]
               (if (identical? ::drained frame)
                 ::drained
                 (f frame state out)))

             ;; wait for the result from `f` to be realized, and
             ;; recur, unless the stream is already drained
             (fn [result]
               (when-not (identical? ::drained result)
                 (d/recur))))))

;; [1] Currently, if the directory size is larger than the msize, this will not work.
;;     Seeking in a directory is not permitted, except to the beginning, but if the
;;     msize is smaller than the data, an offset at the last position of the file
;;     should be honoured, and buffers should only be allocated up to the maximum msize.
;;     This perhaps can be handled by some open fid state which shows the current
;;     position in the file.
