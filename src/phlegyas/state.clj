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
  `(let [state-update# (:update ~data)
         reply-typ# ((keywordize (+ 1 ((:frame ~'frame) ~'frame-byte))) ~'reverse-frame-byte)
         frame-update# (assoc (:reply ~data) :frame reply-typ#)
         _# (if state-update#
             (swap! ~'state state-update#))]
     (into ~'frame frame-update#)))

(defn Tversion
  [frame state]
  (let [requested-message-size (:msize frame)
        version-string (:version frame)]
    (cond
      (not (string/starts-with? version-string protocol-version)) (state! {:reply {:version "unknown"}})
      (<= requested-message-size max-message-size) (state! {:update (fn [x] (assoc x :msize max-message-size))
                                                            :reply {:version protocol-version}})
      :else (state! {:update (fn [x] (assoc x :msize max-message-size))
                     :reply {:version protocol-version
                             :msize max-message-size}}))))

(defn Tauth
  [frame state]
  (error! "no authentication required"))

(defn Tattach
  [frame state]
  (let [fid (:fid frame)
        fs ((:root-filesystem @state))
        fsid (:id fs)
        path (:root-path fs)
        uid (:uname frame)
        gid (:uname frame)]
    (state! {:update (fn [x] (-> x
                                (add-fs fs)
                                (add-fid fid)
                                (add-mapping fid fsid path)
                                (add-role fsid uid gid)))
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
      (state! {:update (fn [x] (-> x
                                  (add-fid newfid)
                                  (add-mapping newfid fs-name path)))
               :reply {:nwqids []}})
      (let [wname-paths (walk-path fs path wnames)
            qids (for [p wname-paths] (stat->qid (path->stat fs p)))]
        (if (empty? wname-paths)
          (error! "path cannot be walked")
          (state! {:update (fn [x] (-> x
                                      (add-fid newfid)
                                      (add-mapping newfid fs-name (last wname-paths))))
                   :reply {:nwqids qids}}))))))

(defn Topen
  [frame state]
  (let [current-state @state
        fid (:fid frame)
        mapping (fid->mapping current-state fid)
        fs ((:filesystem mapping) (:fs-map current-state))
        path (:path mapping)
        role (fid->role fid current-state)
        stat (path->stat fs path)]
    (if (not (permission-check stat role :oread))
      (error! "no read permission")
      (state! {:update (fn [x] (update-mapping x fid {:offset 0}))
               :reply {:iounit (iounit!)
                       :qid-type (:qid-type stat)
                       :qid-vers (:qid-vers stat)
                       :qid-path (:qid-path stat)}}))))

(defn Tcreate
  [frame state]
  (error! "not implemented"))

(defn Tread
  [frame state]
  (let [offset (:offset frame)
        byte-count (:count frame)
        fid (:fid frame)
        mapping (fid->mapping @state fid)
        fs ((:filesystem mapping) (:fs-map @state))
        stat (path->stat fs (:path mapping))
        typ (stat-type stat)]
    (case typ

      :dir (if (and
                (> offset 0)
                (not (= offset (:offset mapping))))
             ; reads of directories must begin at offset 0, or the previous offset +
             ; the last returned byte count, for followup reads of incomplete data.
             ; i.e., either offset = 0, or offset = previous offset + previous count.
             (error! "cannot seek in directories!")

             (let [dirpaths (if (= offset 0)              ; read directory from beginning...
                              (:children stat)            ; so return all children.
                              (:paths-remaining mapping)) ; or else, we are continuing a previous read call.

                   ; `directory-reader` returns a vector, first the data, and second, any remaining paths not visited.
                   ; this can happen if the read call on a directory is larger than the size allowed in a message,
                   ; and read calls can only return integral stat entries, so we need to store the list of paths that
                   ; were not visited in this iteration, so that followup reads can continue where we left off.
                   [data paths-remaining] (directory-reader (into [] (for [x dirpaths] (path->stat fs x))) byte-count)
                   delivered-byte-count (count data)]
               (state! {:update (fn [x] (update-mapping x fid {:offset (+ offset (count data))
                                                              :paths-remaining paths-remaining}))
                        :reply {:data data}})))

      :file (if (and (not= 0 (:length stat)) (>= offset (:length stat))) ; if offset >= length, it means that we are
              (state! {:reply {:data nil}})                              ; reading beyond end of file, so return no data.
              (let [data ((:read-fn stat) stat frame state)]             ; else, read file via calling the read-fn.
                (state! {:reply {:data data}})))

      :append (let [data ((:read-fn stat) stat frame state)]
                  (state! {:reply {:data data}})))))

(defn Twrite
  [frame state]
  (let [stat (fid->stat @state (:fid frame))
        write-fn (:write-fn stat)]
    (if write-fn
      (let [bytes-written (write-fn stat frame state)]
        (state! {:reply {:count bytes-written}}))
      (error! "not implemented"))))

(defn Tclunk
  [frame state]
  (let [current-state @state
        fid (:fid frame)]
    (state! {:update (fn [x] (-> (into x {:fids (disj (:fids x) fid)
                                         :mapping (dissoc (:mapping x) fid)})))})))

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
  "An example state handler. Takes in a `frame`, the `state` atom, and an outport.
  Messages that reach a 9P server can be executed in any order, and it is the job
  of the client to ensure that it does not send conflicting messages before the
  acknowledgement of a previous action has been sent. Therefore, this can be
  executed asynchronously inside a future."
  [frame state out]
  (s/put! out (((:frame frame) state-handlers) frame state)))

(defn consume-with-state [in out state f]
  (d/loop []
    (d/chain (s/take! in ::drained)
             ;; if we got a message, run it through `f`
             ;; as a future, and immediately return.
             (fn [frame]
               (if (identical? ::drained frame)
                 ::drained
                 (do
                   (d/future (f frame state out))
                   ::future)))

             ;; recur, unless the stream is already drained
             (fn [result]
               (when-not (identical? ::drained result)
                 (d/recur))))))
