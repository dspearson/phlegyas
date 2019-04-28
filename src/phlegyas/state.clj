(ns phlegyas.state
  (:require [phlegyas.types :refer :all]
            [phlegyas.vfs :refer :all]
            [phlegyas.frames :refer :all]
            [phlegyas.util :refer :all]
            [clojure.string :as string]
            [clojure.core.async :as async]
            [clojure.core.incubator :as i]
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
         frame-update# (assoc (:reply ~data) :frame reply-typ#)]
     (if state-update#
       (swap! ~'state state-update#))
     (into ~'frame frame-update#)))

(defn-frame-binding Tversion
  [frame connection]
  (cond
    (not (string/starts-with? frame-version protocol-version)) (state! {:reply {:version "unknown"}})
    (<= frame-msize max-message-size) (state! {:update (fn [x] (assoc x :msize frame-msize))
                                               :reply {:version protocol-version}})
    :else (state! {:update (fn [x] (assoc x :msize max-message-size))
                   :reply {:version protocol-version
                           :msize max-message-size}})))

(defn-frame-binding Tauth
  [frame connection]
  (error! "no authentication required"))

(defn-frame-binding Tattach
  [frame connection]
  (let [root-fs ((:root-filesystem current-state))
        root-fs-id (:id root-fs)
        root-path (:root-path root-fs)]
    (state! {:update (fn [x] (-> x
                                (add-fs root-fs)
                                (add-fid frame-fid frame-tag)
                                (add-mapping frame-fid root-fs-id root-path)
                                (add-role root-fs-id frame-uname frame-uname)))
             :reply (path->qid root-fs root-path)})))

(defn-frame-binding Tflush
  [frame connection]
  (state! {}))

(defn-frame-binding Twalk
  [frame connection]
  (if (= (count frame-wnames) 0)
    (state! {:update (fn [x] (-> x
                                (add-fid frame-newfid frame-tag)
                                (add-mapping frame-newfid fs-name path)))
             :reply {:nwqids []}})
    (let [wname-paths (walk-path fs path frame-wnames)
          qids (vec (for [p wname-paths] (stat->qid (path->stat fs p))))]
      (if (< (count wname-paths) (count frame-wnames))
        (state! {:reply {:nwqids qids}})
        (state! {:update (fn [x] (-> x
                                    (add-fid frame-newfid frame-tag)
                                    (add-mapping frame-newfid fs-name (last wname-paths))))
                 :reply {:nwqids qids}})))))

(defn-frame-binding Topen
  [frame connection]
  (let [role (fid->role frame-fid current-state)
        stat (path->stat fs path)]
    (if (not (permission-check stat role :oread))
      (error! "no read permission")
      (state! {:update (fn [x] (update-mapping x frame-fid {:offset 0}))
               :reply {:iounit (iounit!)
                       :qid-type (:qid-type stat)
                       :qid-vers (:qid-vers stat)
                       :qid-path (:qid-path stat)}}))))

(defn-frame-binding Tcreate
  [frame connection]
  (let [new-stat (create-synthetic-file frame-name #'example-function-for-files)
        parent-stat (fid->stat current-state frame-fid)
        parent-path (:parent parent-stat)
        file-path (next-available-path fs)]
    (state! {:update (fn [x]
                       (-> x
                           (assoc-in [:fs-map fs-name :files (keywordize file-path)]
                                     (into new-stat {:qid-path file-path :parent parent-path}))
                           (update-in [:fs-map fs-name :files parent-path]
                                      (fn [y] (assoc y :children (assoc (:children y) (keywordize (sha-str (:name new-stat))) (keywordize file-path)))))))
             :reply {:qid-type (:qid-type new-stat)
                     :qid-vers (:qid-vers new-stat)
                     :qid-path file-path
                     :iounit (iounit!)}})))

(defn-frame-binding Tread
  [frame connection]
  (let [stat (path->stat fs (:path mapping))
        typ (stat-type stat)]
    (case typ

      :dir (if (and (> frame-offset 0) (not (= frame-offset (:offset mapping))))
             ; reads of directories must begin at offset 0, or the previous offset +
             ; the last returned byte count, for followup reads of incomplete data.
             ; i.e., either offset = 0, or offset = previous offset + previous count.
             (error! "cannot seek in directories!")

             (let [dirpaths (if (= frame-offset 0)        ; read directory from beginning...
                              (:children stat)            ; so return all children.
                              (:paths-remaining mapping)) ; or else, we are continuing a previous read call.

                   ; `directory-reader` returns a vector, first the data, and second, any remaining paths not visited.
                   ; this can happen if the read call on a directory is larger than the size allowed in a message,
                   ; and read calls can only return integral stat entries, so we need to store the list of paths that
                   ; were not visited in this iteration, so that followup reads can continue where we left off.
                   [dir-data paths-remaining] (directory-reader fs dirpaths frame-count)
                   delivered-byte-count (count dir-data)]

               (state! {:update (fn [x] (update-mapping x frame-fid {:offset (+ frame-offset (count dir-data))
                                                                    :paths-remaining paths-remaining}))
                        :reply {:data dir-data}})))

      :file (if (and (not= 0 (:length stat)) (>= frame-offset (:length stat))) ; if offset >= length, it means that we are
              (state! {:reply {:data nil}})                                    ; reading beyond end of file, so return no data.
              (let [file-data (fetch-data connection frame stat)]             ; else, read file.
                (state! {:reply {:data file-data}})))

      :append (let [file-data (fetch-data connection frame stat)]
                (state! {:reply {:data file-data}})))))

(defn-frame-binding Twrite
  [frame connection]
  (let [stat (fid->stat current-state frame-fid)
        write-fn (:write-fn stat)]
    (if write-fn
      (let [bytes-written (write-fn :connection connection :frame frame :stat stat)]
        (state! {:reply {:count bytes-written}}))
      (error! "not implemented"))))

(defn-frame-binding Tclunk
  [frame connection]
  (state! {:update (fn [x] (-> x
                              (i/dissoc-in [:fids frame-fid])
                              (i/dissoc-in [:mapping frame-fid])))}))

(defn-frame-binding Tremove
  [frame connection]
  (let [stat (fid->stat current-state frame-fid)
        dir-stat (get (:files fs) (:parent stat))
        new-children (dissoc (:children dir-stat) (keywordize (sha-str (:name stat))))]
    (state! {:update (fn [x] (-> x
                                (update-in [:fs-map fs-name :files (:parent stat)] (fn [y] (assoc y :children new-children)))
                                (i/dissoc-in [:fs-map fs-name :files (keywordize (:qid-path stat))])))})))

(defn-frame-binding Tstat
  [frame connection]
  (let [stat (fid->stat current-state frame-fid)]
    (state! {:reply stat})))

(defn-frame-binding Twstat
  [frame connection]
  (state! {}))

(def state-handlers ((fn [] (into {} (for [[k v] frame-byte] [k (-> k name symbol resolve)])))))

(defn state-handler
  "An example state handler. Takes in a `frame`, the `state` atom, and an outport.
  Messages that reach a 9P server can be executed in any order, and it is the job
  of the client to ensure that it does not send conflicting messages before the
  acknowledgement of a previous action has been sent. Therefore, this can be
  executed asynchronously inside a future."
  [frame connection out]
  (log/debug (:transaction-id frame) "in:" frame)
  (conj-val (:in-flight-requests connection) (:tag frame))
  (let [reply (((:frame frame) state-handlers) frame connection)]
    (log/trace (:transaction-id frame) "state:" @(:state connection))
    (log/debug (:transaction-id frame) "out:" reply)
    (s/put! out reply)
    (disj-val (:in-flight-requests connection) (:tag frame))))

(defn consume [in out connection f]
  (d/loop []
    (d/chain (s/take! in ::drained)
             ;; if we got a message, run it through `f`
             ;; as a future, and immediately return.
             (fn [msg]
               (if (identical? ::drained msg)
                 ::drained
                 (do
                   (d/future (f msg connection out))
                   ::future)))

             ;; recur, unless the stream is already drained
             (fn [result]
               (when-not (identical? ::drained result)
                 (d/recur))))))
