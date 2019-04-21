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
  (with-frame-bindings frame
    (do
      (cond
        (not (string/starts-with? frame-version protocol-version)) (state! {:reply {:version "unknown"}})
        (<= frame-msize max-message-size) (state! {:update (fn [x] (assoc x :msize frame-msize))
                                                   :reply {:version protocol-version}})
        :else (state! {:update (fn [x] (assoc x :msize max-message-size))
                       :reply {:version protocol-version
                               :msize max-message-size}})))))

(defn Tauth
  [frame state]
  (error! "no authentication required"))

(defn Tattach
  [frame state]
  (with-frame-bindings frame
    (do
      (let [root-fs ((:root-filesystem current-state))
            root-fs-id (:id root-fs)
            root-path (:root-path root-fs)]
        (state! {:update (fn [x] (-> x
                                    (add-fs root-fs)
                                    (add-fid frame-fid frame-tag)
                                    (add-mapping frame-fid root-fs-id root-path)
                                    (add-role root-fs-id frame-uname frame-uname)))
                 :reply (path->qid root-fs root-path)})))))

(defn Tflush
  [frame state]
  (state! {}))

(defn Twalk
  [frame state]
  (with-frame-bindings frame
    (do
      (if (= (count frame-wnames) 0)
        (state! {:update (fn [x] (-> x
                                    (add-fid frame-newfid frame-tag)
                                    (add-mapping frame-newfid fs-name path)))
                 :reply {:nwqids []}})
        (let [wname-paths (walk-path fs path frame-wnames)
              qids (for [p wname-paths] (stat->qid (path->stat fs p)))]
          (cond
            (< (count wname-paths) (count frame-wnames))
            (state! {:reply {:nwqids qids}})

            :else
            (state! {:update (fn [x] (-> x
                                        (add-fid frame-newfid frame-tag)
                                        (add-mapping frame-newfid fs-name (last wname-paths))))
                     :reply {:nwqids qids}})))))))

(defn Topen
  [frame state]
  (with-frame-bindings frame
    (do
      (let [role (fid->role frame-fid current-state)
            stat (path->stat fs path)]
        (if (not (permission-check stat role :oread))
          (error! "no read permission")
          (state! {:update (fn [x] (update-mapping x frame-fid {:offset 0}))
                   :reply {:iounit (iounit!)
                           :qid-type (:qid-type stat)
                           :qid-vers (:qid-vers stat)
                           :qid-path (:qid-path stat)}}))))))

(defn Tcreate
  [frame state]
  (with-frame-bindings frame
    (do
      (let [new-file (synthetic-file frame-name #'example-function-for-files)]
        (state! {:update (fn [x]
                           (update-in x [:fs-map fs-name]
                                      (-> fs (insert-file! path new-file))))
                 :reply {:qid-type (:qid-type new-file)
                         :qid-vers (:qid-vers new-file)
                         :qid-path (:qid-path new-file)}})))))

(defn Tread
  [frame state]
  (with-frame-bindings frame
    (do
      (let [stat (path->stat fs (:path mapping))
            typ (stat-type stat)]
        (case typ

          :dir (if (and
                    (> frame-offset 0)
                    (not (= frame-offset (:offset mapping))))
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
                   [data paths-remaining] (directory-reader (into [] (for [x dirpaths] (path->stat fs x))) frame-count)
                   delivered-byte-count (count data)]
               (state! {:update (fn [x] (update-mapping x frame-fid {:offset (+ frame-offset (count data))
                                                                    :paths-remaining paths-remaining}))
                        :reply {:data data}})))

      :file (if (and (not= 0 (:length stat)) (>= frame-offset (:length stat))) ; if offset >= length, it means that we are
              (state! {:reply {:data nil}})                                    ; reading beyond end of file, so return no data.
              (let [data (fetch-data frame state :stat stat)]                  ; else, read file.
                (state! {:reply {:data data}})))

      :append (let [data (fetch-data frame state :stat stat)]
                (state! {:reply {:data data}})))))))

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
  (with-frame-bindings frame
    (do
      (state! {:update (fn [x] (-> (into x {:fids (dissoc (:fids x) frame-fid)
                                           :mapping (dissoc (:mapping x) frame-fid)})))}))))

(defn Tremove
  [frame state]
  (with-frame-bindings frame
    (do
      (let [stat (fid->stat current-state frame-fid)
            dir-stat (fid->stat current-state (:parent stat))
            new-children (disj (:children dir-stat) (:qid-path stat))]
        (state! {:update (fn [x] (update-stat x (:parent stat){:children new-children}))})))))

(defn Tstat
  [frame state]
  (with-frame-bindings frame
    (do
      (let [stat (stat-file fs path)]
        (state! {:reply stat})))))

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
  (log/debug "in: "frame)
  (let [reply (((:frame frame) state-handlers) frame state)]
    (log/debug "out: " reply)
    (s/put! out reply)))

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
