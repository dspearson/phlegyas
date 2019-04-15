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
  `{:frame (into ~'frame {:frame :Rerror :ename ~ermsg})})

(defmacro state!
  [data]
  `(let [changed-state# (:update ~data)
         update-fn# (:update-fn ~data)
         reply-typ# ((keywordize (+ 1 ((:frame ~'frame) ~'frame-byte))) ~'reverse-frame-byte)
         next-frame# (assoc (:reply ~data) :frame reply-typ#)
         state-change# (if changed-state#                        ;; do the state change asynchronously,
                         (d/future                               ;; wrapped in a future,
                           (if update-fn#                        ;; so we can return a reply immediately if desired...
                             (update-fn# ~'state changed-state#) ;; and even pass in a specialist fn for state change.
                             (swap! ~'state (fn [x# y#] (into x# y#)) changed-state#)))
                         nil)]
     {:frame (into ~'frame next-frame#)
      :metadata {:state-change state-change#}}))

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
                            :mapping (assoc (:mapping current-state) newfid {:filesystem fs-name :path (last wname-paths) :offset 0})}
                   :reply {:nwqids qids}}))))))

(defn Topen
  [frame state]
  (let [current-state @state
        fid (:fid frame)
        mapping (fid->mapping fid current-state)
        fs ((:filesystem mapping) (:fs-map current-state))
        path (:path mapping)
        role (fid->role fid current-state)
        stat (path->stat fs path)]
    (if (not (permission-check stat role :oread))
      (error! "no read permission")
      (state! {:update {:mapping (update-mapping fid (:mapping current-state) {:offset 0})}
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
        mapping (fid->mapping (:fid frame) @state)
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
               (state! {:update {:mapping (assoc (:mapping @state)                                   ; update the mapping
                                                 (:fid frame)                                        ; for followup reads:
                                                 (into mapping {:offset (+ offset (count data))      ; update the offset and the
                                                                :paths-remaining paths-remaining}))} ; list of unread directories.
                        :reply {:data data}})))

      :file (if (>= offset (:length stat))                                                 ; if offset >= length, it means that we are
              (state! {:reply {:data nil}})                                                ; reading beyond end of file, so return no data.
              (let [data ((:contents stat) {:stat stat :offset offset :count byte-count})] ; else, read file via calling the contents fn.
                (state! {:reply {:data data}}))))))

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
  "An example state handler. Takes in a `frame`, the `state` atom, and an outport.
  Messages that reach a 9P server can be executed in any order, and it is the job
  of the client to ensure that it does not send conflicting messages before the
  acknowledgement of a previous action has been sent. Therefore, this can be
  executed asynchronously inside a future."
  [frame state out]
  (let [reply (((:frame frame) state-handlers) frame state)
        next-frame (:frame reply)
        state-change (:state-change (:metadata reply))]

    ;; if you need to block on state changes, here's a place to do it.
    ;; you could even put the insertion of the reply frame into a deferred.
    ;; (if state-change
    ;;   (log/info "State change occurred." @state-change))

    (s/put! out next-frame)))

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
