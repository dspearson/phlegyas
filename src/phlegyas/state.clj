(ns phlegyas.state
  (:require [phlegyas.types :refer :all]
            [phlegyas.vfs :refer :all]
            [phlegyas.frames :refer :all]
            [phlegyas.util :refer :all]
            [manifold.stream :as s]
            [taoensso.timbre :as log]))

;; an example state machine

(defmacro iounit!
  []
  `(- (:msize ~'state) 24))

(defmacro error!
  [ermsg]
  `(assoc ~'state :next-frame (into ~'frame {:frame :Rerror :ename ~ermsg})))

(defmacro state!
  [data]
  `(let [changed-state# (:update ~data)
         reply-typ# ((keywordize (+ 1 ((:frame ~'frame) ~'message-type))) ~'reverse-message-type)
         next-frame# (assoc (:reply ~data) :frame reply-typ#)]
     (into (into ~'state changed-state#) {:next-frame (into ~'frame next-frame#)})))

(defn Tversion
  [frame state]
  (let [requested-message-size (:msize frame)
        version-string (:version frame)]
    (cond
      (not (clojure.string/starts-with? version-string protocol-version)) (state! {:reply {:version "unknown"}})
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
  (let [fid (:fid frame)
        fs ((:root-filesystem state))
        fs-map (assoc (:fs-map state) (:id fs) fs)
        fids (set (conj (:fids state) fid))
        mapping (assoc (:mapping state) fid {:filesystem (:id fs) :path (:root-path fs)})
        role (assoc (:user state) (:id fs) {:uid (:uname frame) :gid (:uname frame)})]
    (state! {:update {:fs-map fs-map
                      :fids fids
                      :mapping mapping
                      :role role}
             :reply (path->qid fs (:root-path fs))})))

(defn Tflush
  [frame state]
  (state! {}))

(defn Twalk
  [frame state]
  (let [fid (:fid frame)
        newfid (:newfid frame)
        wnames (:wname frame)
        mapping (get (:mapping state) fid)
        fs-name (:filesystem mapping)
        fs (fs-name (:fs-map state))
        path (:path mapping)]
    (if (= (count wnames) 0)
      (state! {:update (assoc-fid state fid newfid)
               :reply {:nwqid 0 :nwqids []}})
      (let [wname-paths (walk-path fs path wnames)
            qids (for [p wname-paths] (stat->qid (path->stat fs p)))]
        (if (empty? wname-paths)
          (error! "path cannot be walked")
          (state! {:update {:fids (conj (:fids state) newfid)
                            :mapping (assoc (:mapping state) newfid {:filesystem fs-name :path (last wname-paths)})}
                   :reply {:nwqid (count wname-paths)
                           :nwqids qids}}))))))

(defn Topen
  [frame state]
  (let [mode ((keyword (str (:mode frame))) reverse-access-mode)
        fid (:fid frame)
        mapping (get (:mapping state) fid)
        fs-name (:filesystem mapping)
        fs (fs-name (:fs-map state))
        path (:path mapping)
        role (fid->role fid state)
        stat (path->stat fs path)
        qid (stat->qid stat)]
    (if (not (permission-check stat role :oread))
      (error! "no read permission")
      (state! {:reply {:iounit (iounit!)
                       :qtype (:qtype qid)
                       :qvers (:qvers qid)
                       :qpath (:qpath qid)}}))))

(defn Tcreate
  [frame state]
  (error! "not implemented"))

(defn Tread
  [frame state]
  (let [offset (:offset frame)
        byte-count (:count frame)
        mapping (fid->mapping (:fid frame) state)
        fs ((:filesystem mapping) (:fs-map state))
        stat (path->stat fs (:path mapping))
        typ (stat-type stat)]
    (case typ
      :qtdir (if (> offset 0)
               (state! {:reply {:data nil}})
               (state! {:reply {:data {:type :directory :data (into [] (for [x (:children stat)] (path->stat fs x)))}}}))
      :qtfile (if (>= offset (:len stat))
                (state! {:reply {:data nil}})
                (state! {:reply {:data ((:contents stat) {:stat stat :offset offset :count byte-count})}})))))

(defn Twrite
  [frame state]
  (error! "not implemented"))

(defn Tclunk
  [frame state]
  (let [fid (:fid frame)]
    (state! {:update {:fids (disj (:fids state) fid)
                      :mapping (dissoc (:mapping state) fid)}})))

(defn Tremove
  [frame state]
  (error! "not implemented"))

(defn Tstat
  [frame state]
  (let [fid (:fid frame)
        mapping (get (:mapping state) fid)
        fs-name (:filesystem mapping)
        fs (fs-name (:fs-map state))
        path (:path mapping)
        stat (stat-file fs path)]
    (state! {:reply stat})))

(defn Twstat
  [frame state]
  (error! "not implemented"))

(def state-handlers ((fn [] (into {} (for [[k v] message-type] [k (-> k name symbol resolve)])))))

(defn mutate-state
  [in out state]
  (let [updated-state (((:frame in) state-handlers) in state)]
    (s/put! out (assemble-packet (:next-frame updated-state)))
    (dissoc updated-state :next-frame)))
