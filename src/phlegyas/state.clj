(ns phlegyas.state
  (:require [phlegyas.types :refer :all]
            [phlegyas.vfs :refer :all]
            [phlegyas.frames :refer :all]
            [phlegyas.util :refer :all]
            [manifold.stream :as s]
            [taoensso.timbre :as log]))

(def pfx "phlegyas.state/mutate-")

(defmacro error!
  [ermsg]
  `(assoc ~'state :next-frame (into ~'frame {:frame :error :ename ~ermsg})))

(defmacro state!
  [data]
  `(let [changed-state# (:update ~data)
         next-frame# (:reply ~data)]
     (into (into ~'state changed-state#) {:next-frame (into ~'frame next-frame#)})))

(defn mutate-auth
  [frame state]
  (error! "no authentication required"))

(defn mutate-clunk
  [frame state]
  (let [fid (:fid frame)]
    (state! {:update {:fids (disj (:fids state) fid)
                      :mapping (dissoc (:mapping state) fid)}})))

(defn mutate-attach
  [frame state]
  (let [fid (:fid frame)
        [fs-name path fs] (filesystem!)
        fs-map (assoc (:fs-map state) fs-name fs)
        fids (set (conj (:fids state) fid))
        mapping (assoc (:mapping state) fid {:filesystem fs-name :path path})
        role (assoc (:user state) fs-name {:uid (:uname frame) :gid (:uname frame)})]
    (state! {:update {:fs-map fs-map
                      :fids fids
                      :mapping mapping
                      :role role}
             :reply (path->qid fs path)})))

(defn mutate-stat
  [frame state]
  (let [fid (:fid frame)
        mapping (get (:mapping state) fid)
        fs-name (:filesystem mapping)
        fs (fs-name (:fs-map state))
        path (:path mapping)
        stat (stat-file fs path)]
    (state! {:reply stat})))

(defn mutate-version
  [frame state]
  (let [requested-message-size (:msize frame)
        version-string (:version frame)]
    (cond
      (not (clojure.string/starts-with? version-string protocol-version)) (error! "incompatible version")
      (<= requested-message-size max-message-size) (state! {:update {:msize requested-message-size}})
      :else (error! "requested message size too high."))))

(defn mutate-walk
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
        (log/debug "in wname-paths walking routine")
        (log/debug "got wname-paths:" wname-paths)
        (if (empty? wname-paths)
          (error! "path cannot be walked")
          (state! {:update {:fids (conj (:fids state) newfid)
                            :mapping (assoc (:mapping state) newfid {:filesystem fs-name :path (last wname-paths)})}
                         :reply {:nwqid (short (count wname-paths))
                                 :nwqids qids}}))))))

(defn mutate-open
  [frame state]
  (let [mode ((keyword (str (:mode frame))) access-mode-r)
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
      (state! {:reply {:iounit (- (:msize state) 24)
                       :qtype (:qtype qid)
                       :qvers (:qvers qid)
                       :qpath (:qpath qid)}}))))

(defn mutate-openfd
  [frame state]
  (error! "not implemented"))

(defn mutate-write
  [frame state]
  (error! "not implemented"))

(defn mutate-create
  [frame state]
  (error! "not implemented"))

(defn mutate-remove
  [frame state]
  (error! "not implemented"))

(defn mutate-wstat
  [frame state]
  (error! "not implemented"))

(defn mutate-flush
  [frame state]
  (state! {}))

(defn mutate-read
  [frame state]
  (let [offset (:offset frame)
        byte-count (:count frame)
        mapping (fid->mapping (:fid frame) state)
        fs ((:filesystem mapping) (:fs-map state))
        stat (path->stat fs (:path mapping))
        typ (stat-type stat)]
    (case typ
      :qtdir (if (> offset 0)
               (state! {:reply {:data {:type :error}}})
               (state! {:reply {:data {:type :directory :data (into [] (for [x (:children stat)] (path->stat fs x)))}}}))
      :qtfile (if (>= offset (:len stat))
                (state! {:reply {:data {:type :error}}})
                (state! {:reply {:data {:type :raw :data ((:contents stat) {:stat stat :offset offset :count byte-count})}}})))))

(def state-handlers ((fn [] (into {} (for [[k v] message-type] [k (-> (str pfx (name k)) symbol resolve)])))))

(defn update-state
  [in out state]
  (let [updated-state (((:frame in) state-handlers) in state)]
    (s/put! out (construct-packet :R (:next-frame updated-state)))
    (dissoc updated-state :next-frame)))
