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

(set! *warn-on-reflection* true)

;; helper macros

(defmacro iounit!
  "Maximum iounit for messages."
  []
  `(- (:msize ~'@state) 24))

(defmacro error!
  "Return an error frame."
  [ermsg]
  `(into ~'frame {:frame :Rerror :ename ~ermsg}))

(defmacro state!
  "Update the state, and return a frame. Takes a map of the form:
  {:update fn-to-apply-to-state-atom
   :reply {:field-to-add 1
           :another-field 2}}"
  [data]
  `(let [state-update# (:update ~data)
         reply-typ# ((keywordize (+ 1 ((:frame ~'frame) ~'frame-byte))) ~'reverse-frame-byte)
         frame-update# (assoc (:reply ~data) :frame reply-typ#)]
     (if state-update#
       (swap! ~'state state-update#))
     (into ~'frame frame-update#)))

;; protocol message handlers

;; defn-frame-binding is a wrapper around defn which wraps the body forms in `with-frame-bindings`,
;; which is an anamorphic macro that creates a lexical environment (via `let`) and defines a number
;; of useful variables for us. given that many of these are required on each protocol message, this
;; reduces repetition for us. see the `phlegyas.util` namespace for definitions of these helper
;; macros. each protocol message takes a map containing the frame data, and the connection map.
;; the general idea behind this is that the connection map has a state atom. each message we receive
;; either has to look up state and return data from it, or mutate some aspect of it.
;;
;; to make this easy(ish), there is a macro, `state!`, which takes a map as an argument. this map
;; should optionally contain two keys, :update and :reply. :update's value should be a function
;; which embeds the transformations that would be applied to the state atom. :reply is another
;; map containing keys that are in the reply frame, and the data associated with them. we apply
;; this via `into` to the received frame data, so in many cases you do not need to set the majority
;; of data in the frame, as they remain the same in the reply.
;;
;; there is also an `iounit!` macro, that simply dynamically calculates the maximum iounit value
;; that we can reply with given the negotiated message size from the client. it's simply the
;; msize - 24 bytes of protocol header data.
;;
;; there may be bugs in the handling of the protocol, due to my not-so-careful reading of the
;; 9P2000 spec. if you encounter any obvious deficiencies, please let me know. i will improve it
;; as i go.
;;
;; for details of the protocol, see intro(9P) in the Plan 9 documentation.

;; Tversion:
;; 1. first we check that the provided version string starts with 9P2000. if it does not, we do not
;;    recognise it, and reply with "unknown" as per the protocol documentation.
;; 2. then, if the requested msize is less than the maximum message size we allow, we reply with
;;    9P2000 as our protocol version and the negotiated msize.
;; 3. finally, if neither of these are true, then the message size that was requested is larger than
;;    we can handle, so we reply with 9P2000 and our maximum message size, which the client must
;;    use from now on.
(defn-frame-binding Tversion
  [frame connection]
  (cond
    (not (string/starts-with? frame-version protocol-version)) (state! {:reply {:version "unknown"}})
    (<= frame-msize max-message-size) (state! {:update (fn [x] (assoc x :msize frame-msize))
                                               :reply {:version protocol-version}})
    :else (state! {:update (fn [x] (assoc x :msize max-message-size))
                   :reply {:version protocol-version
                           :msize max-message-size}})))

;; Tauth:
;; not currently implemented, i.e. no authentication is required.
(defn-frame-binding Tauth
  [frame connection]
  (error! "no authentication required"))

;; Tattach:
;; this call creates a new view of a filesystem. here, we take the :root-filesystem key
;; from the state atom, which we expect to be a function that when executed with no args,
;; creates a filesystem record for us. see `phlegyas.vfs` for more information.
;; the fn for mutating the state calls `add-fs` to add the filesystem, then calls `add-fid`
;; which adds the requested fid from the frame to the map corresponding to currently allocated
;; fids, then calls `add-mapping` which inserts a mapping between fid to the filesystem id and
;; path of the root of that filesystem, and finally adds a role map which maps the filesystem
;; lookup key to the uname value specified in the attach call.
;; we then reply with the qid of the root of the filesystem.
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

;; Tflush:
;; not currently implemented.
(defn-frame-binding Tflush
  [frame connection]
  (state! {}))

;; Twalk:
;; if wnames is empty, then we are allocating newfid to point to the same
;; filesystem key and path as the provided fid.
;; otherwise, we call `walk-path` to attempt to get all the paths for
;; each entry in wnames. i.e., if we had a filesystem with /a/b/c,
;; and wnames ["a" "b"], we now map newfid to the filesystem+path that
;; corresponds to "b". however, if we got wnames ["a" "b" "d"], then
;; we do NOT map newfid, because the walk was not successful. in both
;; cases, we return all of the qids that we successfully walked.
;;
;; `walk-path` is the function we use to find all the qids for the
;; requested wnames. it returns a vector containing the paths of
;; the walked elements.
;;
;; we then call `path->stat` with the filesystem and walked path, and then
;; `stat->qid` on the result, in a for loop iterating over the returned
;; paths.
;;
;; if the number of paths returned by `walk-path` is less than the amount
;; of wnames, then we know the walk was not successful. we do not allocate
;; newfid, and simply reply with the qids we did walk.
;; otherwise, we add the newfid and create a mapping to the final walked
;; path, replying with the qids.
(defn-frame-binding Twalk
  [frame connection]
  (if (= (count frame-wnames) 0)
    (state! {:update (fn [x] (-> x
                                (add-fid frame-newfid frame-tag)
                                (add-mapping frame-newfid fs-name path)))
             :reply {:nwqids []}})
    (let [wname-paths (walk-path fs path frame-wnames)
          qids (for [p wname-paths] (stat->qid (path->stat fs p)))]
      (if (< (count wname-paths) (count frame-wnames))
        (state! {:reply {:nwqids qids}})
        (state! {:update (fn [x] (-> x
                                    (add-fid frame-newfid frame-tag)
                                    (add-mapping frame-newfid fs-name (last wname-paths))))
                 :reply {:nwqids qids}})))))

;; Topen:
;; contains rudimentary role permission check.
;; if we do have read permission on the file, then we update the mapping
;; to have an :offset key set to 0. this is needed for reads from directories,
;; where seeking is not allowed, so reads of directories need to update the
;; offset so that subsequent reads from the client are legal.
;; we then reply with the iounit, and the qid of the fid we just opened.
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

;; Tcreate:
;; rudimentary example of file creation. all new files are initialised with the
;; `create-synthetic-file` function call, which returns a stat record, with a
;; read+write function set to `example-function-for-files`.
;; we also need to fetch the stat of the parent directory the file is being
;; created in, and need to add the new stat path to its map of children.
;; the path for the new file is chosen by calling `next-available-path`, which
;; just increments an atomic counter. (all paths are unique.)
;; state update consists of associating the new stat (plus its path we just got,
;; and its parent from `parent-stat`) to the filesystem map, and updating the
;; parent stat with the new child path.
;; reply with the qid of the new file, and the iounit (calculated using `iounit!`)
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

;; Tread:
;; first, fetch the stat of the fid, and find out what the type bit is.
;; currently only three types are implemented: :dir, :file, and :append.
;; for directories, seeking is not allowed. we check this is not violated
;; by ensuring that if the frame offset is greater than 0, and it does not
;; match the offset in the fid mapping, then they are violating the protocol.
;; if offset = 0, then we are reading the whole directory, so fetch all the
;; children from the stat. if offset is not 0, and is not a seek, then we
;; fetch :paths-remaining from the fid mapping. these are all the paths that
;; were not returned in the last read call. we then pass the paths to
;; `directory-reader`, which iterates over the paths and returns the packed
;; data to send back to the client, along with the paths it did not walk due
;; to it exceeding the msize for the packet. i.e., [dir-data paths-remaining]
;; we count the numver of bytes in the dir-data, then update the offset in the
;; mapping for the directory to be this data + the last offset, and update
;; the paths-remaining. then we return the packed `dir-data` as :data in the
;; reply.
;; for :file, if the length is not 0 and frame-offset is greater than or
;; equal to the length in the stat, then we have reached EOF. return no data.
;; otherwise, call `fetch-data`, passing in the connection map, frame data, and
;; stat for the fid. this will return packed data for us.
;; for :append, we do not need to check the length. this is handled within the
;; read function of the file instead.
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
              (let [file-data (fetch-data connection frame stat)]              ; else, read file.
                (state! {:reply {:data file-data}})))

      :append (let [file-data (fetch-data connection frame stat)]
                (state! {:reply {:data file-data}})))))

;; Twrite:
;; get the stat of the fid, and the :write-fn in it.
;; if it exists, then we call it, passing in keyword arguments for the connection
;; map, frame map, and the fid stat. this function call should result in the data
;; in the file being updated. write-fns should simply return the number of bytes
;; written to the file.
;; if the :write-fn is not defined, then return error.
(defn-frame-binding Twrite
  [frame connection]
  (let [stat (fid->stat current-state frame-fid)
        write-fn (:write-fn stat)]
    (if write-fn
      (let [bytes-written (write-fn :connection connection :frame frame :stat stat)]
        (state! {:reply {:count bytes-written}}))
      (error! "not implemented"))))

;; Tclunk:
;; dissociate the fid from the map of allocated fids, and remove the mapping
;; of fid to filesystem and path. reply is not required, since the frame we
;; received already had all the data required for the reply.
(defn-frame-binding Tclunk
  [frame connection]
  (state! {:update (fn [x] (-> x
                              (i/dissoc-in [:fids frame-fid])
                              (i/dissoc-in [:mapping frame-fid])))}))

;; Tremove:
;; fetch the stat, the parent directory stat, then a new list of children
;; of the directory stat that no longer contain the path of the fids stat.
;; update dissociates the stat from the filesystem, and applies the update to
;; the parent. no new reply data required.
(defn-frame-binding Tremove
  [frame connection]
  (let [stat (fid->stat current-state frame-fid)
        dir-stat (get (:files fs) (:parent stat))
        new-children (dissoc (:children dir-stat) (keywordize (sha-str (:name stat))))]
    (state! {:update (fn [x] (-> x
                                (update-in [:fs-map fs-name :files (:parent stat)] (fn [y] (assoc y :children new-children)))
                                (i/dissoc-in [:fs-map fs-name :files (keywordize (:qid-path stat))])))})))

;; Tstat:
;; fetch the stat associated with the provided fid, and simply reply with it.
(defn-frame-binding Tstat
  [frame connection]
  (let [stat (fid->stat current-state frame-fid)]
    (state! {:reply stat})))

;; Twstat:
;; not currently implemented.
(defn-frame-binding Twstat
  [frame connection]
  (state! {}))

;; this looks up frame types, and resolves functions for handling them in the current namespace.
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

(defn consume
  "Takes an inport, outport, connection map, and function.
  Recursively takes from import, applies f to it asynchronously.
  Exits when the connection is drained."
  [in out connection f]
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
