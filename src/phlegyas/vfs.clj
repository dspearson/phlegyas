(ns phlegyas.vfs
  (:require [clojure.core.async :as async]
            [clojure.java.io :as io]
            [phlegyas.types :refer :all]
            [phlegyas.util :refer :all]
            [phlegyas.transformers :refer :all]
            [clojure.string :as string]
            [clojure.set :as set]
            [primitive-math :as math
             :refer [int->uint short->ushort
                     uint->int ushort->short
                     long->ulong ulong->long]]
            [taoensso.timbre :as log])
  (:import [java.nio.file Files LinkOption]
           [java.nio.file.attribute BasicFileAttributes PosixFilePermission PosixFilePermissions PosixFileAttributes]))

;; an example VFS layer

(defrecord stat
    [dev qid-type qid-vers qid-path mode atime mtime length name size ssize uid gid muid children contents permissions parent])

(defrecord qid
    [qid-type qid-vers qid-path])

(defrecord filesystem
    [files path-pool id root-path])

(defn stat->qid
  [stat]
  (map->qid {:qid-type (:qid-type stat) :qid-vers (:qid-vers stat) :qid-path (:qid-path stat)}))

(defn version
  [stat]
  (hash (:mtime stat)))

(defn attrs
  [fh]
  (-> fh .toPath (Files/readAttributes PosixFileAttributes (into-array [LinkOption/NOFOLLOW_LINKS]))))

(defn modification-time
  [fh]
  (-> fh .lastModified (/ 1000) int))

(defn access-time
  [fh]
  (-> fh attrs .lastAccessTime .toMillis (/ 1000) int))

(defn octal-mode
  [fh]
  (apply + (for [x (-> fh .toPath (Files/getPosixFilePermissions (into-array [LinkOption/NOFOLLOW_LINKS])))]
             ((keywordize x) java-permission-mode))))

(defn permission-set
  [fh]
  (let [permissions (for [x (-> fh .toPath (Files/getPosixFilePermissions (into-array [LinkOption/NOFOLLOW_LINKS])))]
                      (string/lower-case (str x)))
        permission-map (for [x ["owner" "group" "others"]]
                               {(keyword x) (set (map (fn [x] (keyword (second (string/split x #"_"))))
                                              (filter #(string/starts-with? % x) permissions)))})]
        (into {} permission-map)))

(defn owner
  [fh]
  (-> fh attrs .owner .getName))

(defn group
  [fh]
  (-> fh attrs .group .getName))

(defn directory?
  [fh]
  (-> fh attrs .isDirectory))

(defn symbolic-link?
  [fh]
  (-> fh attrs .isSymbolicLink))

(defn sizeof
  [fh]
  (-> fh .length))

(defn filename
  [fh]
  (-> fh .getName))

(defn stat-size
  [fname uid gid muid]
  (+ 2 4 13 4 4 4 8 2 2 2 2
     (sizeof-string fname)
     (sizeof-string uid)
     (sizeof-string gid)
     (sizeof-string muid)))

(defn path->stat
  [fs path]
  (get (:files fs) path))

(defn fid->mapping
  [conn fid]
  (get (:mapping conn) fid))

(defn file->stat
  [file path & {:keys [read-fn parent length] :or {read-fn #'identity parent nil length nil}}]
  (let [fh (io/file file)
        uid (owner fh)
        gid (group fh)
        muid uid
        fname (if (= file "/") "/" (filename fh))
        mtime (modification-time fh)
        ftyp (if (directory? fh) (:dir qt-mode) (:file qt-mode))
        size (stat-size fname uid gid muid)]
    (map->stat {:qid-type ftyp
                :qid-vers (hash mtime)
                :qid-path path
                :permissions (permission-set fh)
                :type 0
                :dev 0
                :absolute-path (.getAbsolutePath fh)
                :mode (bit-or (octal-mode fh) ftyp)
                :atime (access-time fh)
                :mtime mtime
                :length (if (directory? fh) 0 (or length (sizeof fh)))
                :name fname
                :uid uid
                :gid gid
                :muid muid
                :ssize (+ size 2) ;; Rstat has a duplicate stat field, so we add this to aid with serialisation
                :size size
                :children #{}
                :parent (if (nil? parent) path parent)
                :contents read-fn})))

(defn read-dir
  [fs stat]
  (let [paths (:children stat)]
    (for [path paths]
      (path->stat fs stat))))

(defn root-dir
  [path]
  (file->stat "/" path :read-fn #'identity))

(defn update-children!
  [fs path child]
  (let [stat (path->stat fs path)
        children (:children stat)
        updated-stat (assoc stat :children (conj children child))
        updated-files (assoc (:files fs) path updated-stat)]
    (assoc fs :files updated-files)))

(defn next-available-path
  [fs]
  (ulong->long (swap! (:path-pool fs) inc)))

(defn insert-file!
  [fs parent stat]
  (let [files (:files fs)
        path (or (:qid-path stat) (next-available-path fs))]
    (-> fs
        (assoc :files (assoc files path (assoc stat :parent parent :qid-path path)))
        (update-children! parent path))))

(defn create-filesystem
  []
  (let [path-pool (atom 0)
        root-dir (root-dir 0)]
    (map->filesystem {:files {0 root-dir} :path-pool path-pool :id (keyword (gensym "filesystem_")) :root-path 0})))

(defn synthetic-file
  [filename owner group mode read-fn metadata append]
  (let [size (stat-size filename owner group owner)]
    (map->stat {:qid-type (if append (:append qt-mode) (:file qt-mode))
                :qid-vers 0
                :permissions {:owner #{:read}, :group #{:read}, :others #{:read}}
                :type 0
                :dev 0
                :mode mode
                :atime 0
                :mtime 0
                :length 0
                :name filename
                :uid owner
                :gid group
                :muid owner
                :ssize (+ size 2) ;; Rstat has a duplicate stat field, so we add this to aid with serialisation
                :size size
                :children #{}
                :metadata metadata
                :read-fn read-fn})))

(defn example-function-for-files
  [stat frame state]
  (if (> (:offset frame) 0)
    (byte-array 0)
    (.getBytes "hello, world!\n" "UTF-8")))

(defn create-synthetic-file
  [filename function-call & {:keys [owner group mode metadata append]
                             :or {owner "root"
                                  group "root"
                                  mode 0400
                                  additional-data nil
                                  append false}}]
  (synthetic-file filename owner group mode function-call metadata append))

(defn fid->stat
  [state fid]
  (let [mapping (fid->mapping state fid)
        path (:path mapping)
        fs ((:filesystem mapping) (:fs-map state))]
    (get (:files fs) path)))

(defmacro fid->fsname
  [state fid]
  `(let [mapping# (fid->mapping ~state ~fid)]
     (:filesystem mapping#)))

(defn update-stat
  [state fid data]
  (let [fs-name (fid->fsname state fid)
        stat (into (fid->stat state fid) data)]
    (update-in state [:fs-map fs-name :files] (fn [x] (assoc x (:qid-path stat) stat)))))

(defn update-mapping
  [state fid data]
  (update-in state [:mapping fid] (fn [x] (into x data))))

(defn print-current-time
  [stat frame state]
  (let [fid (:fid frame)
        current-time (str (quot (System/currentTimeMillis) 1000))
        current-time-bytes (.getBytes (str current-time "\n") "UTF-8")
        file-time (:time (:metadata stat))]
    (swap! state (fn [x] (update-stat x fid {:length (+ 1 (:length stat) (count current-time-bytes))})))
    (if (= current-time file-time)
      (byte-array 0)
      (do
        (swap! state (fn [x] (update-stat x fid {:metadata {:time current-time}
                                                :qid-vers (int (hash current-time))})))
        current-time-bytes))))

(defn example-filesystem!
  []
  (let [root-fs (create-filesystem)
        root-path (:root-path root-fs)
        another-example-file (create-synthetic-file "current-time" #'print-current-time :metadata {:time 0} :append true)]
    (-> root-fs
        (insert-file! root-path (create-synthetic-file "example-file" #'example-function-for-files))
        (insert-file! root-path another-example-file))))

(defn stat-file
  [fs path]
  (let [f (get (:files fs) path)
        stat (into {:frame :stat} f)]
    (into {:frame :stat} stat)))

(defn stat->data
  [stat]
  ((:contents stat) stat))

(defn add-fs
  [state fs]
  (assoc state :fs-map (assoc (:fs-map state) (:id fs) fs)))

(defn add-mapping
  [state fid fs path]
  (assoc state :mapping (assoc (:mapping state) fid {:filesystem fs :path path :offset 0})))

(defn update-mapping
  [state fid data]
  (assoc state :mapping (assoc (:mapping state) fid (into (get (:mapping state) fid) data))))

(defn add-fid
  [state fid]
  (assoc state :fids (set (conj (:fids state) fid))))

(defn add-role
  [state fsid uid gid]
  (assoc state :role (assoc (:role state) fsid {:uid uid :gid gid})))

(defn path->name
  [fs path]
  (:name (get fs path)))

(defn path->qid
  [fs path]
  (-> (path->stat fs path) stat->qid))

(defn wname->path
  [fs path wname]
  (if (= wname "..")
    (:parent (get (:files fs) path))
    (loop [candidates (:children (get (:files fs) path))]
      (let [candidate (first candidates)]
        (cond
          (nil? candidate) nil
          (= (:name (path->stat fs candidate)) wname) candidate
          :else (recur (rest candidates)))))))

(defn walk-path
  [fs path wnames]
  (loop [candidates wnames
         search-path path
         paths []]
    (let [candidate (first candidates)
          candidate-path (wname->path fs search-path candidate)]
      (cond
        (nil? candidate) paths
        (nil? candidate-path) paths
        :else (recur (rest candidates) candidate-path (conj paths candidate-path))))))

(defn stat->role
  [stat user]
  (cond
    (= user (:uid stat)) :owner
    (= user (:gid stat)) :group
    :else :other))

(defn allowed-op?
  [permissions operation]
  (let [access-level (operation role-access)]
    (set/subset? access-level permissions)))

(defn role-resolve
  [stat role]
  (cond
    (= (:uid stat) (:uid role)) :owner
    (= (:gid stat) (:gid role)) :group
    :else :others))

(defn permission-check
  [stat rolemap operation]
  (let [role (role-resolve stat rolemap)
        perms (role (:permissions stat))]
    (allowed-op? perms operation)))

(defn fid->role
  [fid conn]
  (get (:role conn) (get (:mapping conn) fid)))

(defn stat-type
  [stat]
  ((keywordize (:qid-type stat)) reverse-qt-mode))

(defn directory-reader
  [statcoll max-size]
  (let [layout (subvec (:Rstat frame-layouts) 2)]
    (loop [accum '()
           last-path nil
           paths (into #{} (map (fn [x] (:qid-path x)) statcoll))
           stats-left statcoll]
      (cond
        (> (count (flatten accum)) max-size)
        [(-> accum rest flatten pack)
         (if last-path
           (conj paths last-path)
           paths)]

        (empty? stats-left)
        [(-> accum flatten pack)
         paths]

        :else
        (recur (conj accum (transform (first stats-left) layout))
               (:qid-path (first stats-left))
               (disj paths (:qid-path (first stats-left)))
               (rest stats-left))))))
