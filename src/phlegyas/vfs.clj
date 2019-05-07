(ns phlegyas.vfs
  (:require [clojure.core.async :as async]
            [clojure.java.io :as io]
            [phlegyas.types :refer :all]
            [phlegyas.util :refer :all]
            [clojure.string :as string]
            [clojure.set :as sets]
            [primitive-math :as math
             :refer [int->uint short->ushort
                     uint->int ushort->short
                     long->ulong ulong->long]]
            [taoensso.timbre :as log])
  (:import [java.nio.file Files LinkOption]
           [java.nio.file.attribute BasicFileAttributes PosixFilePermission PosixFilePermissions PosixFileAttributes]))

;; an example VFS layer. currently a mess, needs cleanup.

(set! *warn-on-reflection* true)

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
  "Get the PosixFileAttributes of a file."
  [^java.io.File fh]
  (Files/readAttributes (.toPath fh)
                        PosixFileAttributes
                        ^"[Ljava.nio.file.LinkOption;" (into-array [LinkOption/NOFOLLOW_LINKS])))

(defn modification-time
  "Get the last modified time of a file."
  [^java.io.File fh]
  (-> fh .lastModified (/ 1000) int))

(defn access-time
  "Get the last access time of a file."
  [^java.io.File fh]
  (-> fh ^sun.nio.fs.UnixFileAttributes attrs .lastAccessTime .toMillis (/ 1000) int))

(defn octal-mode
  "Get the octal mode permissions of a file."
  [^java.io.File fh]
  (apply + (for [x (-> fh .toPath (Files/getPosixFilePermissions (into-array [LinkOption/NOFOLLOW_LINKS])))]
             ((keywordize x) java-permission-mode))))

(defn permission-set
  "Get the set of permissions for a file."
  [^java.io.File fh]
  (let [permissions (for [x (-> fh .toPath (Files/getPosixFilePermissions (into-array [LinkOption/NOFOLLOW_LINKS])))]
                      (string/lower-case (str x)))
        permission-map (for [x ["owner" "group" "others"]]
                         {(keyword x) (set (map (fn [x] (keyword (second (string/split x #"_"))))
                                                (filter #(string/starts-with? % x) permissions)))})]
    (into {} permission-map)))

(defn owner
  "Get the owner of a file."
  [^java.io.File fh]
  (-> fh ^sun.nio.fs.UnixFileAttributes attrs .owner .getName))

(defn group
  "Get the group of a file."
  [^java.io.File fh]
  (-> fh ^sun.nio.fs.UnixFileAttributes attrs .group .getName))

(defn directory?
  "Is the file a directory?"
  [^java.io.File fh]
  (-> fh ^sun.nio.fs.UnixFileAttributes attrs .isDirectory))

(defn symbolic-link?
  "Is the file a symbolic link?"
  [^java.io.File fh]
  (-> fh ^sun.nio.fs.UnixFileAttributes attrs .isSymbolicLink))

(defn sizeof
  "Get the length of a file."
  [^java.io.File fh]
  (-> fh .length))

(defn filename
  "Get the name of the file."
  [^java.io.File fh]
  (-> fh .getName))

(defn stat-size
  "Calculate the size of a stat reply."
  [fname uid gid muid]
  (+ 2 4 13 4 4 4 8 2 2 2 2
     (sizeof-string fname)
     (sizeof-string uid)
     (sizeof-string gid)
     (sizeof-string muid)))

(defn path->stat
  "Given a filesystem and a path, return the stat of the file."
  [fs path]
  (get (:files fs) path))

(defn fid->mapping
  "Get the mapping corresponding to an allocated fid."
  [conn fid]
  (get (:mapping conn) (keywordize fid)))

(defn file->stat
  "Given a filesystem path, and a 9P path number, create a stat for it. Optional keyword
  arguments `read-fn` is a function that is called upon reads, `parent` is the path of
  the directory the file belongs to, and `length` can be used to manually set file size."
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
                :qid-vers (if (= file "/") 0 (hash mtime))
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
                :children {}
                :parent (if (nil? parent) (keywordize path) parent)
                :read-fn read-fn})))

(defn fetch-data
  "Helper function for calling `read-fn` on a stat."
  [connection frame stat]
  (let [read-fn (:read-fn stat)]
    (read-fn :connection connection :frame frame :stat stat)))

(defn read-dir
  "Given a filesystem and a stat, get all children stats of provided stat."
  [fs stat]
  (let [paths (:children stat)]
    (for [path paths]
      (path->stat fs stat))))

(defn root-dir
  "/ stat on a filesystem."
  [path]
  (file->stat "/" path :read-fn #'identity))

(defn next-available-path
  "Get the next value from the atomic counter."
  [fs]
  (ulong->long (swap! (:path-pool fs) inc)))

(defn update-children
  "Takes a filesystem, a path, a lookup key (filename, hashed), and the child path.
  Updates the stat corresponding to path on fs by adding the child."
  [fs path keyname child]
  (update-in fs [:files path :children] (fn [x] (assoc x keyname child))))

(defn insert-file
  "Given a filesystem, and a path, inserts the stat to the filesystem and updates the
  provided parent path list of children to include the newly inserted stat."
  [fs parent stat]
  (let [files (:files fs)
        path (or (:qid-path stat) (next-available-path fs))]
    (-> fs
        (assoc-in [:files (keywordize path)] (assoc stat :parent parent :qid-path path))
        (update-children parent (keywordize (sha-str (:name stat))) (keywordize path)))))

(defn create-filesystem
  "Create a filesystem record."
  []
  (let [path-pool (atom 0)
        root-dir (root-dir 0)]
    (map->filesystem {:files {:0 root-dir} :path-pool path-pool :id (keyword (gensym "filesystem_")) :root-path :0})))

(defn synthetic-file
  "Create a synthetic file stat."
  [filename owner group mode read-fn write-fn metadata append]
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
                :write-fn write-fn
                :read-fn read-fn})))

(defn-frame-binding example-function-for-files
  [& {:keys [connection frame stat]}]
  (if (> frame-offset 0)
    (byte-array 0)
    (.getBytes "hello, world!\n" "UTF-8")))

;; probably should ditch this for `synthetic-file`
(defn create-synthetic-file
  [filename function-call & {:keys [owner group mode metadata append write-fn]
                             :or {owner "root"
                                  group "root"
                                  mode 0400
                                  write-fn function-call
                                  append false}}]
  (synthetic-file filename owner group mode function-call write-fn metadata append))

(defn fid->stat
  "Get the stat corresponding to the fid in the current state of the connection."
  [state fid]
  (let [mapping (fid->mapping state fid)
        path (:path mapping)
        fs ((:filesystem mapping) (:fs-map state))]
    (get (:files fs) path)))

(defmacro fid->fsname
  "Get the filesystem id that a fid belongs to."
  [state fid]
  `(let [mapping# (fid->mapping ~state ~fid)]
     (:filesystem mapping#)))

(defn update-stat
  "Update the stat associated with fid by adding data to it."
  [state fid data]
  (let [fs-name (fid->fsname state fid)
        stat (into (fid->stat state fid) data)]
    (update-in state [:fs-map fs-name :files (keywordize (:qid-path stat))] (fn [x] (into x stat)))))

(defn-frame-binding example-read-write
  [& {:keys [connection frame stat]}]
  (let [count-bytes (count frame-data)
        contents (:contents (:metadata stat))]
    (if (= frame-ftype :Twrite)
      (do
        (swap! state (fn [x] (update-stat x frame-fid {:metadata {:contents (conj contents frame-data)}
                                                      :length (+ (:length stat) count-bytes)})))
        (uint->int count-bytes))
      (-> contents flatten pack))))

(defn-frame-binding print-current-time
  [& {:keys [connection frame stat]}]
  (let [current-time (quot (System/currentTimeMillis) 1000)
        current-time-bytes (.getBytes (str current-time "\n") "UTF-8")
        file-time (:time (:metadata stat))]
    (swap! state (fn [x] (update-stat x frame-fid {:metadata {:time current-time}
                                                  :atime current-time
                                                  :mtime current-time
                                                  :qid-vers (int (hash current-time))
                                                  :length (ulong->long (+ 1 frame-offset (count current-time-bytes)))})))

    (if (or (= frame-offset 0) (not= current-time file-time))
      current-time-bytes
      (byte-array 0))))

(defn example-filesystem!
  []
  (let [root-fs (create-filesystem)
        root-path :0
        another-example-file (create-synthetic-file "current-time" #'print-current-time :metadata {:time 0} :append true)]
    (-> root-fs
        (insert-file root-path (create-synthetic-file "write-to-me" #'example-read-write :write-fn #'example-read-write))
        (insert-file root-path (create-synthetic-file "example-file" #'example-function-for-files))
        (insert-file root-path another-example-file))))

(defn add-fs
  "Add the given filesystem to the connection. Used during attach."
  [state fs]
  (assoc-in state [:fs-map (:id fs)] fs))

(defn add-mapping
  "Add a new fid to the connection. Takes in the current state, new fid, filesystem id, and path of the fid."
  [state fid fs path]
  (assoc-in state [:mapping (keywordize fid)] {:filesystem fs :path path :offset 0}))

(defn update-mapping
  "Update the mapping for fid with the provided data."
  [state fid data]
  (update-in state [:mapping (keywordize fid)] (fn [x] (into x data))))

(defn add-fid
  "Add a new fid to the fid map, along with the tag id of the request responsible."
  [state fid tag]
  (assoc-in state [:fids (keywordize fid)] {:added-by tag}))

(defn add-role
  "Add the user/group that issued the attach to the role map."
  [state fsid uid gid]
  (assoc-in state [:role fsid] {:uid uid :gid gid}))

(defn path->qid
  "Given a filesystem and a path, return the qid for the path."
  [fs path]
  (-> (path->stat fs path) stat->qid))

(defn wname->path
  "Given a filesystem, a path, and a wname (file name), look through the children
  of the stat for the given path and return the path associated with that name.
  We hash the name of the file here, and keywordize it, to prevent issues with
  keywordizing arbitrary strings."
  [fs path wname]
  (if (= wname "..")
    (:parent (get (:files fs) path))
    (get (:children (get (:files fs) path)) (keywordize (sha-str wname)))))

(defn walk-path
  "Given a filesystem, a path, and a vector `wnames`, step through the vector
  attempting to resolve the path of each. i.e. if we are in the root, and want
  to go to /a/b/c, wnames would be [\"a\", \"b\", \"c\"]. If we do not find a
  match for a wname, we return a list of all paths that we _did_ find. In this
  case, a fid is not changed. Walks are only successful if the entire path can
  be walked."
  [fs path wnames]
  (loop [candidates wnames
         search-path path
         paths []]
    (let [candidate (first candidates)
          candidate-path (if candidate (wname->path fs search-path candidate))]
      (cond
        (nil? candidate) paths
        (nil? candidate-path) paths
        :else (recur (rest candidates) candidate-path (conj paths candidate-path))))))

(defn stat->role
  "Given a stat, and a user, find what role we have on it."
  [stat user]
  (cond
    (= user (:uid stat)) :owner
    (= user (:gid stat)) :group
    :else :other))

(defn allowed-op?
  [permissions operation]
  (let [access-level (operation role-access)]
    (sets/subset? access-level permissions)))

(defn role-resolve
  "Given a stat and a role, find what role we have on the stat."
  [stat role]
  (cond
    (= (:uid stat) (:uid role)) :owner
    (= (:gid stat) (:gid role)) :group
    :else :others))

(defn permission-check
  "Given a stat, a role, and an operation we want to perform, see
  if we are allowed to perform it."
  [stat rolemap operation]
  (let [role (role-resolve stat rolemap)
        perms (role (:permissions stat))]
    (allowed-op? perms operation)))

(defn fid->role
  "Given a fid and a connection, return the role associated with it."
  [fid conn]
  (get (:role conn) (get (:mapping conn) (keywordize fid))))

(defn stat-type
  "Get the type of a stat."
  [stat]
  ((keywordize (:qid-type stat)) reverse-qt-mode))

(defn directory-reader
  "Takes a filesystem, a list of paths, and the requested byte count.
  Recursively fetches the stat entries for the given paths, packing them
  into the correct byte format. Returns when either the paths are exhausted,
  or we reached the byte count. Returns a vector of 2 values, the left is the
  data, the right is the paths that we could not walk due to size limitations.
  This can be then consulted on subsequent directory reads."
  [fs paths max-size]
  (let [layout (subvec (:Rstat frame-layouts) 2)]
    (loop [accum '()
           last-path nil
           data-size 0
           paths-remaining (keys paths)]
      (cond
        (> data-size max-size)
        [(-> accum rest flatten pack)
         (if last-path
           (conj paths-remaining last-path)
           paths-remaining)]

        (empty? paths-remaining)
        [(-> accum flatten pack)
         paths-remaining]

        :else
        (let [stat (path->stat fs (get paths (first paths-remaining)))
              data (for [typ layout] ((get put-operation typ) (get stat typ)))]
          (recur (conj accum data)
                 (:qid-path stat)
                 (+ data-size (count data))
                 (rest paths-remaining)))))))
