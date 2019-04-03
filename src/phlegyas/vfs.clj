(ns phlegyas.vfs
  (:require [clojure.core.async :as async]
            [clojure.java.io :as io]
            [phlegyas.types :refer :all]
            [phlegyas.util :refer :all]
            [primitive-math :as math
             :refer [int->uint short->ushort
                     uint->int ushort->short
                     long->ulong ulong->long]]
            [taoensso.timbre :as log])
  (:import [java.nio.file Files LinkOption]
           [java.nio.file.attribute BasicFileAttributes PosixFilePermission PosixFilePermissions PosixFileAttributes]))

(defrecord stat
    [dev qtype qvers qpath mode atime mtime len name uid gid muid children contents permissions parent])

(defrecord qid
    [qtype qvers qpath])

(defn stat->qid
  [stat]
  (let [typ (:qtype stat)
        ver (:qvers stat)
        path (:qpath stat)]
    (map->qid {:qtype typ :qvers ver :qpath path})))

(defn version
  [file]
  (hash (or (:mtime file) ((:handle-version! file)))))

(defrecord filesystem
    [files path-pool])

(defn path-pool!
  []
  (let [channel (async/chan)]
    (async/thread
      (loop [i (long 0)]
        (let [input (async/<!! channel)]
          (if (= input :destroy)
            nil
            (do
              (async/>!! channel i)
              (recur (+ i 1)))))))
    channel))

(defn directory-open!
  [dir]
  dir)

(defn directory-clunk!
  [dir]
  dir)

(defn get-path
  [path-pool]
  (async/>!! path-pool :new)
  (async/<!! path-pool))

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
             ((keyword (str x)) java-permission-mode))))

(defn permission-set
  [fh]
  (let [permissions (for [x (-> fh .toPath (Files/getPosixFilePermissions (into-array [LinkOption/NOFOLLOW_LINKS])))]
                      (clojure.string/lower-case (str x)))
        permission-map (for [x ["owner" "group" "others"]]
                               {(keyword x) (set (map (fn [x] (keyword (second (clojure.string/split x #"_"))))
                                              (filter #(clojure.string/starts-with? % x) permissions)))})]
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

(defn contents
  [fh]
  (if (directory? fh)
    nil
    (slurp fh)))

(defn filename
  [fh]
  (-> fh .getName))

(defn sizeof-string
  [s]
  (count (.getBytes s "UTF-8")))

(defn stat-size
  [fname uid gid muid]
  (log/debug "In stat-size")
  (+ 2 4 13 4 4 4 8 2 2 2 2
     (sizeof-string fname)
     (sizeof-string uid)
     (sizeof-string gid)
     (sizeof-string muid)))

(defn file->stat
  [file path & {:keys [read-fn parent length] :or {read-fn #'identity parent nil length nil}}]
  (let [fh (io/file file)
        uid (owner fh)
        gid (group fh)
        muid uid
        fname (if (= file "/") "/" (filename fh))
        mtime (uint->int (modification-time fh))
        ftyp (if (directory? fh) (:qtdir qt-mode) (:qtfile qt-mode))
        size (stat-size fname uid gid muid)]
    (map->stat {:qtype ftyp
                :qvers (hash mtime)
                :qpath path
                :permissions (permission-set fh)
                :type 0
                :dev 0
                :absolute-path (.getAbsolutePath fh)
                :mode (bit-or (octal-mode fh) ftyp)
                :atime (uint->int (access-time fh))
                :mtime mtime
                :len (if (directory? fh) 0 (or length (sizeof fh)))
                :name fname
                :uid uid
                :gid gid
                :muid muid
                :size (+ size 2) ;; Rstat has a duplicate stat field, so we add this to aid with serialisation
                :ssize size
                :children #{}
                :parent (if (nil? parent) path parent)
                :contents read-fn})))

(defn path->stat
  [fs path]
  (get (:files fs) path))

(defn read-dir
  [fs stat]
  (let [paths (:children stat)]
    (for [path paths]
      (path->stat fs stat))))

(defn root-dir
  [path]
  (file->stat "/" path :read-fn #'identity))

(defn insert-file!
  [fs path stat]
  (let [files (:files fs)]
    (assoc fs :files (assoc files path stat))))

(defn update-children!
  [fs path child]
  (let [stat (path->stat fs path)
        children (:children stat)
        updated-stat (assoc stat :children (conj children child))
        updated-files (assoc (:files fs) path updated-stat)]
    (assoc fs :files updated-files)))

;; TODO
(defn read-file
  [f]
  f)

(defn read-data-field
  [data]
  (let [stat (:stat data)]
    (.getBytes (:data stat) "UTF-8")))

(defn virtual-file
  [path contents]
  (let [size (stat-size "synthetic-file" "root" "root" "root")]
    (map->stat {:qtype (:qtfile qt-mode)
                :qvers 0
                :qpath path
                :permissions {:owner #{:read :execute :write}, :group #{:read :execute}, :others #{:read :execute}}
                :type 0
                :dev 0
                :mode 0444
                :atime 0
                :mtime 0
                :len (count (.getBytes contents "UTF-8"))
                :data contents
                :name "synthetic-file"
                :uid "root"
                :gid "root"
                :muid "root"
                :size (+ size 2) ;; Rstat has a duplicate stat field, so we add this to aid with serialisation
                :ssize size
                :children #{}
                :parent 0
                :contents #'read-data-field})))

(defn filesystem!
  []
  (let [fs-name (keyword (gensym "fs"))
        path-pool (path-pool!)
        path (get-path path-pool)
        file-path (get-path path-pool)
        test-file (virtual-file file-path "hello, world!")
        root-dir (root-dir path)]
    [fs-name path (-> (map->filesystem {:files {path root-dir} :path-pool path-pool}) (insert-file! file-path test-file) (update-children! path file-path))]))

(defn stat-file
  [fs path]
  (let [f (get (:files fs) path)
        stat (into {:frame :stat} f)]
    (log/debug "Got file:" f)
    (log/debug "Stat:" stat)
    (into {:frame :stat} stat)))

(defn stat->data
  [stat]
  ((:contents stat) stat))

(defn assoc-fid
  [state fid newfid]
  (let [mapping (get (:mapping state) fid)
        fs-name (:fs-name mapping)
        path (:path mapping)]
    {:fids (conj (:fids state) newfid)
     :mapping (assoc (:mapping state) newfid mapping)}))

(defn path->name
  [fs path]
  (:name (get fs path)))

(defn path->stat
  [fs path]
  (get (:files fs) path))

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
    (clojure.set/subset? access-level permissions)))

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
  (let [fs-name (get (:mapping conn) fid)]
    (get (:role conn) fs-name)))

(defn stat-type
  [stat]
  ((keyword (str (:qtype stat))) qt-mode-r))

(defn fid->mapping
  [fid conn]
  (get (:mapping conn) fid))
