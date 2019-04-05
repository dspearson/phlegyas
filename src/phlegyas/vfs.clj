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

;; an example VFS layer

(defrecord stat
    [dev qtype qvers qpath mode atime mtime len name size ssize uid gid muid children contents permissions parent])

(defrecord qid
    [qtype qvers qpath])

(defrecord filesystem
    [files path-pool id root-path])

(defn stat->qid
  [stat]
  (map->qid {:qtype (:qtype stat) :qvers (:qvers stat) :qpath (:qpath stat)}))

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

(defn file->stat
  [file path & {:keys [read-fn parent length] :or {read-fn #'identity parent nil length nil}}]
  (let [fh (io/file file)
        uid (owner fh)
        gid (group fh)
        muid uid
        fname (if (= file "/") "/" (filename fh))
        mtime (modification-time fh)
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
                :atime (access-time fh)
                :mtime mtime
                :len (if (directory? fh) 0 (or length (sizeof fh)))
                :name fname
                :uid uid
                :gid gid
                :muid muid
                :ssize (+ size 2) ;; Rstat has a duplicate stat field, so we add this to aid with serialisation
                :size size
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

(defn synthetic-file
  [path filename owner group mode contents read-fn len-fn]
  (let [size (stat-size filename owner group owner)]
    (map->stat {:qtype (:qtfile qt-mode)
                :qvers 0
                :qpath path
                :permissions {:owner #{:read}, :group #{:read}, :others #{:read}}
                :type 0
                :dev 0
                :mode mode
                :atime 0
                :mtime 0
                :len len-fn
                :custom-data-field contents
                :name filename
                :uid owner
                :gid group
                :muid owner
                :ssize (+ size 2) ;; Rstat has a duplicate stat field, so we add this to aid with serialisation
                :size size
                :children #{}
                :parent 0
                :contents read-fn})))

(defn example-filesystem!
  []
  (let [id (keyword (gensym "fs"))
        path-pool (atom 0)
        root-path @path-pool
        file-path (swap! path-pool inc)
        another-file-path (swap! path-pool inc)
        read-fn (fn [x] (.getBytes (:custom-data-field (:stat x)) "UTF-8"))
        example-file (synthetic-file file-path "synthetic-file" "root" "root" 0444 "hello, world!" read-fn (sizeof-string "hello, world!"))
        another-example-file (synthetic-file another-file-path "current-time" "root" "root" 0444 ""
                                             (fn [x] (.getBytes (str (quot (System/currentTimeMillis) 1000)) "UTF-8"))
                                             (sizeof-string (str (quot (System/currentTimeMillis) 1000))))
        root-dir (root-dir root-path)]
    (-> (map->filesystem {:files {root-path root-dir} :path-pool path-pool :id id :root-path root-path})
        (insert-file! file-path example-file)
        (insert-file! another-file-path another-example-file)
        (update-children! root-path file-path)
        (update-children! root-path another-file-path))))

(defn stat-file
  [fs path]
  (let [f (get (:files fs) path)
        stat (into {:frame :stat} f)]
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
  (get (:role conn) (get (:mapping conn) fid)))

(defn stat-type
  [stat]
  ((keywordize (:qtype stat)) reverse-qt-mode))

(defn fid->mapping
  [fid conn]
  (get (:mapping conn) fid))
