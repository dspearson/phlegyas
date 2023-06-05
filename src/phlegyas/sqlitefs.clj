(ns phlegyas.sqlitefs
  (:require [phlegyas.db :as db]
            [clojure.set :refer [rename-keys subset?]]
            [phlegyas.util :refer [uuid! epoch! octal->int pack sizeof-string keywordize]]
            [phlegyas.system :refer [system]]
            [phlegyas.types :refer [qt-mode frame-layouts put-operation role-access reverse-qt-mode]]
            [taoensso.timbre :as timbre
             :refer [info debug error]]
            [next.jdbc :as jdbc]))

(defn directory!
  "Creates a directory"
  [{:keys [parent name handler uid gid]
    :or   {handler "directory-handler"
           uid     "root"
           gid     "root"}}]
  (let [uuid  (uuid!)
        birth (epoch!)]
    {:qid-path uuid
     :dev      0
     :handler  handler
     :qid-type (:dir qt-mode)
     :qid-vers 0
     :name     name
     :parent   (or parent uuid)
     :atime    birth
     :mtime    birth
     :mode     (octal->int "0755")
     :length   0
     :uid      uid
     :gid      gid
     :muid     uid}))

(defn filesystem!
  "Creates a filesystem"
  [{:keys [name bsize rnode]
    :or {bsize 8192}}]
  (let [uuid (uuid!)]
    {:uuid uuid
     :name name
     :bsize bsize
     :rnode rnode}))

(defn create-filesystem
  "Create a 9P filesystem, stores it in the database"
  [system fs-name & {:keys [bsize] :or {bsize 8192}}]
  (let [root-node (directory! {:name "/"})
        fs        (filesystem! {:name fs-name :bsize bsize :rnode (:qid-path root-node)})]
    (info "System db:" (:phlegyas/database @system))
    (jdbc/with-transaction [tx (:ds (:phlegyas/database @system))]
      (info "Inserting:" root-node)
      (db/insert-node tx root-node)
      (db/insert-filesystem tx fs))
    fs))

(defn get-filesystem
  "Get a filesystem from the database"
  [system fs-name]
  (jdbc/with-transaction [tx (:phlegyas/database @system)]
    (let [fs        (db/get-filesystem tx {:name fs-name})
          root-node (db/get-node tx {:qid-path (:rnode fs)})]
      (assoc fs :root root-node))))

(defn add-directory
  "Add a directory to the filesystem"
  [system {:keys [qid-path]} dir-name]
  (let [directory (directory! {:name dir-name :parent qid-path})]
    (when-not
        (empty?
          (jdbc/with-transaction [tx (:phlegyas/database @system)]
            (db/insert-node tx directory)))
      directory)))

(defn stat-size
  "Calculate the size of a stat reply."
  [{:keys [name uid gid muid]}]
  (+ 2 4 13 4 4 4 8 2 2 2 2
     (sizeof-string name)
     (sizeof-string uid)
     (sizeof-string gid)
     (sizeof-string muid)))

(defn normalise-db-stat
  [db-stat]
  (let [stat (rename-keys db-stat {:path :qid-path
                                   :vers :qid-vers
                                   :type :qid-type})]
    (assoc stat
           :size (stat-size stat)
           :type 0)))

(defn get-node
  "Fetches a node's inode from the database"
  [system node]
  (normalise-db-stat (db/get-node (:phlegyas/database @system) node)))

(defn get-directory-contents
  [system node]
  (map normalise-db-stat (db/get-children (:phlegyas/database @system) node)))

(defn directory-reader
  "Takes a filesystem, a list of paths, and the requested byte count.
  Recursively fetches the stat entries for the given paths, packing them
  into the correct byte format. Returns when either the paths are exhausted,
  or we reached the byte count. Returns a vector of 2 values, the left is the
  data, the right is the paths that we could not walk due to size limitations.
  This can be then consulted on subsequent directory reads."
  [system directory max-size]
  (let [layout (subvec (:Rstat frame-layouts) 2)]
    (loop [accum           '()
           last-path       nil
           data-size       0
           paths-remaining (get-directory-contents system directory)]
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
        (let [stat (first paths-remaining)
              data (map #((% put-operation) (% stat)) layout)]
          (recur (conj accum data)
                 (:qid-path stat)
                 (+ data-size (count data))
                 (rest paths-remaining)))))))

(defn calculate-block-info
  "Given an offset and a block size, calculate the block index and the
   position in the block."
  [offset block-size]
  {:block-index       (quot offset block-size)
   :position-in-block (mod offset block-size)})

(defn walk-path
  "Given a filesystem, a path, and a vector `wnames`, step through the vector
  attempting to resolve the path of each. i.e. if we are in the root, and want
  to go to /a/b/c, wnames would be [\"a\", \"b\", \"c\"]. If we do not find a
  match for a wname, we return a list of all paths that we _did_ find. In this
  case, a fid is not changed. Walks are only successful if the entire path can
  be walked."
  [path wnames]
  (let [paths (reduce (fn [acc wname]
                        (let [candidate-path (db/get-child (:phlegyas/database @system) {:qid-path path :name wname})]
                          (if (nil? candidate-path)
                            (reduced acc)
                            (conj acc candidate-path))))
                      [path]
                      wnames)]
    (if (= (count paths) (inc (count wnames)))
      paths
      (vec (butlast paths)))))

(defn fid->role
  "Given a fid and a connection, return the role associated with it."
  [fid {:keys [mapping role]}]
  (-> fid
      keywordize
      mapping
      role))

(defn role-resolve
  "Given a stat and a role, find what role we have on the stat."
  [stat role]
  (cond
    (= (:uid stat) (:uid role)) :owner
    (= (:gid stat) (:gid role)) :group
    :else                       :others))

(defn allowed-op?
  [permissions operation]
  (let [access-level (operation role-access)]
    (subset? access-level permissions)))

(defn permission-check
  "Given a stat, a role, and an operation we want to perform, see
  if we are allowed to perform it."
  [stat rolemap operation]
  (let [role  (role-resolve stat rolemap)
        perms (role (:permissions stat))]
    (allowed-op? perms operation)))

(defn stat-type
  "Get the type of a stat."
  [stat]
  (-> stat :qid-type keywordize reverse-qt-mode))
