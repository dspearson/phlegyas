(ns phlegyas.sqlitefs
  (:require [phlegyas.db :as db]
            [phlegyas.util :refer [uuid!]]
            [phlegyas.system :refer [system]]
            [taoensso.timbre :as timbre
             :refer [info debug error]]
            [next.jdbc :as jdbc]))

(defn directory!
  "Creates a directory"
  [{:keys [parent name handler]
    :or {handler "directory-handler"}}]
  (let [uuid (uuid!)]
    {:uuid uuid
     :name name
     :parent (or parent uuid)
     :handler handler
     :type "directory"}))

(defn create-filesystem
  "Create a 9P filesystem, stores it in the database"
  [system fs-name]
  (let [root-node (directory! {:name "/"})]
    (info "System db:" (:phlegyas/database @system))
    (jdbc/with-transaction [tx (:ds (:phlegyas/database @system))]
      (info "Inserting:" root-node)
      (db/insert-node tx root-node)
      (db/insert-filesystem tx {:name fs-name :root-node (:uuid root-node)}))))

(defn get-filesystem
  "Get a filesystem from the database"
  [system fs-name]
  (jdbc/with-transaction [tx (:phlegyas/database @system)]
    (let [fs        (db/get-filesystem tx fs-name)
          root-node (db/get-node tx (:root fs))]
      (assoc fs :root root-node))))

(defn calculate-block-info
  "Given an offset and a block size, calculate the block index and the
   position in the block."
  [offset block-size]
  (let [block-index (quot offset block-size)
        position-in-block (mod offset block-size)]
    {:block-index block-index, :position-in-block position-in-block}))
