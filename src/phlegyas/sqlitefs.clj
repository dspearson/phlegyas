(ns phlegyas.sqlitefs
  (:require [phlegyas.db :as db]
            [phlegyas.util :refer [uuid!]]
            [next.jdbc :as jdbc]))

(defn directory!
  "Creates a directory"
  [{:keys [parent-id name handler]
    :or {handler "directory-handler"}}]
  (let [uuid (uuid!)]
    {:uuid uuid
     :name name
     :parent-id (or parent-id uuid)
     :handler handler
     :type "directory"}))

(defn create-filesystem
  "Create a 9P filesystem, stores it in the database"
  [system fs-name]
  (let [root-node (directory! {:name "/"})
        root-node-id (db/insert-node root-node)]
    (jdbc/with-transaction [tx (:phlegyas/database @system)]
      (db/insert-node tx root-node)
      (db/insert-filesystem tx {:name fs-name :root-node (:id root-node)}))))

(defn calculate-block-info
  "Given an offset and a block size, calculate the block index and the
   position in the block."
  [offset block-size]
  (let [block-index (quot offset block-size)
        position-in-block (mod offset block-size)]
    {:block-index block-index, :position-in-block position-in-block}))
