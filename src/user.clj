(ns user
  (:require [phlegyas.components :refer [start-system! stop-system!]]
            [phlegyas.db :as db]
            [phlegyas.sqlitefs :as fs]
            [phlegyas.system :refer [system]]))

(comment

  (println @system)

  (start-system!)

  (stop-system!)

  (def test-fs (fs/create-filesystem system "test"))

  (def directory (fs/add-directory system test-fs "test"))
  (println directory)
  (db/get-node (:phlegyas/database @system) directory)

  (db/delete-and-recreate-database (:phlegyas/database @system))
  )
