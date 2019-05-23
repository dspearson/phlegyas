(ns phlegyas.sqlvfs
  (:require [clojure.java.jdbc :refer :all]
            [phlegyas.vfs :as vfs]))

(def fsdb {:classname   "org.sqlite.JDBC"
           :subprotocol "sqlite"
           :subname     "fs.db"
           })

(defn create-database
  [database]
  (try (db-do-commands database
                       (create-table-ddl :filesystem
                                         [[:aname :text]
                                          [:dev :integer]
                                          [:type :integer]
                                          [:qid_type :integer]
                                          [:qid_vers :integer]
                                          [:qid_path :integer]
                                          [:metadata :text]
                                          [:mode :integer]
                                          [:atime :integer]
                                          [:mtime :integer]
                                          [:length :integer]
                                          [:name :text]
                                          [:size :integer]
                                          [:ssize :integer]
                                          [:uid :text]
                                          [:gid :text]
                                          [:muid :text]
                                          [:children :text]
                                          [:contents :blob]
                                          [:permissions :text]
                                          [:parent :text]]))
       (catch Exception e
         (println (.getMessage e)))))

(defn insert-stat
  [database stat]
  (let [new-stat (-> stat
                     (assoc :aname "")
                     (assoc :qid_vers (:qid-vers stat))
                     (assoc :qid_type (:qid-type stat))
                     (assoc :qid_path (:qid-path stat))
                     (dissoc :qid-path)
                     (dissoc :qid-vers)
                     (dissoc :qid-type)
                     (dissoc :permissions)
                     (dissoc :write-fn)
                     (dissoc :read-fn))]
    (insert! database :filesystem new-stat)))
