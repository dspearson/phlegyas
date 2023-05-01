(ns phlegyas.db
  (:require
   [clojure.java.io :refer [file]]
   [hugsql.core :as hugsql]
   [taoensso.timbre :as timbre
    :refer [info debug error]]))

(def tables
  "Set of tables to load from SQL definition files"
  [:filesystems
   :nodes
   :blocks])

(def tables-loaded
  "Set of tables that have successfully had their SQL definitions loaded into HugSQL functions."
  (into #{} (map #(let [sql (str "phlegyas/sql/" (name %) ".sql")] (hugsql/def-db-fns sql) %) tables)))

(defmacro create-all-tables
  "Create database tables. Expects each table listed in the `tables` set
  to have a corresponding create-`table`-table function defined."
  [x]
  `(run! #((ns-resolve (find-ns 'phlegyas.db) (-> (str "create-" (name %) "-table") symbol)) ~x) ~tables))

(defn initialise-database
  [database-spec]
  (try
    (create-all-tables database-spec)
    ;; (create-all-indexes database-spec)
    (debug "Database created.")
    (catch Exception e (error "Unable to create database:" e))))