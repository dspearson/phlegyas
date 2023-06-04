(ns phlegyas.sqlitefs-test
  (:require [phlegyas.sqlitefs :as vfs]
            [phlegyas.components :as c]
            [phlegyas.db :refer [delete-and-recreate-database]]
            [phlegyas.util :refer [uuid! uuid=]]
            [clojure.test :refer [is deftest testing]]))

(defonce system (atom nil))

(defmacro with-system
  [& body]
  `(try
     (let [system# (c/start-system! ~'system "test_system.edn")]
       (delete-and-recreate-database (:phlegyas/database system#))
       ~@body)
     (finally
       (c/stop-system! ~'system))))

(deftest new-filesystem
  (with-system
    (let [fs-name         (uuid!)
          _               (vfs/create-filesystem system fs-name)
          created-fs-name (:name (vfs/get-filesystem system fs-name))]
      (is (uuid= fs-name created-fs-name)))))

(deftest filesystem-with-nested-directory
  (with-system
    (let [fs-name           (uuid!)
          fs                (vfs/create-filesystem system fs-name)
          root-directory    (vfs/get-node system {:qid-path (:rnode fs)})
          sub-directory-1   (vfs/add-directory system root-directory "sub-directory-1")
          _                 (vfs/add-directory system sub-directory-1 "sub-directory-2")
          fetched-directory (vfs/get-node system sub-directory-1)
          dir-contents      (vfs/get-directory-contents system root-directory)
          subdir-contents   (vfs/get-directory-contents system root-directory)
          _                 (println (first dir-contents))
          dir               (vfs/directory-reader system root-directory 8192)]
      (println "Dir:" dir)
      (testing "Directory is successfully inserted and retrievable"
        (is (uuid= (:qid-path sub-directory-1)
                   (:qid-path fetched-directory))))

      (testing "Directory contents are enumerable"
        (is (= 1 (count dir-contents))))

      (testing "Directories can be nested"
        (is (= 1 (count subdir-contents)))))))
