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
          fs              (vfs/create-filesystem system fs-name)
          created-fs-name (:name (vfs/get-filesystem system fs-name))]
      (is (uuid= fs-name created-fs-name)))))

(deftest filesystem-with-nested-directory
  (with-system
    (let [fs-name            (uuid!)
          fs                 (vfs/create-filesystem system fs-name)
          root-directory     (vfs/get-node system {:qid-path (:rnode fs)})
          sub-directory-1    (vfs/add-directory system root-directory "sub-directory-1")
          sub-directory-2    (vfs/add-directory system root-directory "sub-directory-2")
          fetched-directory  (vfs/get-node system sub-directory-1)
          directory-contents (vfs/get-directory-contents system root-directory)
          _                  (println (first directory-contents))
          dir                (vfs/directory-reader system root-directory 8192)]
      ;; FIXME: ^ dir read fails, run in non test context to debug

      (testing "Directory is successfully inserted and retrievable"
        (is (uuid= (:path sub-directory-1)
                   (:path fetched-directory))))

      (testing "Directory contents are enumerable"
        (is (= 2 (count directory-contents)))))))
