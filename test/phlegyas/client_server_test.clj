(ns phlegyas.client-server-test
  (:use [clojure test])
  (:require [aleph.tcp :as tcp]
            [phlegyas.core :as pc]
            [phlegyas.util :refer :all]
            [phlegyas.types :refer :all]
            [phlegyas.client :refer :all]))

(def test-port 10003)

(deftest version-negotiation
  (with-server (tcp/start-server pc/tcp-route {:port test-port :join? false})
    (let [connection (connect "localhost" test-port)]
      (is (= @(:protocol-version connection) protocol-version)))))

(deftest cloning-fid
  (with-server (tcp/start-server pc/tcp-route {:port test-port :join? false})
    (let [connection (connect "localhost" test-port)
          cloned-fid (clone-fid connection 0)]
      (is (= cloned-fid 1)))))

(deftest clone-fid-returns-correct-map
  (with-server (tcp/start-server pc/tcp-route {:port test-port :join? false})
    (let [connection (connect "localhost" test-port)
          fid-info (get @(:mapping connection) 0)
          cloned-fid (clone-fid connection 0)
          cloned-fid-info (get @(:mapping connection) cloned-fid)]
      (is (= fid-info cloned-fid-info)))))

(deftest reading-directory
  (with-server (tcp/start-server pc/tcp-route {:port test-port :join? false})
    (let [connection (connect "localhost" test-port)
          cloned-fid (clone-fid connection 0)
          dir-contents (lsdir connection cloned-fid)]
      (is (seq? dir-contents)))))

(deftest walking-path
  (with-server (tcp/start-server pc/tcp-route {:port test-port :join? false})
    (let [connection (connect "localhost" test-port)
          cloned-fid (clone-fid connection 0)
          dir-entry (first (lsdir connection cloned-fid))
          walked-fid (walk-fid connection 1 [dir-entry])]
      (is (int? walked-fid)))))

(deftest remove-file
  (with-server (tcp/start-server pc/tcp-route {:port test-port :join? false})
    (let [connection (connect "localhost" test-port)
          cloned-fid (clone-fid connection 0)
          dir-entry (first (lsdir connection cloned-fid))
          walked-fid (walk-fid connection 1 [dir-entry])]
      (is (remove-fid connection walked-fid)))))
