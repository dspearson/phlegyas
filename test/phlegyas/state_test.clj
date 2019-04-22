(ns phlegyas.state-test
  (:use [clojure test])
  (:require [clojure.java.io :as io]
            [aleph.tcp :as tcp]
            [manifold.stream :as s]
            [phlegyas.core :as pc]
            [phlegyas.util :refer :all]
            [phlegyas.types :refer :all]
            [phlegyas.frames :refer :all]))

(defn dispatch
  [packet client]
  (s/put! client packet)
  @(s/take! client))

(def test-port 10002)

(def Tversion-frame {:frame :Tversion :tag notag :msize max-message-size :version protocol-version})
(def Tversion-reply {:frame :Rversion :tag 65535 :msize 2147483647 :version "9P2000"})

(def Tattach-frame {:frame :Tattach :tag 0 :fid 0 :afid nofid :uname "dsp" :aname "dsp"})
(def Tattach-reply {:frame :Rattach :tag 0 :qid-type -128 :qid-vers 0 :qid-path 0})

(def Twalk-frame {:frame :Twalk :tag 0 :fid 0 :newfid 1 :wnames []})
(def Twalk-reply {:frame :Rwalk :tag 0 :nwqids []})

(def Tclunk-frame {:frame :Tclunk :tag 0 :fid 1})
(def Tclunk-reply {:frame :Rclunk :tag 0})

(deftest version-negotiation (with-server (tcp/start-server pc/tcp-route {:port test-port :join? false})
                               (let [c @(tcp/client {:host "localhost" :port test-port})]
                                 (is (= Tversion-reply (-> Tversion-frame assemble-packet (dispatch c) disassemble-packet))))))

(deftest attach-filesystem (with-server (tcp/start-server pc/tcp-route {:port test-port :join? false})
                             (let [c @(tcp/client {:host "localhost" :port test-port})]
                               (-> Tversion-frame assemble-packet (dispatch c))
                               (is (= Tattach-reply (-> Tattach-frame assemble-packet (dispatch c) disassemble-packet))))))

(deftest walk-new-fid (with-server (tcp/start-server pc/tcp-route {:port test-port :join? false})
                        (let [c @(tcp/client {:host "localhost" :port test-port})]
                          (-> Tversion-frame assemble-packet (dispatch c))
                          (-> Tattach-frame assemble-packet (dispatch c))
                          (is (= Twalk-reply (-> Twalk-frame assemble-packet (dispatch c) disassemble-packet))))))

(deftest clunk-fid (with-server (tcp/start-server pc/tcp-route {:port test-port :join? false})
                     (let [c @(tcp/client {:host "localhost" :port test-port})]
                       (-> Tversion-frame assemble-packet (dispatch c))
                       (-> Tattach-frame assemble-packet (dispatch c))
                       (-> Twalk-frame assemble-packet (dispatch c))
                       (is (= Tclunk-reply (-> Tclunk-frame assemble-packet (dispatch c) disassemble-packet))))))
