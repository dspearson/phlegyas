(ns phlegyas.core
  (:require [phlegyas.util :refer :all]
            [phlegyas.frames :refer :all]
            [phlegyas.types :refer :all]
            [phlegyas.transformers :refer :all]
            [phlegyas.async :refer :all]
            [clojure.core.async :as async]
            [manifold.stream :as s]
            [aleph.tcp :as tcp]
            [taoensso.timbre :as log]
            [primitive-math :as math
             :refer [int->uint short->ushort
                     uint->int ushort->short
                     ubyte->byte byte->ubyte]]))

(def srv nil)
(def debug false)

(log/set-level! :info)

(defn tcp-route
  [s info]
  (let [in (s/stream)
        out (s/stream)
        ninep-server (server! in out)]
    (s/connect s in)
    (s/connect out s)))

(defn go
  []
  (def srv (tcp/start-server tcp-route {:port 10001 :join? false})))

(defn stop
  []
  (.close srv))

(defn r
  []
  (if (nil? srv)
    (go)
    (do
      (stop)
      (go))))

(defn d
  []
  (log/set-level! :debug)
  (def debug true)
  (if (nil? srv)
    (go)
    (do
      (stop)
      (go))))
