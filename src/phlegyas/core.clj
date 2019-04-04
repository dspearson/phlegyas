(ns phlegyas.core
  (:require [phlegyas.frames :refer :all]
            [phlegyas.state :refer :all]
            [clojure.core.async :as async]
            [manifold.stream :as s]
            [aleph.tcp :as tcp]
            [taoensso.timbre :as log]
            [primitive-math :as math
             :refer [int->uint short->ushort
                     uint->int ushort->short
                     ubyte->byte byte->ubyte]]))

(defn server!
  [in out & {:keys [state-machine] :or {state-machine #'mutate-state}}]
  (async/thread
    (let [frame-stream (s/stream)
          connection-id (java.util.UUID/randomUUID)]
      (log/info connection-id "connection established.")
      (frame-assembler in frame-stream)
      (loop [state {:connection-id connection-id}]
        (let [frame @(s/take! frame-stream)]
          (log/debug "State:" state)
          (if (nil? frame)
            (do
              (log/info connection-id "connection closed."))
            (recur (state-machine frame out state))))))))

(log/set-level! :info)

(def srv nil)

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

(defn r
  []
  (if (nil? srv)
    (go)
    (do
      (.close srv)
      (go))))
