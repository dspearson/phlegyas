(ns phlegyas.core
  (:require [phlegyas.frames :refer :all]
            [phlegyas.state :refer :all]
            [phlegyas.vfs :refer :all]
            [phlegyas.util :refer :all]
            [clojure.core.async :as async]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [aleph.tcp :as tcp]
            [taoensso.timbre :as log])
  (:gen-class))

(defn server!
  [in out & {:keys [root-filesystem-constructor]}]
  (let [state (atom {:root-filesystem (or root-filesystem-constructor #'example-filesystem!)})
        incoming-frame-stream (s/stream)
        outgoing-frame-stream (s/stream)
        in-flight-requests (atom #{})
        frame-assembler-thread (frame-assembler in incoming-frame-stream)
        connection {:state state
                    :incoming-frame-stream incoming-frame-stream
                    :outgoing-frame-stream outgoing-frame-stream
                    :in-flight-requests in-flight-requests
                    :in-stream in
                    :out-stream out
                    :frame-assembler-thread frame-assembler-thread}]
    (s/connect-via outgoing-frame-stream #(s/put! out (assemble-packet %)) out)
    (consume incoming-frame-stream outgoing-frame-stream connection #'state-handler)
    connection))

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
  (def global-connections (atom {}))
  (def srv (tcp/start-server tcp-route {:port 10001 :join? false})))

(defn r
  []
  (if (nil? srv)
    (go)
    (do
      (.close srv)
      (go))))

(defn dial
  [host port]
  (tcp/client {:host host :port port}))

(defn -main
  [& args]
  (tcp/start-server tcp-route {:port 10001 :join? true}))
