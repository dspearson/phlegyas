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

(def global-connections (atom {}))

(defn server!
  [in out]
  (let [state (atom {:root-filesystem #'example-filesystem!})
        incoming-frame-stream (s/stream)
        outgoing-frame-stream (s/stream)
        uuid (keyword (uuid!))
        _ (frame-assembler in incoming-frame-stream)]
    (swap! global-connections assoc uuid state)
    (s/connect-via outgoing-frame-stream #(s/put! out (assemble-packet %)) out)
    (consume-with-state incoming-frame-stream outgoing-frame-stream state #'state-handler)))

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

(defn dial
  [host port]
  (tcp/client {:host host :port port}))

(defn -main
  [& args]
  (tcp/start-server tcp-route {:port 10001 :join? true}))
