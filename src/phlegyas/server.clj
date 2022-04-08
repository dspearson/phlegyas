(ns phlegyas.server
  (:require [phlegyas.frames :refer [frame-assembler assemble-packet]]
            [phlegyas.state :refer [state-handler consume]]
            [phlegyas.vfs :refer [example-filesystem!]]
            [manifold.stream :as s]
            [aleph.tcp :as tcp]))

(defn server!
  [in out & {:keys [root-filesystem-constructor handler]}]
  (let [state                  (atom {:root-filesystem (or root-filesystem-constructor #'example-filesystem!)})
        handler                (or handler state-handler)
        incoming-frame-stream  (s/stream)
        outgoing-frame-stream  (s/stream)
        in-flight-requests     (atom #{})
        frame-assembler-thread (frame-assembler in incoming-frame-stream)
        connection             {:state                  state
                                :handler handler
                                :incoming-frame-stream  incoming-frame-stream
                                :outgoing-frame-stream  outgoing-frame-stream
                                :in-flight-requests     in-flight-requests
                                :in-stream              in
                                :out-stream             out
                                :frame-assembler-thread frame-assembler-thread}]
    (s/connect-via outgoing-frame-stream #(s/put! out (assemble-packet %)) out)
    (consume incoming-frame-stream outgoing-frame-stream connection state-handler)
    connection))

(defn tcp-route-with-custom-handlers
  [handler connection-atom]
  (fn [s _]
    (let [in  (s/stream)
          out (s/stream)
          _   (reset! connection-atom (server! in out :handler handler))]
      (s/connect s in)
      (s/connect out s))))

(defn tcp-route
  [s _]
  (let [in  (s/stream)
        out (s/stream)
        _   (server! in out)]
    (s/connect s in)
    (s/connect out s)))

(defn start-server
  ([]
   (start-server 10001))
  ([port]
   (start-server port true))
  ([port join?]
   (start-server port join? (atom nil)))
  ([port join? connection]
   (let [router (tcp-route-with-custom-handlers state-handler connection)
         server (tcp/start-server tcp-route {:port port :join? join?})]
     server)))
