(ns phlegyas.core
  (:require [phlegyas.frames :refer :all]
            [phlegyas.state :refer :all]
            [phlegyas.vfs :refer :all]
            [phlegyas.util :refer :all]
            [phlegyas.types :refer :all]
            [clojure.core.async :as async]
            [manifold.stream :as s]
            [aleph.tcp :as tcp]
            [taoensso.timbre :as log]
            [primitive-math :as math
             :refer [int->uint short->ushort
                     uint->int ushort->short
                     ubyte->byte byte->ubyte]]))

(def example-mutation-stream (s/stream))
(def state-defaults {:root-filesystem #'example-filesystem!
                     :mutation-stream example-mutation-stream})

(defn example-callback
  [{:keys [state data]}]
  (log/info "callback activated!")
  (log/info "adding a file to the filesystem...")
  (let [fs ((:root-filesystem-name state) (:fs-map state))
        file-path (swap! (:path-pool fs) inc)
        synthetic-file (synthetic-file file-path
                                       (:filename data)
                                       "root"
                                       "root"
                                       0444
                                       "callback!"
                                       (fn [x] (.getBytes (:custom-data-field (:stat x)) "UTF-8"))
                                       (sizeof-string "callback!"))]
    (assoc state :fs-map (assoc (:fs-map state)
                                (:id fs)
                                (-> fs
                                    (insert-file! file-path synthetic-file)
                                    (update-children! (:root-path fs) file-path))))))

(defn add-file
  [filename]
  (s/put! example-mutation-stream {:fn example-callback
                                   :data {:filename filename}}))


(defn server!
  [in out & {:keys [state-machine initial-state] :or {state-machine #'mutate-state initial-state state-defaults}}]
  (async/thread
    (let [frame-stream (s/stream)
          connection-id (java.util.UUID/randomUUID)]
      (log/info connection-id "connection established.")
      (frame-assembler in frame-stream)
      (loop [state (into initial-state {:connection-id connection-id})]
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

(defn dial
  [host port]
  (tcp/client {:host host :port port}))
