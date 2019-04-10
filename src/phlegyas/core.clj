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

;; (def example-mutation-stream (s/stream))

;; (defn example-callback
;;   [{:keys [state data]}]
;;   (log/info "callback activated!")
;;   (log/info "adding a file to the filesystem...")
;;   (let [fs ((:root-filesystem-name state) (:fs-map state))
;;         file-path (swap! (:path-pool fs) inc)
;;         synthetic-file (synthetic-file file-path
;;                                        (:filename data)
;;                                        "root"
;;                                        "root"
;;                                        0444
;;                                        "callback!"
;;                                        (fn [x] (.getBytes (:custom-data-field (:stat x)) "UTF-8"))
;;                                        (sizeof-string "callback!"))]
;;     (assoc state :fs-map (assoc (:fs-map state)
;;                                 (:id fs)
;;                                 (-> fs
;;                                     (insert-file! file-path synthetic-file)
;;                                     (update-children! (:root-path fs) file-path))))))

;; (defn add-file
;;   [filename]
;;   (s/put! example-mutation-stream {:fn example-callback
;;                                    :data {:filename filename}}))


;; (defn server!
;;   [in out & {:keys [state-machine initial-state] :or {state-machine #'mutate-state initial-state state-defaults}}]
;;   (async/thread
;;     (let [frame-stream (s/stream)
;;           connection-id (java.util.UUID/randomUUID)]
;;       (log/info connection-id "connection established.")
;;       (frame-assembler in frame-stream)
;;       (loop [state (atom (into initial-state {:connection-id connection-id}))]
;;         (let [frame @(s/take! frame-stream)]
;;           (log/debug "State:" state)
;;           (if (nil? frame)
;;             (do
;;               (log/info connection-id "connection closed."))
;;             (recur (state-machine frame out state))))))))

(defn server!
  [in out]
  (let [state (atom {:root-filesystem #'example-filesystem!})
        incoming-frame-stream (s/stream)
        outgoing-frame-stream (s/stream)
        _ (frame-assembler in incoming-frame-stream)]
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
