(ns phlegyas.components
  (:require [integrant.core :refer [init-key halt-key! halt! prep init] :as integrant]
            [aero.core :as aero]
            [nrepl.server :as nrepl]
            [clojure.java.io :as io]
            [phlegyas.server :refer [start-server]]
            [phlegyas.system :refer [system]]
            [phlegyas.util :refer [str->file parse-int]]
            [phlegyas.db :refer [initialise-database]]
            [taoensso.timbre :as timbre
             :refer [info debug error]]))

(defn get-config
  []
  (if-let [config (str->file (System/getenv "PHLEGYAS_CONFIG"))]
    config
    (io/resource "config.edn")))

(defmethod init-key :phlegyas/config
  [_ _]
  (let [config (get-config)]
    (info "Reading configuration:" (.toString config))
    (aero/read-config config)))

(defmethod halt-key! :phlegyas/config
  [_ _])

(defmethod init-key :phlegyas/nrepl
  [_ {:keys [config]}]
  (let [{:keys [enabled? port]} (:nrepl config)]
    (when enabled?
      (info "Starting NREPL server, port:" port)
      (nrepl/start-server :port port :bind "127.0.0.1"))))

(defmethod halt-key! :phlegyas/nrepl
  [_ server]
  (when server
    (info "Stopping NREPL server.")
    (nrepl/stop-server server)))

(defmethod init-key :phlegyas/server
  [_ {:keys [config]}]
  (let [{:keys [enabled? port]} (:server config)]
    (when enabled?
      (info "Starting 9P server, port:" port)
      (start-server {:port port}))))

(defmethod halt-key! :phlegyas/server
  [_ server]
  (info "Stopping server.")
  (.close server))

(defmethod init-key :phlegyas/database
  [_ {:keys [config]}]
  (let [{:keys [dbname] :as db-config} (:database config)]
    (when-not (str->file dbname)
      (info "Creating database:" dbname)
      (initialise-database db-config))
    db-config))

(defmethod halt-key! :phlegyas/database
  [_ database])

(defn start-system!
  ([]
   (start-system! "system.edn"))
  ([config]
   (info "System starting...")
   (reset! system (-> config
                      io/resource
                      slurp
                      integrant/read-string
                      prep
                      init))))

(defn stop-system!
  ([]
   (stop-system! system))
  ([system]
   (when @system
     (halt! @system)
     (reset! system nil))))
