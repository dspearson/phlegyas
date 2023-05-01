(ns phlegyas.core
  (:gen-class)
  (:require
   [clojure.tools.cli :refer [parse-opts]]
   [phlegyas.server :refer [start-server]]))

(def cli-options
  [["-p" "--port PORT" "Port number"
    :default 10001
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]])

(defn -main
  [& args]
  (let [opts (parse-opts args cli-options)]
    (start-server (get-in opts [:options :port]))))
