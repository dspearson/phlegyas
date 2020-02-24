(ns phlegyas.core
  (:require [phlegyas.server :refer :all])
  (:gen-class))

(defn -main
  [& args]
  (start-server))
