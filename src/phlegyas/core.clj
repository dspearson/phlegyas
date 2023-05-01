(ns phlegyas.core
  (:gen-class)
  (:require
   [phlegyas.components :refer [start-system!]]))

(defn -main
  [& args]
  (start-system!))
