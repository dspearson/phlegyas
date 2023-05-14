(ns user
  (:require [phlegyas.components :refer [start-system! stop-system!]]
            [phlegyas.system :refer [system]]))

(comment

  (println @system)

  (start-system!)

  (stop-system!))
