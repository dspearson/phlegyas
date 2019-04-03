(ns phlegyas.async
  (:require [clojure.core.async :as async]
            [manifold.stream :as s]
            [phlegyas.frames :refer :all]
            [phlegyas.types :refer :all]
            [phlegyas.util :refer :all]
            [phlegyas.vfs :refer :all]
            [phlegyas.state :refer :all]
            [taoensso.timbre :as log]))

(defn server!
  [in out & {:keys [state-machine] :or {state-machine #'update-state}}]
  (async/thread
    (log/info "connection established.")
    (let [frame-stream (s/stream)
          frame-thread (frame-assembler in frame-stream)]
      (loop [state {}]
        (log/debug "State:" state)
        (if (nil? state)
          (do
            (log/info "connection aborted.")
            (async/close! in)
            (async/close! out))
          (recur (state-machine @(s/take! frame-stream) out state)))))))
