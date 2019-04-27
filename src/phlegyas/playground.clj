(ns phlegyas.playground
  (:require [phlegyas.frames :refer :all]
            [phlegyas.state :refer :all]
            [phlegyas.vfs :refer :all]
            [phlegyas.util :refer :all]
            [phlegyas.client :refer :all]
            [clojure.core.async :as async]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [aleph.tcp :as tcp]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [taoensso.timbre :as log])
  (:gen-class))

;; useful for profiling.

(tufte/add-basic-println-handler! {})

(defn test-connection
  []
  (let [state (atom {:root-filesystem #'example-filesystem!})
        incoming-frame-stream (s/stream)
        outgoing-frame-stream (s/stream)
        in-flight-requests (atom #{})
        ;; frame-assembler-thread (frame-assembler in incoming-frame-stream)
        connection {:state state
                    :incoming-frame-stream incoming-frame-stream
                    :outgoing-frame-stream outgoing-frame-stream
                    :in-flight-requests in-flight-requests
                    :in-stream nil
                    :out-stream nil
                    :frame-assembler-thread nil}]
    connection))

(log/set-level! :trace)

(defn attach-and-create
  [conn]
  (Tversion {:frame :Tversion :msize 8192 :tag 0 :version "9P2000"} conn)
  (Tattach {:frame :Tattach :tag 0 :afid 0 :fid 0 :uname "dsp" :aname "dsp"} conn)

  (for [x (range 1 501)]
    (do
      (Twalk {:frame :Twalk :tag 0 :fid 0 :newfid x :wnames []} conn)
      (Twalk {:frame :Twalk :tag 0 :fid x :newfid x :wnames [(str x)]} conn)
      (Tcreate {:frame :Tcreate :tag 0 :fid x :name (str x) :perm 0755 :iomode 1} conn)
      (Tclunk {:frame :Tclunk :tag 0 :fid x} conn)))
  true)
