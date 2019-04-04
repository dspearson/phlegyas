(defproject phlegyas "0.0.1-SNAPSHOT"
  :description "phlegyas: an implementation of 9P2000"
  :url "https://github.com/dspearson/phlegyas"
  :license {:name "ISC Licence"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.async "0.4.490"]
                 [primitive-math "0.1.6"]
                 [manifold "0.1.9-alpha3"]
                 [aleph "0.4.6"]
                 [com.taoensso/timbre "4.10.0"]]
  :plugins [[cider/cider-nrepl "0.21.1"]]
  :main ^:skip-aot phlegyas.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})