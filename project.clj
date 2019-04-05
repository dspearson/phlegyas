(defproject phlegyas "0.1.1"
  :description "phlegyas: an implementation of 9P2000"
  :url "https://github.com/dspearson/phlegyas"
  :license {:name "ISC Licence"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [primitive-math "0.1.6"]]
  :plugins [[cider/cider-nrepl "0.21.1"]]
  :main ^:skip-aot phlegyas.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
