(defproject phlegyas "master-SNAPSHOT"
  :description "phlegyas: an implementation of 9P2000"
  :url "https://github.com/dspearson/phlegyas"
  :license {:name "ISC Licence"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/core.async "1.3.618"]
                 [primitive-math "0.1.6"]
                 [manifold "0.1.9"]
                 [aleph "0.4.6"]
                 [com.taoensso/timbre "5.1.2"]
                 [org.clojure/core.incubator "0.1.4"]
                 [org.clojure/tools.cli "1.0.206"]]
  :plugins [[cider/cider-nrepl "0.28.3"]
            [jonase/eastwood "0.4.0"]
            [lein-ancient "0.7.0"]]
  :main ^:skip-aot phlegyas.core
  :target-path "target/%s"
  :global-vars {*warn-on-reflection* true}
  :profiles {:uberjar {:aot :all}
             :test    {:plugins [[lein-ancient "0.7.0"]
                                 [lein-cljfmt "0.7.0"]
                                 [lein-kibit "0.1.8"]
                                 [jonase/eastwood "0.4.0"]]}}
  :cljfmt {:remove-surrounding-whitespace? false})
