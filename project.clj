(defproject phlegyas "master-SNAPSHOT"
  :description "phlegyas: an implementation of 9P2000"
  :url "https://github.com/dspearson/phlegyas"
  :license {:name "ISC Licence"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.597"]
                 [cljsjs/bytebuffer "5.0.1-0"]
                 [org.clojure/core.async "1.1.587"]
                 [primitive-math "0.1.6"]
                 [manifold "0.1.8"]
                 [aleph "0.4.6"]
                 [com.taoensso/timbre "4.10.0"]
                 [buddy/buddy-core "1.6.0"]
                 [org.clojure/core.incubator "0.1.4"]
                 [org.clojure/tools.cli "1.0.194"]]
  :source-paths ["src"]
  :aliases {"fig"       ["trampoline" "run" "-m" "figwheel.main"]
            "fig:build" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
            "fig:min"   ["run" "-m" "figwheel.main" "-O" "advanced" "-bo" "dev"]
            "fig:test"  ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" "core.test-runner"]}
  :plugins [[cider/cider-nrepl "0.24.0"]
            [jonase/eastwood "0.3.7"]]
  :main ^:skip-aot phlegyas.core
  :target-path "target/%s"
  :global-vars {*warn-on-reflection* true}
  :profiles {:dev {:dependencies [[com.bhauman/figwheel-main "0.2.3"]
                                  [com.bhauman/rebel-readline-cljs "0.1.4"]
                                  [lein-npm "0.6.2"]]}
             :uberjar {:aot :all}
             :test {:plugins [[lein-ancient "0.6.15"]
                              [lein-cljfmt "0.6.6"]
                              [lein-kibit "0.1.8"]
                              [jonase/eastwood "0.3.7"]]}}
  :cljfmt {:remove-surrounding-whitespace? false})
