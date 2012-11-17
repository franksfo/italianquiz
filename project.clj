(defproject italianverbs "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :plugins [[lein-ring "0.7.5"]]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [compojure "1.1.3"]
                 [congomongo "0.2.0"]
                 [org.clojure/tools.logging "0.2.3"]
                 [hiccup "1.0.1"]]
  :dev-dependencies [[org.clojure/tools.nrepl "0.2.0-beta10"]]
  :ring {:handler italianverbs.core/app})



