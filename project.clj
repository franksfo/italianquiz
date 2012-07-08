(defproject italianverbs "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :plugins [[lein-ring "0.7.1"]]
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [slice "0.4.0-SNAPSHOT"]
                 [compojure "0.6.0-RC3"]
                 [congomongo "0.1.9-SNAPSHOT"]
                 [net.defn.ring/ring-jetty-adapter "0.2.0"]
		 [hiccup "1.0.0"]]
  :dev-dependencies [[swank-clojure "1.4.0-SNAPSHOT"]]
  :ring {:handler italianverbs.core/app})


