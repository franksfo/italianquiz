(defproject italianverbs "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [slice "0.4.0-SNAPSHOT"]
                 [org.apache.lucene/lucene-core "3.0.2"]
                 [compojure "0.6.0-RC3"]
                                        ;                 [congomongo "0.1.3-SNAPSHOT"]
                 [congomongo "0.1.7-SNAPSHOT"]
; not found in clojars..
;		 [mungbean "1.0-SNAPSHOT"]
                 [net.defn.ring/ring-jetty-adapter "0.2.0"]
                 [ring/ring-core "0.3.0"]                   
                 [ring/ring-jetty-adapter "0.2.0"]    
		 [hiccup "0.3.4"]]
  :dev-dependencies [[lein-ring "0.3.2"]]
  :ring {:handler italianverbs.core/app})
