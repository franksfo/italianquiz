(defproject italianverbs "1.0.0-SNAPSHOT"
  :description "Italian language learning app"
  :plugins [[lein-ring "0.7.3"]]
  :dependencies [[org.clojure/clojure "1.5.1"]

                 [org.clojure/math.numeric-tower "0.0.4"]

                 ;; this is the latest version as of March 19, 2014:
                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"] 

                 [clj-time "0.7.0"]
                 [clojail "1.0.6"]
                 [compojure "1.1.6"]
                 [congomongo "0.4.4"]
                 [org.clojure/java.jdbc "0.0.6"]
                 [org.xerial/sqlite-jdbc "3.7.2"]

                 [log4j/log4j "1.2.16" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jdmk/jmxtools
                                                    com.sun.jmx/jmxri]]
                 [org.clojure/tools.logging "0.2.6"]
                 [hiccup "1.0.1"]]
  :profiles
  {:dev {:dependencies [[ring-mock "0.1.3"]
                        [org.clojure/tools.nrepl "0.2.0-beta10"]]}}
  ;; italianverbs.core/app is defined in src/italianverbs/core.clj.
  :ring {:handler italianverbs.core/app})



