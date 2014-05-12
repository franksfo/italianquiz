(defproject italianverbs "1.0.0-SNAPSHOT"
  :description "Italian language learning app"
  :plugins [[lein-ring "0.7.3"]]
  :dependencies [[org.clojure/clojure "1.5.1"]

;; http://stackoverflow.com/questions/3782970/how-can-i-display-the-definition-of-a-function-in-clojure-at-the-repl
;                  [org.clojure/repl] 

                 [compojure "1.1.3"]
                 [congomongo "0.2.0"]
                 [org.clojure/java.jdbc "0.0.6"]
                 [org.xerial/sqlite-jdbc "3.7.2"]
                 [clojail "1.0.6"]
                 [log4j/log4j "1.2.16" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jdmk/jmxtools
                                                    com.sun.jmx/jmxri]]
                 [org.clojure/tools.logging "0.2.6"]
                 [hiccup "1.0.1"]

                 [ring/ring-jetty-adapter "1.1.0"]
                 [ring/ring-devel "1.1.0"]
                 [ring-basic-authentication "1.0.1"]
                 [environ "0.2.1"]
                 [com.cemerick/drawbridge "0.0.6"]]

  :min-lein-version "2.0.0"
  :plugins [[environ/environ.lein "0.2.1"]]
  :hooks [environ.leiningen.hooks]


  :profiles
  {:dev {:dependencies [[ring-mock "0.1.3"]
                        [org.clojure/tools.nrepl "0.2.0-beta10"]]}
   :production {:env {:production true}}}
  ;; italianverbs.core/app is defined in src/italianverbs/core.clj.
  :ring {:handler italianverbs.core/app})
