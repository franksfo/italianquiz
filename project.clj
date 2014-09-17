;; You must define POSTGRES_ENV and (if needed) POSTGRES_SECRET
;; in your environment so that 'lein ring server[-headless]' can work.
(defproject italianverbs "1.0.0-SNAPSHOT"
  :description "Italian language learning app"
  :dependencies [[clj-time "0.7.0"]
                 [clojail "1.0.6"]
                 [clojurewerkz/mailer "1.2.0"]
                 [com.cemerick/drawbridge "0.0.6"
                  :exclusions [ring/ring-core]] ;; https://github.com/cemerick/drawbridge/issues/8
                 [com.cemerick/friend "0.2.1"]
                 [compojure "1.1.6"]
                 [congomongo "0.4.4"]
                 [environ "0.2.1"]
                 [formative "0.8.8"]
                 [hiccup "1.0.1"]
                 [javax.servlet/servlet-api "2.5"]
                 [korma "0.3.0-RC5"]
                 [log4j/log4j "1.2.16" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jdmk/jmxtools
                                                    com.sun.jmx/jmxri]]


                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"] 
                 [org.clojure/data.json "0.2.5"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.postgresql/postgresql "9.2-1002-jdbc4"]
                 [org.webjars/foundation "4.0.4"]
                 [org.xerial/sqlite-jdbc "3.7.2"]
                 [ring/ring-jetty-adapter "1.1.0"]
                 [ring/ring-devel "1.1.0"]
                 [ring-basic-authentication "1.0.1"]]

  :min-lein-version "2.0.0"
  :plugins [[environ/environ.lein "0.2.1"]
            [lein-ring "0.7.3"]]

  :resource-paths ["resources"]

  :hooks [environ.leiningen.hooks]

  ;; You must define POSTGRES_ENV and (if needed) POSTGRES_SECRET
  ;; in your environment so that 'lein ring server[-headless]' can work.
  ;; See src/italianverbs/korma.clj for a list of supported
  ;; POSTGRES_ENVs.
  :profiles
  {:dev {:dependencies [[ring-mock "0.1.3"]
                        [org.clojure/tools.nrepl "0.2.0-beta10"]]}
   :production {:env {:production true
                      :postgres-env "heroku-dev"}}}
  ;; italianverbs.core/app is defined in src/italianverbs/core.clj.
  :ring {:handler italianverbs.core/app})
