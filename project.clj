;; You must define POSTGRES_ENV and (if needed) POSTGRES_SECRET
;; in your environment so that 'lein ring server[-headless]' can work.
(defproject italianverbs "1.0.0-SNAPSHOT"
  :description "Italian language learning app"
  :dependencies [[clj-time "0.7.0"]
                 [clojail "1.0.6"]
                 [clojure.jdbc "0.4.0"]
                 [clojurewerkz/mailer "1.2.0"]
                 [com.cemerick/drawbridge "0.0.6"
                  :exclusions [ring/ring-core]] ;; https://github.com/cemerick/drawbridge/issues/8
                 [com.cemerick/friend "0.2.1"]
                 [compojure "1.1.6"]
                 [congomongo "0.4.4"]
                 [digest "1.4.4"]
                 [environ "1.0.0"]
                 [formative "0.8.8"]
                 [hiccup "1.0.5"]
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
                 [org.postgresql/postgresql "9.4-1201-jsonb-jdbc41-SNAPSHOT"]
                 [org.webjars/foundation "4.0.4"]
                 [org.xerial/sqlite-jdbc "3.7.2"]
                 [postmark "1.1.0" :exclusions [org.clojure/clojure]]
                 [ring/ring-jetty-adapter "1.1.0"]
                 [ring/ring-devel "1.1.0"]
                 [ring-basic-authentication "1.0.1"]]

  :min-lein-version "2.0.0"
  :plugins [
            [lein-environ "1.0.0"]
            [lein-localrepo "0.4.0"]
            [lein-pprint "1.1.1"]
            [lein-ring "0.7.3"]]

  ;; The 'eugene' repository hosts my slightly-modified Postgres JDBC
  ;; driver that copes with JSONB-typed columns.  Source code
  ;; available at: https://github.com/ekoontz/pgjdbc/tree/jsonb
  :repositories {"eugene" "http://hiro-tan.org/~ekoontz/mvn/repository"}

  :resource-paths ["resources"]

  :hooks [environ.leiningen.hooks]

  ;; You must define POSTGRES_ENV and (if needed) POSTGRES_SECRET
  ;; in your environment so that 'lein ring server[-headless]' can work.
  ;; See src/italianverbs/korma.clj for a list of supported
  ;; POSTGRES_ENVs.
  :profiles {:workstation {:env {:postgres-env "workstation"}}
             :production {:env {:postgres-env "heroku"}}
             :travis-ci {:env {:postgres-env "travis-ci"}}}

  ;; italianverbs.core/app is defined in src/italianverbs/core.clj.
  :ring {:handler italianverbs.core/app})
