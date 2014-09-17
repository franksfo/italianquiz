(ns italianverbs.mail
  (:require [clojurewerkz.mailer.core :refer [delivery-mode! with-settings with-defaults 
                                              with-settings build-email deliver-email]]))

(delivery-mode! :test)

;; build a message (can be used in unit tests or for various forms of delayed delivery)
;;
;; Please note that email/templates/warning.mustache should be on your classpath. For example, with Leiningen 2,
;; you would use :resource-paths for this, like so: :resource-paths ["src/resources"]
(build-email {:from "Joe The Robot", 
              :to ["ops@megacorp.internal" "oncall@megacorp.internal"] 
              :subject "OMG everything is down!"}
             "email/warning.mustache" {:name "Joe"}
             :text/plain)

             
