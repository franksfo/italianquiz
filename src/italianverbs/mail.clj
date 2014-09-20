(ns italianverbs.mail
  (:require [clojurewerkz.mailer.core :refer [delivery-mode! with-settings with-defaults 
                                              with-settings build-email deliver-email]]))

(delivery-mode! :test)

;; build a message (can be used in unit tests or for various forms of delayed delivery)
;;
;; Please note that email/templates/warning.mustache should be on your classpath. For example, with Leiningen 2,
;; email templates are stored in the top-level project.clj's :resource-paths, which is: "resources". 
;; within that is 'email' which you can find in the second parameter below.
(def my-cool-email-message
  (build-email {:from "Joe The Robot", 
                :to ["ops@megacorp.internal" "oncall@megacorp.internal"] 
                :subject "OMG everything is down!"}
               "email/warning.mustache" {:name "Joe"}
               :text/plain))
