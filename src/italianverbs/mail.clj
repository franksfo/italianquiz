(ns italianverbs.mail
  (:require [clojurewerkz.mailer.core :refer [delivery-mode! with-settings with-defaults 
                                              with-settings build-email deliver-email render
                                              with-delivery-mode]]))

(delivery-mode! :test)

;; build a message (can be used in unit tests or for various forms of delayed delivery)
;;
;; Please note that email/templates/warning.mustache should be on your classpath. For example, with Leiningen 2,
;; email templates are stored in the top-level project.clj's :resource-paths, which is: "resources". 
;; within that is 'email' which you can find in the second parameter below.
(def registration-email
  (build-email {:from "Verbcoach Registration", 
                :to ["ekoontz@hiro-tan.org"]
                :subject "Verbcoach: Welcome! Please confirm your password."}
               "email/warning.mustache" {:name "Joe" :authentication-hash "abcdef12345678"
                                         :email "joe@foo.com"}
               :text/plain))

(def reset-password-email
  (build-email {:from "Verbcoach Lost Password Recovery", 
                :to ["student@somewhere.edu"] 
                :subject "Verbcoach: Reset your password"}
               "email/warning.mustache" {:name "Joe" :authentication-hash "12345678abcdef"}
               :text/plain))

(defn send-test-mail []
  (with-defaults { :foo "bar" :from "Joe The Robot <robot@megacorp.internal>" :subject "[Do Not Reply] Warning! Achtung! Внимание!" }
    registration-email))

(defn foo []
  (with-delivery-mode :smtp
    (deliver-email {:from "Joe The Robot", :to ["ekoontz@hiro-tan.org"] :subject "OMG everything is down!"}
                   "email/warning.mustache" {:name "Joe" :host "mail.hiro-tan.org"})))



