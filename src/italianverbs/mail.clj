(ns italianverbs.mail
  (:require [clojurewerkz.mailer.core :refer [delivery-mode! with-settings with-defaults 
                                              with-settings build-email deliver-email render
                                              with-delivery-mode]]
            [environ.core :refer [env]]
            [postmark.core :refer [postmark]]))

;; TODO: use weavejester/environ.

(def api-key (or (get (System/getenv "POSTMARK_API_KEY"))
                 "4148c60f-3d15-4e7f-bd81-b56cd9d73c5b"))

;; sender email: ekoontz@verbcoach.com
(def send-message (postmark api-key "ekoontz@verbcoach.com"))

(def postmark-api-key (env :postmark-api-key))

(defn welcome-message []
  (send-message {:to "ekoontz@hiro-tan.org"
                 :subject "Welcome to Verbcoach!"
                 :text "Hello! Click here to confirm your registration."
                 :tag "welcome"}))


(defn forgot-password-message []
  (send-message {:to "ekoontz@hiro-tan.org"
                 :subject "Instructions for Password reset"
                 :text "Click here to reset your password."
                 :tag "support"}))


