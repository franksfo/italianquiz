(ns italianverbs.session
    (:use
    [hiccup core])
    (:require [clojure.string :as string]
              [somnium.congomongo :as db])
    (:import (java.security ;; TOODO: what are these imports doing here?
              NoSuchAlgorithmException
              MessageDigest)
             (java.math BigInteger)))

(defn get-session-key [request]
  (let [cookies (get request :cookies)]
    (if cookies
      (let [ring-session (get cookies "ring-session")]
        (if ring-session
          (get ring-session :value))))))

(defn find-or-insert-user [username]
  (let [found (db/fetch-one :users :where {:name username})]
       (if found found
	 (db/insert! :users {:name username :lastlogin "never"}))))

;; TODO : figure out date/time in Clojure.
(defn last-activity [username]
  (let [my-user (db/fetch-one :users :where {:name username})]
       (db/update! :users my-user (merge my-user {:lastlogin "reallynow"}))))

(defn request-to-session [request]
  (get (get (get request :cookies) "ring-session") :value))

(defn get-session-row [request]
  (db/fetch-one :session))

(defn get-username [request]
  (let [fetch (get-session-row request)]
    (if fetch
      (get fetch :user))))

(defn register [request] ;; create a new session for the given user.
  "register session from database keyed on request; return session record from db."
  (let [username (str "eugene-" (subs
                                 (if (not (nil?
                                           (get-session-key request)))
                                   (get-session-key request)
                                   "<nilsession>")
                                 0 5))
        newuser (find-or-insert-user username)
        newsession
        (do (last-activity username)
            (db/insert! :session {:user username
                               :start "now"
                               :cookie (get-session-key request)}))]
       {:name (get newuser :name)}))

(defn unregister [request]
  "remove session from database keyed on request; return nil."
  (let [cookie (if (get request :cookies)
                 (if (get (get request :cookies) "ring-session")
                   (if (get (get (get request :cookies) "ring-session") :value)
                     (get (get (get request :cookies) "ring-session") :value))))]
    (if cookie
      (db/destroy! :session {:cookie cookie})
      nil)))

