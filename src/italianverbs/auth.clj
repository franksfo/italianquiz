(ns italianverbs.auth)

(require '[clojure.tools.logging :as log])
(require '[clojure.string :as str])
(require '[digest])
(require '[environ.core :refer [env]])
(require '[italianverbs.korma :as db])
(require '[cemerick.friend :as friend])
(require '[cemerick.friend.workflows :as workflows])
(require '[cemerick.friend.credentials :as creds])

(defn get-user-id [fetch-fn]
  (let [username (:username (friend/current-authentication))]
    (:id (first (fetch-fn :student {:username username})))))

(defn confirm-and-create-user [request]
  (do (log/info (str "confirm-and-create-user: " request))
      {:status 302
       :headers {"Location" "/game"}}))

(defn haz-admin []
  (if (not (nil? (friend/current-authentication)))
    (not (nil?
          (:italianverbs.core/admin
           (:roles (friend/current-authentication)))))))

(defn is-authenticated [request if-authenticated]
  (if (not (nil? (friend/current-authentication)))
    if-authenticated
    {:status 302
     :headers {"Location" "/login"}}))

