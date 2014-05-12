;; TODO: verify using tests that a user authenticated with session 'x' cannot modify a question
;; whose session is 'y' where 'x' != 'y'.
;; (see update-question-by-id-with-guess) where this is enforced by the db/fetch's :where clause.
(ns italianverbs.db
  (:require [somnium.congomongo :as congo]
            [clojure.java.jdbc :as jdbc]
            ))

;(def db-connection (congo/make-connection "italian" :host "localhost" :port 27017))
(def db-connection (congo/make-connection "heroku_app25118324" :host "ds033107.mongolab.com" :port 33107))



(congo/set-connection! db-connection)
(congo/authenticate db-connection "foo" "bar")

(defn fetch [ & args]
  (apply congo/fetch args))

(defn fetch-and-modify [ & args]
  (apply congo/fetch-and-modify args))

(defn insert! [ & args]
  (apply congo/insert! args))

(defn object-id [ & args]
  (apply congo/object-id args))

(defn primary-key [map]
  (:_id map))

