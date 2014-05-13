;; TODO: verify using tests that a user authenticated with session 'x' cannot modify a question
;; whose session is 'y' where 'x' != 'y'.
;; (see update-question-by-id-with-guess) where this is enforced by the db/fetch's :where clause.
(ns italianverbs.postgres
  (:require [korma :as korma]
            [clojure.java.jdbc :as jdbc]
            ))

(def db-connection true)

(defn fetch [ & args])

(defn fetch-and-modify [ & args])

(defn insert! [ & args])

(defn object-id [ & args])

(defn primary-key [map])

