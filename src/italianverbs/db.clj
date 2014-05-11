;; TODO: verify using tests that a user authenticated with session 'x' cannot modify a question
;; whose session is 'y' where 'x' != 'y'.
;; (see update-question-by-id-with-guess) where this is enforced by the db/fetch's :where clause.
(ns italianverbs.db
  (:require [somnium.congomongo :as mongo]
            [clojure.java.jdbc :as jdbc]
            ))

(defn fetch [ & args]
  (apply mongo/fetch args))

(defn fetch-and-modify [ & args]
  (apply mongo/fetch-and-modify args))

(defn insert! [ & args]
  (apply mongo/insert! args))

(defn object-id [ & args]
  (apply mongo/object-id args))

(defn primary-key [map]
  (:_id map))

