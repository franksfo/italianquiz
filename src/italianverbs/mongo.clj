;; TODO: verify using tests that a user authenticated with session 'x' cannot modify a question
;; whose session is 'y' where 'x' != 'y'.
;; (see update-question-by-id-with-guess) where this is enforced by the db/fetch's :where clause.
(ns italianverbs.mongo
  (:require [somnium.congomongo :as congo]
            [clojure.java.jdbc :as jdbc]
            ))

;(def db-connection (congo/make-connection "italian" :host "localhost" :port 27017))

(congo/set-connection! db-connection)
(congo/authenticate db-connection "foo" "bar")

(defn fetch [collection & [ where ]]
  "select from collection where.."
  (congo/fetch collection :where where))

(defn fetch-and-modify [collection id & [modify-with remove?]]
  "modify-with: map of key/value pairs with which to modify this row."
  (let [modify-with (if modify-with modify-with {})
        remove? (if remove? remove? false)]
    (apply congo/fetch-and-modify (list collection {:_id id} modify-with :remove? remove?))))

(defn insert! [collection & [add-with]]
  "args are collection and map of key/value pairs with which to initialize new row."
  (apply congo/insert! (list collection add-with)))

(defn object-id [ & args]
  (apply congo/object-id args))

(defn primary-key [map]
  (:_id map))

