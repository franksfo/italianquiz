;; TODO: verify using tests that a user authenticated with session 'x' cannot modify a question
;; whose session is 'y' where 'x' != 'y'.
;; (see update-question-by-id-with-guess) where this is enforced by the db/fetch's :where clause.
(ns italianverbs.db
  (:require [somnium.congomongo :as mongo] ;; TODO: provide database abstraction over mongo and other possible backing stores.
            ))

(defn fetch [ & args]
  (apply mongo/fetch args))

