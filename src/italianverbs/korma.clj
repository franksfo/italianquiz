(ns italianverbs.korma
  (:use [korma db core]))


;; http://sqlkorma.com/docs#db
(def dev (postgres {:db "verbcoach"
                   :user "verbcoach"
                   :password ""
                   ;; optional keys
                   :host "localhost"
                   :port "5432"
                   :delimiters ""}))

(def heroku (postgres {:db "from-heroku"
                       :user "from-heroku"
                       :password "from-heroku"
                       ;; optional keys
                       :host "from-heroku"
                       :port "from-heroku"
                       :delimiters ""}))
                       ;; remove delimiters

(defdb korma-db dev)

;; http://sqlkorma.com/docs#entities
(declare verb vgroup)

(defentity verb
  (pk :id))

(defentity vgroup
  (pk :id)
  (has-many verb))

(defn fetch [collection & [ where ]]
  "select from collection where.."
  nil)

(defn fetch-and-modify [collection id & [modify-with remove?]]
  "modify-with: map of key/value pairs with which to modify row whose id is given in params."
  nil)

(defn insert! [collection & [add-with]]
  "args are collection and map of key/value pairs with which to initialize new row."
  nil)

(defn object-id [ & args]
  nil)

(defn primary-key [map]
  map)


