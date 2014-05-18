(ns italianverbs.korma
  (:use [korma db core])
  (:require [clojure.tools.logging :as log]))

;; example stuff that works:

;; (insert verb (values {:value (str {:a 42 :b 43 :c 44})}))
;;   yields:
;; {:value "{:a 42, :c 44, :b 43}", :updated nil, :created #inst "2014-05-16T05:38:53.928635000-00:00", :id 6}


;; http://sqlkorma.com/docs#entities
;; TODO: move to verb.clj or similar: model-type stuff.
(declare verb vgroup)

(defentity verb
  (pk :id)
  (entity-fields :value))

(defentity vgroup
  (pk :id)
  (has-many verb))

(def key-to-t
  {:verb verb
   :tag vgroup})

(defn keyword-to-table [collection-as-key]
  (let [result (key-to-t collection-as-key)]
    (if result result
        (throw (.Exception "don't know what table this is: " collection-as-key)))))

(def table-to-where
  {:verb (fn [the-map]
           {:value (str the-map)})
   :tag (fn [the-map]
             the-map)})

(def do-each-row
  ;; return the 'interesting' parts of a row as a map.
  {:verb (fn [row] ;; for the verb table, parse the :value column into a map, and then
           ;; merge with the other non-:value columns, and underscore the id.
           (merge
            (read-string (:value row))
            {:_id (:id row)}
            (dissoc (dissoc row :value) :id)))

   :tag (fn [row] ;; for the vgroup table, it's simpler: simply convert :id to :_id.
          (merge
           {:_id (:id row)}
           (dissoc row :id)))})

;; http://sqlkorma.com/docs#db
(def dev (postgres {:db "verbcoach"
                   :user "verbcoach"
                   :password "verbcoach"
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

(defn fetch [collection & [ the-where ]]
  "select from collection; might take an id. For each returned row, return simply the row as a clojure map, but merge it with an extra field for the primary key (id)."
  (let [the-where
        (if the-where the-where nil)
        id (if (:_id the-where) (Integer. (:_id the-where)))]
    (log/info (str "doing fetch with id: " id))
    (log/info (str "table: " (keyword-to-table collection)))
    (if id
      (let [row (first
                 (select (keyword-to-table collection)
                         (where {:id id})))]
        (if row
          (list (apply (collection do-each-row)
                       (list row)))))

      ;; else, id not given: do a select with a where (or not, if no where).
      (map (fn [row] 
             (apply (collection do-each-row)
                    (list row)))
           (if the-where
             (let [the-where (apply (table-to-where collection) (list the-where))]
               (select (keyword-to-table collection)
                       (where (apply (table-to-where collection) (list the-where)))))
             (select (keyword-to-table collection)))))))

(defn fetch-and-modify [collection id & [modify-with remove?]]
  "modify-with: map of key/value pairs with which to modify row whose id is given in params."
  (let [id (Integer. id)]
    (if remove?
      (delete (keyword-to-table collection)
              (where {:id id}))

      ;; remove=false: do update instead.
      (update (keyword-to-table collection)
              (where {:id id})))))

(def insert-values
  {:verb (fn [add-with]
           {:value (str add-with)})
   :tag (fn [add-with]
          add-with)})

(defn insert! [collection & [add-with]]
  "args are collection and map of key/value pairs with which to initialize new row. we simply serialize the map with (str). Any embedded objects will be lost due to serialization, so map should be only of atoms (strings, numbers, etc) or vectors of atoms (vectors of vectors should work too, provided they are eventually atoms at the leaves)"
  ;; We remove :created and :updated, if any, since we'll let postgres handle
  ;; those through its own constraints and triggers.
  (let [add-with (dissoc (dissoc add-with :created) :updated)]
    (insert (keyword-to-table collection)
            (values 
             (apply (collection insert-values)
                    (list add-with))))))

(defn object-id [ & args ]
  "compare with mongo/object-id, which uses mongo ids."
  (first args))

(defn primary-key [map]
  (:_id map))



