(ns italianverbs.korma
  (:use [korma db core])
  (:require [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [italianverbs.unify :refer (unify fail?)]))

;; example stuff that works:

;; (insert verb (values {:value (str {:a 42 :b 43 :c 44})}))
;;   yields:
;; {:value "{:a 42, :c 44, :b 43}", :updated nil, :created #inst "2014-05-16T05:38:53.928635000-00:00", :id 6}


;; http://sqlkorma.com/docs#entities
;; TODO: move to verb.clj or similar: model-type stuff.
(declare test verb vgroup)

(defentity question
  (pk :id)
  (entity-fields :english :italian))

(defentity test
  (pk :id)
  (has-many question))

(defentity verb
  (pk :id)
  (entity-fields :value))

(defentity vgroup
  (pk :id)
  (has-many verb))

(def key-to-table
  {:verb verb
   :test test
   :tag vgroup})

(defn keyword-to-table [collection-as-key]
  "Map a keyword representing a collection to a PostgreSQL table. In the future, a collection
might even represent arbitrary SQL such as a join of multiple tables or a SELECT .. WHERE
on a table."
  (let [table (key-to-table collection-as-key)]
    (if table table
        (throw (.Exception "don't know what table this collection is: " collection-as-key)))))

(def table-to-where
  {:verb (fn [the-map]
           {:value (str the-map)})
   :tag (fn [the-map]
             the-map)})

(def collection-update
  {:verb (fn [modify-with id]
           (let [modify-with (dissoc (dissoc modify-with :created) :updated)
                 set-the-fields {:value (str modify-with)}]
             (log/info (str "updating :verb table with set-fields: " set-fields))
             (log/info (str "updating :verb table with id: " id))
             (update verb
                     (set-fields set-the-fields)
                     (where {:id id}))))
   :tag (fn [modify-with id]
          (log/info (str "UPDATE STATEMENT: " (str "UPDATE vgroup SET verbs = '{" (string/join ","
                                                                                               (:verbs modify-with))
                                                   "}' WHERE id=?") (vec (list id))))


          ;; TODO: allow updates of name of tag: right now, it just
          ;; handles :verbs of the tag.
          (if (:verbs modify-with)
            (exec-raw [(str "UPDATE vgroup SET verbs = '{" (string/join ","
                                                                        (:verbs modify-with))
                            "}' WHERE id=?") (vec (list id))])
            ;; if :id and :verbs are not set, ignore (for now) with a warning.
            (log/warn (str "ignoring update of tag collection with modify-with: " modify-with " and id: " id))))

   ;; default
   true (fn [modify-with id]
          (update verb
                  (set-fields modify-with)
                  (where {:id id})))})

(def do-each-row
  ;; return the 'interesting' parts of a row as a map.
  {:verb (fn [row] ;; for the verb table, parse the :value column into a map, and then
           ;; merge with the other non-:value columns, and underscore the id.
           (merge
            (read-string (:value row))
            {:_id (:id row)}

            ;; .getTime: java.sql.Timestamp -> long
            ;; from-long: long -> Joda DateTime.
            {:created (c/from-long (.getTime (:created row)))} 

            (reduce #(dissoc %1 %2) row
                    (list :created :id :value))))

   :tag (fn [row] ;; for the vgroup table, it's simpler: simply convert :id to :_id.
          (merge
           {:_id (:id row)}
           {:verbs (if (nil? (:verbs row))
                     []
                     (vec (.getArray (:verbs row))))}
           (dissoc (dissoc row :id)
                   :verbs)))})

;; http://sqlkorma.com/docs#db
(def dev (postgres {:db "verbcoach"
                    :user "verbcoach"
                    :password (System/getenv "POSTGRES_SECRET")
                    :host "localhost"
                    :port "5432"}))

(def heroku (postgres {:db "ddb134r1j9l37p"
                       :user "vozlyexfiyoqnl"
                       :password (System/getenv "POSTGRES_SECRET")
                       :host "ec2-184-73-251-115.compute-1.amazonaws.com"
                       :port "5432"
                       :delimiters ""}))

(def heroku-dev (postgres {:db "ddb134r1j9l37p"
                       :user "vozlyexfiyoqnl"
                       :password (System/getenv "POSTGRES_SECRET")
                       :host "ec2-184-73-251-115.compute-1.amazonaws.com"
                       :port "5432"
                       :delimiters ""}))


(def postgres_env (System/getenv "POSTGRES_ENV"))
(defdb korma-db 
  (cond (= postgres_env "heroku")
        heroku
        (= postgres_env "heroku-dev")
        heroku-dev
        true
        dev))

(def table-to-filter
  {:verb (fn [row the-where]
           (log/info (str "the row: " row))
           (log/info (str "the row's value: " (:value row)))
           (log/info (str "the row's value (read-string): " (read-string (:value row))))
           (log/info (str "the-where: " the-where))
           (log/info (str "unify: " (unify (read-string (:value row))
                                           the-where)))
           (not (fail? (unify (read-string (:value row))
                              the-where))))})

(defn fetch [collection & [ the-where ]]
  "select from collection; might take an id. For each returned row, return simply the row as a clojure map, but merge it with an extra field for the primary key (id)."
  (let [the-where
        (if the-where the-where nil)
        id (if (:_id the-where) (Integer. (:_id the-where)))
        table (keyword-to-table collection)]
    (log/info (str "doing fetch with id: " id))
    (log/info (str "table: " table))
    (map (collection do-each-row)
           (if id
             (let [rows (select table
                                (where {:id id}))]
               (if (nil? rows)
                 (log/warn (str "no row found for id: " id)))
               (if (> (.size rows) 1)
                 (log/warn (str "more than one row matched id: " id)))
               rows)

             ;; else, id not given: do a select with a where (or not, if no where).
             (do
               (log/info (str "doing a select with where=" the-where))
               (if the-where
                 (if (collection table-to-filter)
                   (filter (fn [row]
                             ((collection table-to-filter)
                              row the-where))
                           (select table))
                   (select table
                           (where the-where)))
                 (select table)))))))

(defn fetch-and-modify [collection id & [modify-with remove?]]
  "modify-with: map of key/value pairs with which to modify row whose id is given in params."
  (log/info (str "collection: " collection))
  (log/info (str "id: " id))
  (log/info (str "modify-with: " modify-with))
  (let [id (Integer. id)]
    (if remove?
      (delete (keyword-to-table collection)
              (where {:id id}))

      ;; remove=false: do update instead.
      (do
        (log/info (str "collection update: modify-with: " modify-with))
        (log/info (str "collection update: id: " id))
        (apply (collection collection-update)
               (list modify-with id))))))

;; TODO: document what insert-values is for.
(def insert-values
  {:verb (fn [add-with]
           {:value (str add-with)})
   :tag (fn [add-with]
          add-with)
   :test (fn [add-with]
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
  (Integer. (first args)))

(defn primary-key [map]
  (:_id map))



