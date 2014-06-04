(ns italianverbs.korma
  (:refer-clojure :exclude [test])
  (:use [korma db core])
  (:require [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [italianverbs.unify :refer (unify fail?)]))

(declare korma-db)

;; example stuff that works:

;; (insert verb (values {:value (str {:a 42 :b 43 :c 44})}))
;;   yields:
;; {:value "{:a 42, :c 44, :b 43}", :updated nil, :created #inst "2014-05-16T05:38:53.928635000-00:00", :id 6}


;; http://sqlkorma.com/docs#entities
;; TODO: move to verb.clj or similar: model-type stuff.
(declare question question-submit qsubmit student-test tsubmit vc_user verb vgroup)

(defentity question
  (pk :id)
  (belongs-to student-test {:fk :test})
  (entity-fields :english :italian))

(defentity student-test
  (table :test)
  (pk :id)
  (has-many question))

(defentity test-submit
  (pk :id)
  (has-many question-submit)
  (entity-fields :test :user))

(defentity question-submit
  (pk :id)
  (entity-fields :answer :test-submit :question))

(defentity verb
  (pk :id)
  (entity-fields :value))

(defentity vgroup
  (pk :id)
  (has-many verb))

(def key-to-table
  {:question question
   :question-submit qsubmit
   :tag vgroup
   :test student-test
   :test-submit tsubmit
   :user vc_user
   :verb verb})

(defn keyword-to-table [collection-as-key]
  "Map a keyword representing a collection to a PostgreSQL table. In the future, a collection
might even represent arbitrary SQL such as a join of multiple tables or a SELECT .. WHERE
on a table."
  (let [table (key-to-table collection-as-key)]
    (if table table
        (throw (.Exception "don't know what table this collection is: " collection-as-key)))))

(def collection-update
  ;; TODO: add a default to be used if no matching key in this map.
  {:question (fn [modify-with id]
               (update question
                       (set-fields modify-with)
                       (where {:id id})))
   :question-submit (fn [modify-with id]
                      (update (key-to-table :question-submit)
                              (set-fields modify-with)
                              (where {:id id})))
   :test (fn [modify-with id]
           (update student-test
                   (set-fields modify-with)
                   (where {:id id})))
   :test-submit (fn [modify-with id]
                  (update (key-to-table :test-submit)
                          (set-fields modify-with)
                          (where {:id id})))
   :verb (fn [modify-with id]
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
            (log/warn (str "ignoring update of tag collection with modify-with: " modify-with " and id: " id))))})


(defn jdbc2joda [time]
  (c/from-long (.getTime time)))

(def do-each-row
  ;; return the 'interesting' parts of a row as a map.
  ;; TODO: add a default to be used if no matching key in this map.
  {

   :question (fn [row]
               (merge
                {:created (jdbc2joda (:created row))}
                {:_id (:id row)}
                (if (:updated row)
                  {:updated (jdbc2joda (:updated row))}
                  (log/warn (str "no updated value found for row:" row)))
                (reduce #(dissoc %1 %2) row
                        '(:_id :updated :created))))

   :question-submit (fn [row]
                      (merge
                       {:_id (:id row)}
                       {:created (jdbc2joda (:created row))}
                       
                       (reduce #(dissoc %1 %2) row
                               '(:_id :updated :created))))

   :tag (fn [row] ;; for the vgroup table, it's simpler: simply convert :id to :_id.
          (merge
           {:_id (:id row)}
           {:verbs (if (nil? (:verbs row))
                     []
                     (vec (.getArray (:verbs row))))}
           (dissoc (dissoc row :id)
                   :verbs)))

   :test (fn [row]
          (merge
           {:_id (:id row)}
           {:created (jdbc2joda (:created row))}

           (reduce #(dissoc %1 %2) row
                   '(:_id :updated :created))))

   :test-submit (fn [row]
                  (merge
                   {:_id (:id row)}
                   {:created (jdbc2joda (:created row))}
                   
                   (reduce #(dissoc %1 %2) row
                           '(:_id :updated :created))))

   :verb (fn [row] ;; for the verb table, parse the :value column into a map, and then
           ;; merge with the other non-:value columns, and underscore the id.
           (merge
            (read-string (:value row))
            {:_id (:id row)}

            ;; .getTime: java.sql.Timestamp -> long
            ;; from-long: long -> Joda DateTime.
            {:created (jdbc2joda (:created row))}

            (reduce #(dissoc %1 %2) row
                    (list :created :id :value))))})


;; http://sqlkorma.com/docs#db
(def workstation (postgres {:db "verbcoach"
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
        (= postgres_env "workstation")
        workstation
        true
        nil))
;;        (throw (Exception. (str "POSTGRES_ENV was not defined in your environment.")))))

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
    (if-let [collection-map-function
             (collection do-each-row)]
      (map collection-map-function
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
                 (select table)))))

      ;; else,no collection-mapping function.
      (do
        (log/warn (str "no collection-mapping function for " collection))
        (if the-where
          (if (collection table-to-filter)
            (filter (fn [row]
                      ((collection table-to-filter)
                       row the-where))
                           (select table))
            (select table
                    (where the-where)))
          (select table))))))

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
;; TODO: convert to a fn and allow for defaults if no key for a given table.
(def insert-values
  {:question (fn [add-with]
               add-with)
   :tag (fn [add-with]
          add-with)
   :test (fn [add-with]
           add-with)
   :verb (fn [add-with]
           {:value (str add-with)})})

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



