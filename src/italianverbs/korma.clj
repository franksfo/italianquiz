(ns italianverbs.korma
  (:use [korma db core]))

;; example stuff that works:

;; (insert verb (values {:value (str {:a 42 :b 43 :c 44})}))
;;   yields:
;; {:value "{:a 42, :c 44, :b 43}", :updated nil, :created #inst "2014-05-16T05:38:53.928635000-00:00", :id 6}

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

;; http://sqlkorma.com/docs#entities
(declare verb vgroup)

(defentity verb
  (pk :id)
  (entity-fields :value))

(defentity vgroup
  (pk :id)
  (has-many verb))

(defn keyword-to-table [collection-as-key]
  (cond
   (= collection-as-key :verb) verb
   (= collection-as-key :tag) vgroup
   true
   (throw (.Exception "don't know what table this is: " collection-as-key))))

(defn fetch [collection & [ the-where ]]
  "select from collection; might take an id. For each returned row, return simply the 'value' column as a clojure map, but merge it with an extra field for the primary key (id)."
  (let [the-where
        (if the-where the-where nil)
        id (if the-where (:_id the-where))]
    (if id
      (let [row (first
                 (select (keyword-to-table collection)
                         (where {:id id})))]
        (if row (read-string (:value row))))
      (map (fn [row] 
             ;; here we merge primary key column into returned map.
             (merge {:_id (:id row)} 
                    (read-string (:value row))))
           (if the-where
             (select (keyword-to-table collection)
                     (where {:value (str the-where)}))
             (select (keyword-to-table collection)))))))

(defn fetch-and-modify [collection id & [modify-with remove?]]
  "modify-with: map of key/value pairs with which to modify row whose id is given in params."
  (if remove?
    (delete (keyword-to-table collection)
            (where {:id id}))

    ;; remove=false: do update instead.
    (update (keyword-to-table collection)
            (where {:id id}))))

(defn insert! [collection & [add-with]]
  "args are collection and map of key/value pairs with which to initialize new row. we simply serialize the map with (str). Any embedded objects will be lost due to serialization, so map should be only of atoms (strings, numbers, etc) or vectors of atoms (vectors of vectors should work too, provided they are eventually atoms at the leaves)"
  ;; We remove :created and :updated, if any, since we'll let postgres handle
  ;; those through its own constraints and triggers.
  (let [add-with (dissoc (dissoc add-with :created) :updated)]
    (insert (keyword-to-table collection)
            (values {:value (str add-with)}))))

(defn object-id [ & args ]
  "compare with mongo/object-id, which uses mongo ids."
  (first args))

(defn primary-key [map]
  map)


