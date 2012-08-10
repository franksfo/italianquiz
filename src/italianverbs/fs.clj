(ns italianverbs.fs
  (:use [clojure.set])
  (:require
   [italianverbs.fs :as fs] ;; needed maybe by the (eval fs/..) stuff below.
   [clojure.string :as string]
   [clojure.core :as core]
   [clojure.contrib.string :as stringc]
   [clojure.contrib.str-utils2 :as str-utils]))

(defn get-head [sign]
  (if (get sign :head)
    (get-head (get sign :head))
    sign))

;; TODO: need tests.
(defn get-in-r [map keys]
  "same as clojure.core (get-in), but it resolves references if need be."
  (let [result 
        (if (first keys)
          (let [result (get map (first keys))]
            (if (= (type result) clojure.lang.Ref)
              (get-in-r @result (rest keys))
              (get-in-r result (rest keys))))
          map)]
    (if (= (type result) clojure.lang.Ref)
      @result
      result)))

(defn get-r [map key]
  "same as clojure.core (get), but it resolves references if need be."
  (let [result (get map key)]
    (if (= (type result) clojure.lang.Ref)
      @result
      result)))

(defn get-root-head [sign]
  (cond
   (get sign :head)
   (get-root-head (get sign :head))
   true
   sign))

(defn unify [& args]
  (let [val1 (first args)
        val2 (second args)]
    (cond

     (= (.count args) 1)
     (first args)
     
     (and (or (= (type val1) clojure.lang.PersistentArrayMap)
              (= (type val1) clojure.lang.PersistentHashMap))
          (or (= (type val2) clojure.lang.PersistentArrayMap)
              (= (type val2) clojure.lang.PersistentHashMap)))
     (reduce #(merge-with unify %1 %2) args)

     (and 
      (= (type val1) clojure.lang.Ref)
      (not (= (type val2) clojure.lang.Ref)))
     (do (dosync
          (alter val1
                 (fn [x] (unify @val1 val2))))
         val1)

     (and 
      (= (type val2) clojure.lang.Ref)
      (not (= (type val1) clojure.lang.Ref)))
     (do (dosync
          (alter val2
                 (fn [x] (unify val1 @val2))))
         val2)

     (and 
      (= (type val1) clojure.lang.Ref)
      (= (type val2) clojure.lang.Ref))
      (do (dosync
           (alter val1
                  (fn [x] (unify @val1 @val2))))
          (dosync
           (alter val2
                  (fn [x] @val1)))
       val1)
     
     (not (= :notfound (:not val1 :notfound)))
     (let [result (unify (:not val1) val2)]
       (if (= result :fail)
         val2
         :fail))

     (not (= :notfound (:not val2 :notfound)))
     (let [result (unify val1 (:not val2))]
       (if (= result :fail)
         val1
         :fail))

     (or (= val1 :fail)
         (= val2 :fail))
     :fail

     (= val1 :top) val2
     (= val2 :top) val1

     ;; these two rules are unfortunately necessary because of mongo/clojure storage of keywords as strings.
     (= val1 "top") val2
     (= val2 "top") val1

     ;; :foo,"foo" => :foo
     (and (= (type val1) clojure.lang.Keyword)
          (= (type val2) java.lang.String)
          (= (string/replace-first (str val1) ":" "") val2))
     val1

     ;; "foo",:foo => :foo
     (and (= (type val2) clojure.lang.Keyword)
          (= (type val1) java.lang.String)
          (= (string/replace-first (str val2) ":" "") val1))
     val2

     (= val1 val2) val1

     :else :fail)))

(defn merge [& args]
  (let [val1 (first args)
        val2 (second args)]
    (cond

     (= (.count args) 1)
     (first args)

     (and (or (= (type val1) clojure.lang.PersistentArrayMap)
              (= (type val1) clojure.lang.PersistentHashMap))
          (or (= (type val2) clojure.lang.PersistentArrayMap)
              (= (type val2) clojure.lang.PersistentHashMap)))
     (reduce #(merge-with merge %1 %2) args)

     (and 
      (= (type val1) clojure.lang.Ref)
      (not (= (type val2) clojure.lang.Ref)))
     (do (dosync
          (alter val1
                 (fn [x] (merge @val1 val2))))
         val1)

     (and 
      (= (type val2) clojure.lang.Ref)
      (not (= (type val1) clojure.lang.Ref)))
     (do (dosync
          (alter val2
                 (fn [x] (merge val1 @val2))))
         val2)

     (and 
      (= (type val1) clojure.lang.Ref)
      (= (type val2) clojure.lang.Ref))
      (do (dosync
           (alter val1
                  (fn [x] (merge @val1 @val2))))
          val1)

     (not (= :notfound (:not val1 :notfound)))
     (let [result (unify (:not val1) val2)]
       (if (= result :fail)
         val2
         :fail))

     (not (= :notfound (:not val2 :notfound)))
     (let [result (unify val1 (:not val2))]
       (if (= result :fail)
         val1
         :fail))

     (or (= val1 :fail)
         (= val2 :fail))
     :fail

     (= val1 :top) val2
     (= val2 :top) val1
     (= val1 nil) val2

     ;; note difference in behavior between nil and :nil!:
     ;; (nil is ignored, while :nil! is not).
     ;; (merge 42 nil) => 42
     ;; (merge 42 :nil!) => :nil!
     (= val2 nil) val1
     (= val2 :nil!) val2
     (= val2 "nil!") val2 ;; needed because of translation error from mongod to clojure.

     (= val1 val2) val1

     :else ;override with remainder of arguments, like core/merge.
     (apply merge (rest args)))))

(defn unify-and-apply [maps]
  "merge maps, and then apply the function (:fn merged) to the merged map."
  (let [merged
        (eval `(fs/unify ~@maps))
        fn (:fn merged)
        eval-fn (if (and fn (= (type fn) java.lang.String))
                  (eval (symbol fn)) ;; string->fn (since a fn cannot (yet) be 
                  fn)] ;; otherwise, assume it's a function.
    (if (:fn merged)
      (fs/unify merged
            (apply eval-fn
                   (list merged)))
      maps)))

(defn set-paths [fs paths val]
  (let [path (first paths)]
    (if path
      (set-paths (assoc-in fs path val) (rest paths) val)
      fs)))

(defn encode-refs [fs inv-fs]
  (if (first inv-fs)
    (let [ref-pair (first inv-fs)]
      ;; ref-pair: <value, set-of-pairs-that-point-to-this-reference> >
      (let [value (first ref-pair)
            paths (second ref-pair)]
       (encode-refs
        (set-paths fs paths value)
        (rest inv-fs))))
     fs))

(def *exclude-keys* #{:_id})

(defn deref-map [input]
  input)
                                        ;  {:foo input})

(defn pathify [fs & [prefix]]
"Transform a map into a map of paths/value pairs,
 where paths are lists of keywords, and values are atomic values.
 e.g.:
 {:foo {:bar 42, :baz 99}} =>  { { (:foo :bar) 42}, {(:foo :baz) 99} }
The idea is to map the key :foo to the (recursive) result of pathify on :foo's value."
(println (str "pathify with: " fs)))

(defn vals-r [map]
  "return all non-map values from a map, recursively"
  (let [vals (vals map)]
    (map (fn [val]
           (if (or (= (type val) clojure.lang.PersistentArrayMap)
                   (= (type val) clojure.lang.PersistentHashMap))
             (flatten (vals-r val))
             val))
         vals)))

(defn uniq [vals]
  "remove duplicate values from vals."
  (let [val (first vals)]
    (if val
      (cons val
            (filter (fn [otherval] (not (= otherval val)))
                    (rest vals))))))

(defn rfv [map]
  (let [keys (keys map)
        refs (uniq (vals-r map))]
    {(first refs) '((a)(b))
     nil {:a :ph :b :ph}}))
;    {(:a map) @(:a map)}))

;(mapcat (fn [kv]
;          (let [key (first kv)
;                val (second kv)
;                deref-val (if (= clojure.lang.Ref (type val))
;                            @val
;                            val)]
;            
;;            (println (str "key: " key))
;;            (println (str "val: " val))
;;            (println (str "deref: " deref))
;;            (println (str "val type: " (type val)))
;            (if (not (contains? *exclude-keys* key))
;              (if (or (= (type val) clojure.lang.PersistentArrayMap)
;                      (= (type val) clojure.lang.PersistentHashMap))
;                (do
;                  (println (str "first if with val: " val))
;                  (println (str "returning: " (pathify val (concat prefix (list key)))))
;                  (pathify val (concat prefix (list key))))
;                (do
;                  (println (str "first else with val: " val))
;                  (println (str "cond: " (and
;                                          true
;                                          (= (type val) clojure.lang.Ref)
;                                          (or (= (type @val) clojure.lang.PersistentArrayMap)
;                                              (= (type @val) clojure.lang.PersistentHashMap)))))
;                  (if (and
;                     false
;                     (= (type val) clojure.lang.Ref)
;                     (or (= (type @val) clojure.lang.PersistentArrayMap)
;                         (= (type @val) clojure.lang.PersistentHashMap)))
;                  (do
;                    (println (str "(2)key: " key))
;                    (println (str "(2)val: " val))
;                    (println (str "(2)@val: " @val))
;;                    (list (concat prefix (list key)) (pathify @val)))
;                    (map (fn [kv]
;                           (println (str "KV: " (seq kv)))
;                            (concat (concat prefix (list key)) kv))
;                         (list (pathify @val))))
;                                    
;                    
;                  (if (and false (= (type val) clojure.lang.Ref))
;                    (do
;                      (println (str "just dereffing and returning:"
;                                    (list (concat prefix (list key)) @val)))
;                      (list (concat prefix (list key)) @val))
;                    (do
;                      (println "actually got here.")
;                      (list (concat prefix (list key)) val)))))))))
;        fs))

(defn map-pathify [pathified]
  (let [first (first pathified)
        second (second pathified)]
    (if (and first second)
      (core/merge {first second}
                  (map-pathify (rest (rest pathified)))))))

;; This returns a list of key-value pairs <R,[P]>, not a map (despite what annotation below says.)
;; TODO: return a map.
(defn ref-invert [input]
  "turn a map<P,R> into an inverted map<R,[P]> where every ref R has a list of what paths P point to it."
  (let [input (map-pathify (pathify input))]
    (let [keys (keys input) ;; the set of all paths that point to any value.
          vals (set (vals input))] ;; the set of all values pointed to by any path
      (let [inverted-list
            (map (fn [val] ;; for each val..
                   (if (= (type val) clojure.lang.Ref)
                     (list @val
                           (list @val
                                 (set 
                                  (mapcat (fn [key]  ;; ..find all paths that point to val.
                                            (if (= (get input key)
                                                   val)
                                              (list key)))
                                          keys))))))
                 vals)]
         (mapcat (fn [pair] (if (not (nil? (second pair))) (list (second pair))))
                 inverted-list)))))

(defn set-to-list [refs]
  (let [ref (first refs)]
    (if ref
      (cons (list (first ref)
                  (seq (second ref)))
            (set-to-list (rest refs))))))

(defn serialize [fs]
  (let [inv (ref-invert fs)]
    (core/merge (encode-refs fs inv)
                {:refs (set-to-list inv)})))

(defn set-all-paths [fs paths val]
  "for each path in paths, set that path to point to val (so that all such paths will share val)."
  (let [path (first paths)]
    (if path
      (set-all-paths
       (assoc-in fs (map #'keyword path) val)
       (rest paths)
       val)
      fs)))

(defn deserialize [fs & [refs]]
  "apply refs as a list of references to be created in fs."
  (let [refs (if refs refs (:refs fs))]
    (let [the-ref (first refs)]
      (if the-ref
        (let [val (first the-ref)
              paths (second the-ref)
              shared-val (ref val)]
          (deserialize
           (set-all-paths fs paths shared-val)
           (rest refs)))
        (dissoc fs :refs))))) ;; finally, remove :ref key since it's no longer needed.

(defn copy [map]
  (deserialize (serialize map)))
