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

(defn pathify [fs & [prefix]]
"Transform a map into a map of paths/value pairs,
 where paths are lists of keywords, and values are atomic values.
 e.g.:
 {:foo {:bar 42, :baz 99}} =>  { { (:foo :bar) 42}, {(:foo :baz) 99} }
The idea is to map the key :foo to the (recursive) result of pathify on :foo's value."
(println (str "pathify with: " fs)))

;; TODO: very inefficient due to recursive uniq call:
;; instead, sort first and then remove (adjacent) dups.
;; most know how to order references.
(defn uniq [vals]
  "remove duplicate values from vals."
  (let [val (first vals)]
    (if val
      (cons val
            (filter (fn [otherval] (not (= otherval val)))
                    (uniq (rest vals)))))))

(defn paths-to-value [map value path]
  (if (= map value) (list path)
      (if (= (type map) clojure.lang.Ref)
        (paths-to-value @map value path)
        (if (or (= (type map) clojure.lang.PersistentArrayMap)
                (= (type map) clojure.lang.PersistentHashMap))
          (mapcat (fn [key]
                    (paths-to-value (get map key) value (concat path (list key))))
                  (keys map))))))

(defn all-refs [input]
  (if input
    (if (= (type input) clojure.lang.Ref)
      (cons input
            (all-refs @input))
      (if (or (= (type input) clojure.lang.PersistentArrayMap)
              (= (type input) clojure.lang.PersistentHashMap))
        ;; TODO: fix bug here: vals resolves @'s
        (concat
         (mapcat (fn [key]
                   (let [val (get input key)]
                     (if (= (type input) clojure.lang.Ref)
                       (list val))))
                 input)
         (all-refs (vals input)))
        (if (and (seq? input)
                 (first input))
          (concat
           (all-refs (first input))
           (all-refs (rest input))))))))
  
(defn skeletize [input-val]
  (if (or (= (type input-val) clojure.lang.PersistentArrayMap)
          (= (type input-val) clojure.lang.PersistentHashMap))
    (zipmap (keys input-val)
            (map (fn [val]
                   (if (= (type val) clojure.lang.Ref)
                     :PH
                     (if (or (= (type val) clojure.lang.PersistentArrayMap)
                             (= (type val) clojure.lang.PersistentHashMap))
                       (skeletize val)
                       val)))
                 (vals input-val)))
    input-val))

;; TODO s/map/input-map/
;; TODO: merge or distinguish from all-refs (above)
(defn get-refs [input-map]
  (uniq (all-refs input-map)))

;; TODO s/map/input-map/
(defn skels [input-map]
  "create map from reference to their skeletons."
  (let [refs (get-refs input-map)]
    (zipmap
     refs
     (map (fn [ref]
            (skeletize @ref))
          refs))))
          
(defn ref-skel-map [input-map]
  "create map from (ref=>skel) to paths to that ref."
  (let [refs (get-refs input-map)
        skels (skels input-map)]
    (zipmap
     (map (fn [ref]
            {:ref ref
             :skel (get skels ref)})
          refs)
     (map (fn [eachref]
            (paths-to-value input-map eachref nil))
          refs))))

(defn ser-db [input-map]
  (let [refs (get-refs input-map)
        skels (skels input-map)]
    (ref-skel-map input-map)))

;; (((:a :c) (:b :c) (:d))
;;  ((:a) (:b))
;;  nil)
;;     =>
;; {((:a :c) (:b :c) (:d)) => 2
;;  ((:a)    (:b))         => 1
;;  nil                    => 0
;; }
(defn max-lengths [serialization]
  (let [keys (keys serialization)]
    (zipmap
     keys
     (map (fn [paths]
            (if (nil? paths) 0
                (apply max (map (fn [path] (if (nil? path) 0 (.size path))) paths))))
          keys))))

(defn sort-by-max-lengths [serialization]
  (let [max-lengths (max-lengths serialization)]
    (sort (fn [x y] (< (second x) (second y)))
          max-lengths)))

(defn sort-shortest-path-ascending-r [serialization path-length-pairs]
  (if (first path-length-pairs)
    (let [path-length-pair (first path-length-pairs)
          paths (first path-length-pair)
          max-length (second path-length-pair)]
      (cons
       (list paths
             (get serialization paths))
       (sort-shortest-path-ascending-r serialization (rest path-length-pairs))))))

(defn ser-intermed [input-map]
  (let [top-level (skeletize input-map)
        rsk (ref-skel-map input-map)
        sk (map (fn [ref-skel]
                  (:skel ref-skel))
                (keys rsk))]
    (merge
     {nil (skeletize input-map)}
     (zipmap
      (vals rsk)
      sk))))     

;(defn deser-1 [pathset value]
;  "for all paths in pathset, set them to the value."
;  {}
;  )

;(defn deser-r [serialized]
;  (if (first serialized)
;    (merge
;     (deser-1 (first serialized))
;     (deser-r (rest serialized)))))

(defn create-shared-values [serialized]
  (map (fn [paths-vals]
         (let [val (second paths-vals)]
           ;; TODO: why/why not do copy val rather than just val(?)
           (ref val)))
       serialized))

(defn create-path-in [path value]
  "create a path starting at map through all keys in map."
  (if (first path)
    (if (rest path)
      (let [assigned (create-path-in (rest path) value)]
        {(first path) assigned})
      {(first path) value})
    value))
       
;  {:a {:b {:c {:d {:e value}}}}})


(defn deser [serialized]
  (let [base (second (first serialized))]
    (apply merge
           (cons base
                 (flatten
                  (map (fn [paths-val]
                         (let [paths (first paths-val)
                               val (ref (second paths-val))]
                           (map (fn [path]
                                  (create-path-in path val))
                                paths)))
                       (rest serialized)))))))

(defn ser [input-map]
  (let [ser (ser-intermed input-map)]
    (sort-shortest-path-ascending-r ser (sort-by-max-lengths ser))))

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
