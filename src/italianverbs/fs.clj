(ns italianverbs.fs
  (:use [hiccup core page-helpers]
        [clojure.set]
        [rdutest])
  (:require
   [italianverbs.fs :as fs]
   [clojure.string :as string]
   [clojure.contrib.string :as stringc]
   [clojure.contrib.str-utils2 :as str-utils]))

;; a library of aliases for common get- type actions.
(defn get-head [sign]
  (if (get sign :head)
    (get-head (get sign :head))
    sign))

(defn fetch-criteria [vp subject verb-head]
  {:cat :verb
   :infl (get vp :infl)
   :person (get (get-head subject) :person)
   :number (get (get-head subject) :number)
   :root.english (get (get-head verb-head) :english)})

(defn get-root-head [sign]
  (cond
   (get sign :head)
   (get-root-head (get sign :head))
   true
   sign))


(defn- collect-values [maps keys]
  (let [key (first keys)]
    (if key
      (merge
       {key (mapcat (fn [eachmap]
                      (let [val (get eachmap key :notfound)]
                        (if (and (not (= val :notfound))
                                 (not (= val nil)))
                          (list val))))
                    maps)}
       (collect-values maps (rest keys))))))

(defn collect-values-with-nil [maps keys]
  (let [key (first keys)]
    (if key
      (merge
       {key (mapcat (fn [eachmap]
                      (let [val (get eachmap key :notfound)]
                        (if (not (= val :notfound))
                          (list val))))
                    maps)}
       (collect-values-with-nil maps (rest keys))))))

(defn- merge-atomically-like-core [values]
  (last values))

(defn merge-values-like-core [values]
  (let [value (first values)]
    (if value
      (if (or (= (type value) clojure.lang.PersistentArrayMap)
              (= (type value) clojure.lang.PersistentHashMap))
        (merge
         value
         (merge-values-like-core (rest values)))
        (merge-atomically-like-core values))
        {})))

(defn merge-values-like-core-nil-override [values]
  (let [value (first values)]
    (if value
      (if (or (= (type value) clojure.lang.PersistentArrayMap)
              (= (type value) clojure.lang.PersistentHashMap))
        (let [rest-of (merge-values-like-core-nil-override (rest values))]
          (if (= nil rest-of)
            nil
            (merge
             value
             rest-of)))
        (merge-atomically-like-core values))
      (if (and (= (.size values) 1)
               (= value nil))
        nil {}))))

(defn- merge-r-like-core [collected-map keys]
  "merge a map where each value is a list of values to be merged for that key."
  (let [key (first keys)]
    (if key
      (let [merged-values (merge-values-like-core (get collected-map key))]
        (if (not (= merged-values {}))
          (merge
           {key merged-values}
           (merge-r-like-core collected-map (rest keys)))
          (merge-r-like-core collected-map (rest keys))))
      {})))

(defn- merge-r-like-core-nil-override [collected-map keys]
  "merge a map where each value is a list of values to be merged for that key."
  (let [key (first keys)]
    (if key
      (let [merged-values (merge-values-like-core-nil-override (get collected-map key))]
        (if (not (= merged-values {}))
          (merge
           {key merged-values}
           (merge-r-like-core-nil-override collected-map (rest keys)))
          (merge-r-like-core-nil-override collected-map (rest keys))))
      {})))

(defn- merge-atomically [values]
  (let [value (first values)]
    (if value
      (if (second values)
        (if (= (first values) (second values))
          (merge-atomically (rest values))
          :fail)
        value))))

(defn- merge-values [values]
  (let [value (first values)]
    (if value
      (if (or (= (type value) clojure.lang.PersistentArrayMap)
              (= (type value) clojure.lang.PersistentHashMap))
        (merge
         (first values)
         (merge-values (rest values)))
        (merge-atomically values))
        {})))

(defn- merge-r [collected-map keys]
  "merge a map where each value is a list of values to be merged for that key."
  (let [key (first keys)]
    (if key
      (merge
       {key (merge-values (get collected-map key))}
       (merge-r collected-map (rest keys)))
      {})))

(defn merge [& maps]
  "like clojure.core/merge, but works recursively. also lacks optimization, testing"
  (let [keyset (set (mapcat #'keys maps))]  ;; the set of all keys in all of the maps.
    (merge-r (collect-values maps keyset)
             (seq keyset))))

(defn union-keys [maps]
  (set (mapcat #'keys maps)))

;; TODO: use merge-with http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/merge-with
;; TODO: it's misleading to say it's 'like core' when behavior differs w.r.t. nil."
;;  (merge-nil-override is more 'like core' in this respect).
(defn merge-like-core [& maps]
  "like clojure.core/merge, but works recursively, and works like it also in that the later values wins (see test 'atomic-merge' for usage.)"
  (let [keyset (union-keys maps)
        values (collect-values maps keyset)]
    (merge-r-like-core values (seq keyset))))

(defn merge-nil-override [& maps]
  "like clojure.core/merge, but works recursively, and works like it also in that later values win (see test 'nil-should-override'), even if that value is nil."
  (let [keyset (union-keys maps)
        values (collect-values-with-nil maps keyset)]
    (merge-r-like-core-nil-override values (seq keyset))))

;; EXACTLY THE SAME AS (mergec):
;; (until i learn to write wrappers).
(defn m [& maps]
  "like clojure.core/merge, but works recursively, and works like it also in that the last value wins (see test 'atomic-merge' for usage.)"
  (let [keyset (union-keys maps)
        values (collect-values maps keyset)]
    (merge-r-like-core values (seq keyset))))

;; similar to clojure core's get-in, but supports :ref as a special feature.
(defn get-path [fs path & [root]]
  (let [root (if root root (if (:root fs) (:root fs) fs))
        feat (first path)
        val (get fs (first path))
        ref (if val (get val :ref))]
    (if (not (= ref nil))
      ;; a ref: resolve ref.
      (get-path root (concat ref (rest path)))
      ;; else, not a ref.
      (if (> (.size path) 0)
        (get-path (get fs (first path))
                  (rest path)
                  root)
        (if (or (= (type fs) clojure.lang.PersistentArrayMap)
                (= (type fs) clojure.lang.PersistentHashMap))
          (m {:root root} fs)
          fs)))))

(defn mergec [& maps]
  (merge-like-core (list maps)))

(defn merge-and-apply [maps]
  "merge maps, and then apply the function (:fn merged) to the merged map."
  (let [merged
        (eval `(fs/m ~@maps))
        fn (:fn merged)
        eval-fn (if (and fn (= (type fn) java.lang.String))
                  (eval (symbol fn)) ;; string->fn (since a fn cannot (yet) be serialized in persistent db)
                  fn)] ;; otherwise, assume it's a function.
    (if (:fn merged)
      (fs/m merged
            (apply eval-fn
                   (list merged)))
      maps)))

;;  needed below (for testing only) for :merge-and-apply-test-with-fn-as-string test.
(defn myfn [map]
  (fs/m map
        {:a (+ 1 (:a map))
         :foo "bar"}))

(defn set-paths [fs paths val]
  (let [path (first paths)]
    (if path
      (set-paths (assoc-in fs path val) (rest paths) val)
      fs)))

(defn encode-refs [fs inv-fs]
   ;; for each (keys inv-fs)
   ;; assoc-in fs path string-rep
  (if (first inv-fs)
    (let [ref-pair (first inv-fs)]
      ;; ref-pair: <string-representation-of-ref,
      ;;            <value, set-of-pairs-that-point-to-this-reference> >
      (let [encoded-ref (first ref-pair)
            value (first (second ref-pair))
            paths (second (second ref-pair))]
       (encode-refs
        (set-paths fs paths encoded-ref)
        (rest inv-fs))))
     fs))

(def *exclude-keys* #{:_id})

(defn pathify [fs & [prefix]]
"Transform a map into a map of paths/value pairs,
 where paths are lists of keywords, and values are atomic values.
 e.g.:
 {:foo {:bar 42, :baz 99}} =>  { { (:foo :bar) 42}, {(:foo :baz) 99} }
The idea is to map the key :foo to the (recursive) result of pathify on :foo's value."
(mapcat (fn [kv]
          (let [key (first kv)
                val (second kv)]
            (if (not (contains? *exclude-keys* key))
              (if (or (= (type val) clojure.lang.PersistentArrayMap)
                      (= (type val) clojure.lang.PersistentHashMap))
                (pathify val (concat prefix (list key)))
                (list (concat prefix (list key)) val)))))
        fs))

(defn map-pathify [pathified]
  (let [first (first pathified)
        second (second pathified)]
    (if (and first second)
      (merge {first second}
             (map-pathify (rest (rest pathified)))))))


(defn ref-invert [input]
  "turn a map<P,V> into an inverted map<V,[P]> where every V has a list of what paths P point to it."
  (let [input (map-pathify (pathify input))]
    (let [keys (keys input) ;; the set of all paths that point to any value.
          vals (set (vals input))] ;; the set of all values pointed to by any path
      (let [inverted-list
            (map (fn [val] ;; for each val..
                   (if (= (type val) clojure.lang.Ref)
                     (list (str val)
                           (list @val
                                 (set 
                                  (mapcat (fn [key]  ;; ..find all paths that point to val.
                                            (if (= (get input key)
                                                   val)
                                              (list key)))
                                          keys))))))
                 vals)]
        (zipmap
         (mapcat (fn [pair] (if (not (nil? (first pair))) (list (first pair))))
                 inverted-list)
         (mapcat (fn [pair] (if (not (nil? (second pair))) (list (second pair))))
                 inverted-list))))))

(defn serialize [fs]
  (let [inv (ref-invert fs)]
    (merge (encode-refs fs (ref-invert fs))
           {:refs inv})))

(defn set-all-paths [fs paths val]
  (let [path (first paths)]
    (if path
      (set-all-paths
       (assoc-in fs path val)
       (rest paths)
       val)
      fs)))

(defn deserialize [fs & [refs]]
  "apply refs as a list of references to be created in fs."
  (let [refs (if refs refs (vals (:refs fs)))]
    (let [the-ref (first refs)]
      (if the-ref
        (let [val (first the-ref)
              paths (second the-ref)
              shared-val (ref val)]
          (deserialize
           (set-all-paths fs paths shared-val)
           (rest refs)))
        (dissoc fs :refs))))) ;; finally, remove :ref key since it's no longer needed.

(def tests
  (list
   (rdutest
    "Recursive merge of 3 maps."
    (let [map1 {:foo {:bar 99}}
          map2 {:foo {:baz 42}}
          map3 {:biff 12}]
      (m map1 map2 map3))
    (fn [merge-result]
      ;; result should look like:
      ;; {:foo {:bar 99
      ;;        :baz 42}
      ;;  :biff 12}}
      (and
       (= (:bar (:foo merge-result)) 99)
       (= (:baz (:foo merge-result)) 42)
       (= (:biff merge-result) 12)))
    :recursive-merge)
   
   (rdutest
    "Recursive merge of 3 maps, tested with (get-path)"
    (let [map1 {:foo {:bar 99}}
          map2 {:foo {:baz 42}}
          map3 {:biff 12}]
      (merge map1 map2 map3))
    (fn [merge-result]
      (and
       (= (get-path merge-result '(:foo :bar)) 99)
       (= (get-path merge-result '(:foo :baz)) 42)))
    :recursive-merge-with-paths)

   (rdutest
    "Testing that merge(v1,v2)=fail if v1 != v2."
    (merge {:foo 42} {:foo 43})
    (fn [result]
      (= (:foo result) :fail))
    :atomic-fail)
   
   (rdutest
    "Testing that merge-like-core(v1,v2)=v2 (overriding works)."
    (merge-like-core {:foo 42} {:foo 43})
    (fn [result]
      (= (:foo result) 43))
    :atomic-merge)

   (rdutest
    "Ignore nils in values."
    (merge-like-core {:foo true} {:foo nil})
    (fn [result]
      (= (:foo result) true))
    :ignore-nil-values)

   (rdutest
    "Ignore nils in values."
    (merge-like-core {:foo nil} {:foo nil})
    (fn [result]
      (= result {}))
    :ignore-nil-values-2)
   
   (rdutest
    "{:a 42}{:a nil} => {:a nil}"
    (apply fs/merge-nil-override (list {:a 42}{:a nil}))
    (fn [map]
      (= (:a map) nil))
    :nil-should-override-atomic)
   
   (rdutest
    "{:a {:foo 42}}{:a nil} => {:a nil}"
    (apply fs/merge-nil-override (list {:a {:foo 42}} {:a nil}))
    (fn [map]
      (= (:a map) nil))
    :nil-should-override)
   
   (rdutest
    "(apply the :fn function of a map on the result of running fs/m)"
    (merge-and-apply
     (list 
      {:a 42 :fn (fn [map]
                   (fs/m map
                         {:a (+ 1 (:a map)) :foo "bar"}))}
      {:b 99}))
    (fn [map]
      (and (= (:foo map) "bar")
           (= (:a 43))))
    :merge-and-apply-test)
   
   (rdutest
    "(apply the :fn value (after converting from a string to a function) of a map on the result of running fs/m)"
    (merge-and-apply (list {:a 42 :fn "myfn"}
                           {:b 99}))
    (fn [map]
      (and (= (:foo map) "bar")
           (= (:a 43))))
    :merge-and-apply-test-with-fn-as-string)

   (rdutest
    "resolve :ref pointers: (:b :c) should be identical to (:a)."
    {:a {:ref '(:b :c)}
     :b {:c {:foo :bar}}}
    (fn [map]
      (= (get-path map '(:b :c))
         (get-path map '(:a))))
    :resolve-refs)

   (rdutest
    "ref chain: ref->ref1->val"
    {:a {:ref '(:b :c)}
     :b {:c {:ref '(:d :e)}}
     :d {:e 42}}
    (fn [map]
      (= (get-path map '(:a))
         42))
    :double-ref)

   ;; ref chain with referential equality: make sure (=) works as expected:
   ;; if (= (get-path x (P1 :ref) P2)), then (= (get-path x P1) (get-path y P2))
   (rdutest
    "ref chain: ref->ref1->val"
    {:a {:ref '(:b :c)}
     :b {:c {:ref '(:d :e)}}
     :d {:e {:f 42}}}
    (fn [map]
      (= (get-path map '(:a))
         (get-path map '(:d :e))))
    :ref-equality)

   ;; (same as previous path, but with end of path an atom)
   (rdutest
    "ref chain: ref->ref1->val"
    {:a {:ref '(:b :c)}
     :b {:c {:ref '(:d :e)}}
     :d {:e {:f 42}}}
    (fn [map]
      (= (get-path map '(:a :f))
         (get-path map '(:d :e :f))
         42))
    :ref-equality-atom)

   ;; P1 = '(F1 F2) => (get-path P1) == (get-path (get-path F1) F2)
   (rdutest
    "(get-path (get-path))"
    {:a {:ref '(:b :c)}
     :b {:c {:ref '(:d :e)}}
     :d {:e {:f 42}}}
    (fn [map]
      (= (get-path map '(:d :e))
         (get-path (get-path map '(:b)) '(:c))))
    :get-path-nested)

   ;; P1 = '(F1 F2 F3) => (get-path P1) == (get-path (get-path (get-path F1) F2) F3)
   (rdutest
    "(get-path (get-path))"
    {:a {:ref '(:b :c)}
     :b {:c {:ref '(:d :e)}}
     :d {:e {:f 42}}}
    (fn [map]
      (= (get-path map '(:d :e :f))
         (get-path (get-path (get-path map '(:b)) '(:c)) '(:f))
         42))
    :get-path-nested-atom)

   ;; test map inversion
   ;; path (:a :b) points to a reference, whose value is an integer, 42.
   ;; path (:c) also points to the same reference.
   ;;
   ;;[ :a  [:b [1] 42]
   ;;  :c  [1]]
   ;;
   ;; => {#<Ref: 42> => {(:a :b) (:c)}
   ;;                               
   (rdutest "map inversion"
            (let [myref (ref 42)
                  fs {:a {:b myref}
                      :c myref}]
              (ref-invert fs))
            (fn [result]
              (and (= (.size result) 1)
                   (= (type (first (keys result)))
                      java.lang.String)
                   (= (first (first (vals result)))
                      42)
                   (= (.size (vals result)) 1)
                   (let [paths (second (first (vals result)))]
                     (and (= (.size paths) 2)
                          (or (= (first paths) '(:a :b))
                              (= (first paths) '(:c)))
                          (or (= (second paths) '(:a :b))
                              (= (second paths) '(:c)))))))
            :map-inversion)

   ;; 
   (rdutest "serialization"
            (let [myref (ref 42)
                  fs {:a {:b myref}
                      :c myref}
                  inverted (ref-invert fs)]
              (encode-refs fs inverted))
            (fn [result]
              (and 
               (= (type (get-in result '(:a :b))) java.lang.String)
               (= (type (get-in result '(:c))) java.lang.String)
               (= (get-in result '(:a :b))
                  (get-in result '(:c)))))
            :serialization)

      (rdutest "deserialization"
            (let [myref (ref 42)
                  fs {:a {:b myref}
                      :c myref}
                  serialized (serialize fs)]
              (list fs (deserialize serialized)))
            (fn [result] ;; fs and our deserialized fs should be isomorphic.
              (let [original (first result)
                    copy (second result)]
                (and (= (get-in original '(:a :b))
                        (get-in original '(:c)))
                     (= (get-in copy '(:a :b))
                        (get-in copy '(:c))))))
            :deserialization)))


