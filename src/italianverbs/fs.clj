(ns italianverbs.fs
  (:use [clojure.set]
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

(defn collect-values [maps keys]
  (if (> (.size keys) 0)
    (let [key (first keys)]
      (conj
       {key

        (let [do-mapcat
              (mapcat (fn [eachmap]
                        (let [val (get eachmap key :notfound)]
                          (if (and (not (= val :notfound))
                                   (not (= val nil)))
                            (list val))))
                      maps)]
          (if (> (.size do-mapcat) 0) ;; if mapping is an empty list, return nil.
            do-mapcat
            nil))}
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

;; TODO: 'atom' is confusing here because i mean it the sense of
;; simple values like floats or integers, as opposed to sets, maps, or lists.
;; use 'simple' here and elsewhere in this file.
(defn- merge-atomically-like-core [values]
  (if (> (.size values) 1)
    (let [do-rest (merge-atomically-like-core (rest values))]
      (if (= do-rest :top)
        (first values)
        do-rest))
    (first values)))

;; forward declarations:
(declare merge-r-like-core)
(declare merge-r-like-core-nil-override)

(defn merge-values-like-core [values]
  (if (and (not (nil? values)) (not (nil? (seq values))) (> (.size (seq values)) 0))
    (let [value (first values)
          refs (keep (fn [val] (if (= (type val) clojure.lang.Ref) val)) values)
          nonrefs (keep (fn [val] (if (= (type val) clojure.lang.Ref) nil val)) values)]
      (if (> (.size refs) 0)
        ;; return the reference after setting it to the value of its merged values with
        ;; the rest of the items to be merged.
        ;; TODO: all the refs are ignored except first: should point the others (rest refs) to the first.
        (let [do-sync (dosync
                       (alter (first refs)
                              (fn [x] (merge-values-like-core (cons @(first refs) nonrefs)))))]
          (first refs))
        (if value
          (if (or (= (type value) clojure.lang.PersistentArrayMap)
                  (= (type value) clojure.lang.PersistentHashMap))
            (let [maps values
                  keys (set (mapcat #'keys maps))
                  collect-values (collect-values maps keys)]
              (merge-r-like-core collect-values keys))
            (merge-atomically-like-core values))
          {})))
    values))

(defn merge-values-like-core-nil-override [values]
  (let [value (first values)
        refs (keep (fn [val] (if (= (type val) clojure.lang.Ref) val)) values)
        nonrefs (keep (fn [val] (if (= (type val) clojure.lang.Ref) nil val)) values)]
    (if (> (.size refs) 0)
      ;; return the reference after setting it to the value of its merged values with
      ;; the rest of the items to be merged.
      ;; TODO: all the refs are ignored except first: should point the others (rest refs) to the first.
      (let [do-sync (dosync
                     (alter (first refs)
                            (fn [x] (merge-values-like-core-nil-override (cons @(first refs) nonrefs)))))]
        (first refs))
      (if value
        (if (or (= (type value) clojure.lang.PersistentArrayMap)
                (= (type value) clojure.lang.PersistentHashMap))
          (let [rest-of (if (> (.size (rest values)) 0)
                          (merge-values-like-core-nil-override (rest values))
                          :empty-list)]
            (if
                (or (= nil rest-of)
                    (= {} rest-of)) ;; this canonicalizes {} to nil (so {} and nil are treated the same, but nil is the canonical representation).
              nil
              (let [maps values
                    keys (set (mapcat #'keys maps))
                    collect-values (collect-values maps keys)]
                (merge-r-like-core-nil-override collect-values keys))))
          (merge-atomically-like-core values))
        (if (and (= (.size values) 1)
                 (= value nil))
          nil {})))))

(defn- merge-r-like-core [collected-map keys]
  "merge a map where each value is a list of values to be merged for that key."
  (if (and (not (nil? keys)) (> (.size keys) 0))
    (let [key (first keys)]
      (let [merged-values (merge-values-like-core (get collected-map key))]
        (if (not (= merged-values {}))
          (conj
           {key merged-values}
           (merge-r-like-core collected-map (rest keys)))
          ;; handle nils the same as non-nils.
          (conj
           {key merged-values}
           (merge-r-like-core collected-map (rest keys))))))
      {}))

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
          (if (= (first values) :top)
            (merge-atomically (rest values))
          :fail))
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
  (if (> (.size keys) 0)
    (let [key (first keys)]
      (if key
        (conj
         {key (merge-values (get collected-map key))}
         (merge-r collected-map (rest keys)))
        {}))
    {}))

(defn merge [& maps]
  "like clojure.core/merge, but works recursively. also lacks optimization, testing"
  (let [keyset (set (mapcat #'keys maps))]  ;; the set of all keys in all of the maps.
    (merge-r (collect-values maps keyset)
             (seq keyset))))

(defn union-keys [maps]
  ;; TODO: check that maps is a list (prevent 'evaluation aborted' messages).
  (set (mapcat #'keys maps)))

;; TODO: use merge-with http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/merge-with
;; TODO: it's misleading to say it's 'like core' when behavior differs w.r.t. nil."
;;  (merge-nil-override is more 'like core' in this respect).
(defn merge-like-core [& maps]
  "like clojure.core/merge, but works recursively, and works like it also in that the last value wins (see test 'atomic-merge' for usage.)"
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
;; TODO: eliminate in favor of merge-values-like-core.
(defn m [& maps]
  "like clojure.core/merge, but works recursively, and works like it also in that the last value wins (see test 'atomic-merge' for usage.)"
  (if (= (type (first maps)) java.lang.Integer)
    ;; if first is an integer, assume the others are too.
    ;; TODO: use (keep) as above with merge-values-like-core.
    (merge-atomically maps)
    (if (= (type (first maps)) clojure.lang.Ref)
      ;; return the reference after setting it to the value of its merged values with
      ;; the rest of the items to be merged.
      (let [do-sync (dosync
                     (alter (first maps)
                            (fn [x] (merge-values (cons @(first maps) (rest maps))))))]
        (first maps))
      ;; else assume all are really maps.
      (let [keyset (union-keys maps)
            values (collect-values maps keyset)]
        (merge-r-like-core values (seq keyset))))))

;; similar to clojure core's get-in, but supports :ref as a special feature.
;; TODO: :ref is obsolete: remove metions of it.
;; TODO: don't use this method, it doesn't make sense because it appends a :root
;; to all return values for unknown reasons. Instead, just use clojure's (get-in).
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
    (merge (encode-refs fs inv)
           {:refs (set-to-list inv)})))

(defn set-all-paths [fs paths val]
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

;; TODO: getting this on initial C-c C-k.
;;Unknown location:
;;  error: java.lang.StackOverflowError (fs.clj:352)

;;Unknown location:
;;  error: java.lang.StackOverflowError

;;Compilation failed.

(def tests
  (list

   (rdutest
    "union-keys"
    (union-keys (list {:foo 99}))
    (fn [result]
      (= (seq result) '(:foo))))

   (rdutest
    "collect-values"
    (let [maps (list {:foo 99})]
      (collect-values maps
                      (union-keys maps)))
    (fn [result]
      (= (:foo result) (list 99))))

   (rdutest
    "merge-values-like-core"
    (let [maps (list {:foo 99})
          keyset (union-keys maps)
          values (collect-values maps keyset)]
      (merge-values-like-core (get values (first keyset))))
    (fn [result]
      (= result 99)))
   
   (rdutest
    "merge-r-like-core"
    (let [maps (list {:foo 99})
          keyset (union-keys maps)
          values (collect-values maps keyset)]
      (merge-r-like-core values (seq keyset)))
    (fn [result]
      true))
  
   (rdutest
    "simple merge test."
    (m {:foo 99} {:bar 42})
    (fn [result]
      (and (= (:foo result) 99)
           (= (:bar result) 42))))
         
   (rdutest
    "Recursive merge of 3 maps."
    (let [map1 {:foo {:bar 99}}
          map2 {:foo {:baz 42}}
          map3 {:biff 12}]
      (m map1 map2 map3))
    (fn [merge-result]
      ;; test that result looks like:
      ;; {:foo {:bar 99
      ;;        :baz 42}
      ;;  :biff 12}}
      (and
       (= (:bar (:foo merge-result)) 99)
       (= (:baz (:foo merge-result)) 42)
       (= (:biff merge-result) 12))))
   
   (rdutest
    "Recursive merge of 3 maps, tested with (get-path)"
    (let [map1 {:foo {:bar 99}}
          map2 {:foo {:baz 42}}
          map3 {:biff 12}]
      (m map1 map2 map3))
    (fn [merge-result]
      (and
       (= (get-path merge-result '(:foo :bar)) 99)
       (= (get-path merge-result '(:foo :baz)) 42))))

   (rdutest
    "Testing that merge(v1,v2)=fail if v1 != v2."
    (merge {:foo 42} {:foo 43})
    (fn [result]
      (= (:foo result) :fail)))
   
   (rdutest
    "Testing that merge-like-core(v1,v2)=v2 (overriding works)."
    (merge-like-core {:foo 42} {:foo 43})
    (fn [result]
      (= (:foo result) 43)))
 
  (rdutest
    "Ignore nils in values (true,nil)."
    (merge-like-core {:foo true} {:foo nil})
    (fn [result]
      (= (:foo result) true)))

  (rdutest
    "{} (unlike with nil) overrides true in merge-like-core."
    (merge-like-core {:foo true} {:foo {}})
    (fn [result]
      (= (:foo result) {})))
  
   (rdutest
    "Ignore nils in values (nil,nil)."
    (merge-like-core {:foo nil} {:foo nil})
    (fn [result]
      (= result {:foo nil})))
   
   (rdutest
    "{:a 42}{:a nil} -merge-nil-override-> {:a nil}"
    (apply fs/merge-nil-override (list {:a 42}{:a nil}))
    (fn [map]
      (= (:a map) nil)))
   
;   (rdutest
;    "{:a {:foo 42}}{:a nil} => {:a nil}"
;    (apply fs/merge-nil-override (list {:a {:foo 42}} {:a nil}))
;   (fn [map]
;     (= (:a map) nil)))

;   (rdutest
;    "{:a {:foo 42}}{:a {}} => {:a nil}"
;    (apply fs/merge-nil-override (list {:a {:foo 42}} {:a {}}))
;    (fn [map]
;      (= (:a map) nil)))
   
;   (rdutest
;    "(apply the :fn function of a map on the result of running fs/m)"
;    (merge-and-apply
;     (list 
;      {:a 42 :fn (fn [map]
;                   (fs/m map
;                         {:a (+ 1 (:a map)) :foo "bar"}))}
;     {:b 99}))
;   (fn [map]
;      (and (= (:foo map) "bar")
;           (= (:a 43)))))
   
;   (rdutest
;    "(apply the :fn value (after converting from a string to a function) of a map on the result of running fs/m)"
;    (merge-and-apply (list {:a 42 :fn "myfn"}
;                           {:b 99}))
;    (fn [map]
;      (and (= (:foo map) "bar")
;           (= (:a 43)))))

;   (rdutest
;    "resolve :ref pointers: (:b :c) should be identical to (:a)."
;    {:a {:ref '(:b :c)}
;     :b {:c {:foo :bar}}}
;    (fn [map]
;      (= (get-path map '(:b :c))
;         (get-path map '(:a)))))

;   (rdutest
;    "ref chain: ref->ref1->val"
;    {:a {:ref '(:b :c)}
;     :b {:c {:ref '(:d :e)}}
;     :d {:e 42}}
;    (fn [map]
;      (= (get-path map '(:a))
;         42)))

   ;; ref chain with referential equality: make sure (=) works as expected:
   ;; if (= (get-path x (P1 :ref) P2)), then (= (get-path x P1) (get-path y P2))
;   (rdutest
;    "ref chain: ref->ref1->val"
;    {:a {:ref '(:b :c)}
;     :b {:c {:ref '(:d :e)}}
;     :d {:e {:f 42}}}
;    (fn [map]
;      (= (get-path map '(:a))
;         (get-path map '(:d :e)))))

   ;; (same as previous path, but with end of path an atom)
;   (rdutest
;    "ref chain: ref->ref1->val"
;    {:a {:ref '(:b :c)}
;     :b {:c {:ref '(:d :e)}}
;     :d {:e {:f 42}}}
;    (fn [map]
;      (= (get-path map '(:a :f))
;         (get-path map '(:d :e :f))
;         42)))

   ;; P1 = '(F1 F2) => (get-path P1) == (get-path (get-path F1) F2)
;   (rdutest
;    "(get-path (get-path))"
;    {:a {:ref '(:b :c)}
;     :b {:c {:ref '(:d :e)}}
;     :d {:e {:f 42}}}
;    (fn [map]
;      (= (get-path map '(:d :e))
;         (get-path (get-path map '(:b)) '(:c)))))

   ;; P1 = '(F1 F2 F3) => (get-path P1) == (get-path (get-path (get-path F1) F2) F3)
;   (rdutest
;    "(get-path (get-path))"
;    {:a {:ref '(:b :c)}
;     :b {:c {:ref '(:d :e)}}
;     :d {:e {:f 42}}}
;    (fn [map]
;      (= (get-path map '(:d :e :f))
;         (get-path (get-path (get-path map '(:b)) '(:c)) '(:f))
;         42)))

   ;; test map inversion
   ;; path (:a :b) points to a reference, whose value is an integer, 42.
   ;; path (:c) also points to the same reference.
   ;;
   ;;[ :a  [:b [1] 42]
   ;;  :c  [1]]
   ;;
   ;; => {#<Ref: 42> => {(:a :b) (:c)}
   ;;                               
;   (rdutest "map inversion"
;            (let [myref (ref 42)
;                  fs {:a {:b myref}
;                      :c myref}]
;              (ref-invert fs))
;            (fn [result]
;              (and (= (.size result) 1)
;                   (= (first (first result))
;                      42)
;                   (let [paths (second (first result))]
;                     (and (= (.size paths) 2)
;                          (or (= (first paths) '(:a :b))
;                              (= (first paths) '(:c)))
;                          (or (= (second paths) '(:a :b))
;                              (= (second paths) '(:c))))))))

;   (rdutest "serialization"
;            (let [myref (ref 42)
;                  fs {:a {:b myref}
;                      :c myref}]
;              (serialize fs))
;            (fn [result]
;              (and 
;               (= (get-in result '(:a :b)) 42)
;               (= (get-in result '(:c)) 42)
;               (not (nil? (:refs result)))
;               (= (.size (:refs result)) 1)
;               (= (first (first (:refs result))) 42)
;               (let [paths (second (first (:refs result)))]
;                 (and (= (.size paths) 2)
;                      (or (= (first paths) '(:a :b))
;                          (= (first paths) '(:c)))
;                      (or (= (second paths) '(:a :b))
;                          (= (second paths) '(:c))))))))

;      (rdutest "deserialization"
;            (let [myref (ref 42)
;                  fs {:a {:b myref}
;                      :c myref}
;                  serialized (serialize fs)]
;              (list fs (deserialize serialized)))
;            (fn [result] ;; fs and our deserialized fs should be isomorphic.
;              ;; TODO: also test to make sure fs original and copy are distinct as well (not ref-equal)
;              (let [original (first result)
;                    copy (second result)]
;                (and (= (get-in original '(:a :b))
;                        (get-in original '(:c)))
;                     (= (get-in copy '(:a :b))
;                        (get-in copy '(:c)))))))

;      (rdutest "merging atomic values: fails"
;               (merge-values (list 42 43))
;               (fn [result]
;                 (= result :fail)))

;      (rdutest "merging atomic values: succeeds"
;               (merge-values (list 42 42))
;               (fn [result]
;                 (= result 42)))

;      (rdutest "merging atomic values with references"
;               (let [myref (ref :top)
;                     val 42]
;                 (fs/m myref val))
;               (fn [result]
;                 (and
;                  (= (type result) clojure.lang.Ref)
;                  (= @result 42))))

      ;; {:a [1] :top
      ;;  :b [1]     } ,
      ;; {:a 42}
      ;;        =>
      ;; {:a [1] 42
      ;;  :b [1] }
;      (rdutest "merging with references"
;            (let [myref (ref :top)
;                  fs1 {:a myref :b myref}
;                  fs2 {:a 42}]
;              (fs/m fs1 fs2))
;            (fn [result]
;              (and
;               (= (type (:a result)) clojure.lang.Ref)
;               (= @(:a result) 42)
;               (= @(:b result) 42)
;               (= (:a result) (:b result)))))

      ;; {:a [1] :top    {:a 42}
      ;;  :b [1]     } ,
      ;; 
      ;;        =>
      ;; {:a [1] 42
      ;;  :b [1] }
;      (rdutest
;       "merging with references with nil-override family of functions (used by lexiconfn/add)"
;            (let [myref (ref :top)
;                  fs1 {:a myref :b myref}
;                  fs2 {:a 42}]
;              (fs/merge-nil-override fs1 fs2))
;           (fn [result]
;             (and
;               (= (type (:a result)) clojure.lang.Ref)
;               (= @(:a result) 42)
;               (= @(:b result) 42)
;               (= (:a result) (:b result)))))

;      (rdutest
;       "merging with references with nil-override family of functions (used by lexiconfn/add) (2)"
;            (let [myref (ref :top)
;                  fs1 {:a myref}
;                  fs2 {:a :foo}]
;              (fs/merge-nil-override fs1 fs2))
;           (fn [result]
;              (and
;               (= (type (:a result)) clojure.lang.Ref)
;               (= @(:a result) :foo))))

;      (rdutest
;       "merging with inner reference:keyset"
;       (let [fs1 {:b (ref :top)}
;             fs2 {:b 42}
;            maps (list fs1 fs2)]
;         (seq (set (mapcat #'keys maps)))) ;; mapcat->set->seq removes duplicates.
;       (fn [result]
;         (= result '(:b))))

;      (rdutest
;       "merging with inner reference:collect-values: result should be: {:b (42 <Ref>)} (map with one key, :b, whose value is a list of two elements: 42 and a <Ref> (in either order)"
;       (let [fs1 {:b (ref :top)}
;             fs2 {:b 42}
;             maps (list fs1 fs2)]
;        (collect-values maps (set (mapcat #'keys maps))))
;       (fn [result]
;         (and (not (nil? (:b result)))
;              (= (.size (:b result)) 2)
;              (or (= 42
;                     (first (:b result)))
;                  (= 42
;                     (second (:b result))))
;              (or (= clojure.lang.Ref
;                     (type (first (:b result))))
;                  (= clojure.lang.Ref
;                     (type (second (:b result))))))))

;      (rdutest
;       "merging with inner reference:merge-r-like-core"
;       (let [fs1 {:b (ref :top)}
;             fs2 {:b 42}
;             maps (list fs1 fs2)
;             collect-values (collect-values maps (set (mapcat #'keys maps)))]
;         (merge-r-like-core collect-values (set (mapcat #'keys maps))))
;       (fn [result]
;         (and (not (nil? (:b result)))
;              (= (type (:b result)) clojure.lang.Ref))
;              (= @(:b result) 42)))

      ;; [b [1] top], [b 42] => [b [1] 42]
;      (rdutest
;       "merging with reference"
;       (let [fs1 {:b (ref :top)}
;             fs2 {:b 42}]
;         (fs/merge-values-like-core (list fs1 fs2)))
;       (fn [result]
;         (and (= (type (:b result)) clojure.lang.Ref)
;              (= @(:b result)) 42)))

      ;; [b [1] top], [b 42] => [b [1] 42]
;      (rdutest
;       "merging with merge-nil-override with reference"
;       (let [fs1 {:b (ref :top)}
;             fs2 {:b 42}]
;         (fs/merge-nil-override fs1 fs2))
;       (fn [result]
;         (and (= (type (:b result)) clojure.lang.Ref)
;              (= @(:b result)) 42)))

      ;; [a [b [1] top]], [a [b 42]] => [a [b [1] 42]]
;      (rdutest
;       "merging with merge-nil-override with inner reference"
;       (let [fs1 {:a {:b (ref :top)}}
;             fs2 {:a {:b 42}}]
;         (fs/merge-nil-override fs1 fs2))
;       (fn [result]
;         (and (= (type (:b (:a result))) clojure.lang.Ref)
;              (= @(:b (:a result))) 42)))

      ;; [a [b [1] top]], [a [b 42]] => [a [b [1] 42]]
;      (rdutest
;       "merging with merge-nil-override with inner reference, second position"
;       (let [fs1 {:a {:b 42}}
;             fs2 {:a {:b (ref :top)}}]
;         (fs/merge-nil-override fs1 fs2))
;       (fn [result]
;         (and (= (type (:b (:a result))) clojure.lang.Ref)
;              (= @(:b (:a result))) 42)))

;      (rdutest
;       "merge-values-like-core-nil-override with reference, second position"
;       (let [values (list :masc (ref :top))]
;         (merge-values-like-core-nil-override values))
;       (fn [result]
;         (and (= (type result) clojure.lang.Ref)
;              (= @result :masc))))

;      (rdutest
;       "merge-r-like-core-nil-override with reference, second position"
;       (let [values (collect-values-with-nil (list {:a :masc} {:a (ref :top)}) #{:a})]
;         (merge-r-like-core-nil-override values '(:a)))
;       (fn [result]
;         (and (= (type (:a result)) clojure.lang.Ref)
;              (= @(:a result) :masc))))

;      (rdutest
;       "merging with merge-nil-override with reference, second position"
;       (let [fs1 {:a 42}
;             fs2 {:a (ref :top)}]
;         (fs/merge-nil-override {:a 42} {:a 42}))
;                                        ;        (fs/merge-nil-override fs1 fs2))
;       (fn [result]
;         (and (= (type (:a result)) clojure.lang.Ref)
;              (= @(:a result) 42))))

;      (rdutest
;       "test merging with ':not' (special feature) (succeed atomic)"
;       (m (merge-values-like-core '(42 {:not 43})))
;       (fn [result]
;         (= result 42)))

;      (rdutest
;       "test merging with ':not' (special feature) (fail atomic)"
;       (m (merge-values-like-core '(42 {:not 42})))
;       (fn [result]
;         (= result :fail)))

;      (rdutest
;       "test merging with ':not' (special feature) (combine negation)"
;       (m (merge-values-like-core '({:not 41} {:not 42})))
;       (fn [result]
;         (= (set (:not result)) (set 41 42)))
;       :test-merge-not-with-combine-not)
      
;      (rdutest
;       "test merging with ':not' (special feature) (4)"
;       (m {:key {:not "foo"} } {:key "bar"})
;       (fn [result]
;         (= (:key result) "bar"))
;       :test-not-merge)
      ))



