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

(defn union-keys [maps]
  (if (and maps (> (.size maps) 0))
    (union
     (set (keys (first maps)))
     (union-keys (rest maps)))
    {}))

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

(defn- merge-atomically [values]
  (let [value (first values)]
    (if value
      (if (second values)
        (if (= (first values) (second values))
          (merge-atomically (rest values))
          :fail)
        value))))

(defn- merge-atomically-like-core [values]
  (last values))

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

(defn- merge-r [collected-map keys]
  "merge a map where each value is a list of values to be merged for that key."
  (let [key (first keys)]
    (if key
      (merge
       {key (merge-values (get collected-map key))}
       (merge-r collected-map (rest keys)))
      {})))

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

(defn merge [& maps]
  "like clojure.core/merge, but works recursively."
  (let [keyset (union-keys maps)]
    (merge-r (collect-values maps keyset)
             (seq keyset))))

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

(defn ref-invert [input]
  "turn a map<P,V> into an inverted map<V,[P]> where every V has a list of what paths P point to it."
  (let [keys (keys input)
        vals (set (vals input))] ;; (set) makes vals distinct.
    (let [inverted-list
          (map (fn [val]
                 (list val
                       (mapcat (fn [key] 
                                 (if (= (get input key)
                                        val)
                                   (list key)))
                               keys)))
               vals)]
      (zipmap (map #'first inverted-list)
              (map #'second inverted-list)))))

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

   ;; test ref serialization (1)
   ;; :a and :b's value is a reference, whose value is an integer, 42.
   ;; :c's value is an integer, which just so happens to be 42.
   ;;[ :a [1] 42
   ;;  :b [1]
   ;;  :c     42 ]
   ;; =>
   ;;
   ;; {#<Ref: 42> => (:a :b)
   ;;  42         => (:c)}
   ;;                               

   (rdutest "test serialization"
            (let [myref (ref 42)
                  fs {:a myref
                      :b myref
                      :c 42}]
              (ref-invert fs))
            (fn [result]
              (let [keys (set (keys result))
                    vals (vals result)]
                    
                (and (= (.size keys) 2)
                     (or (= (first keys) 42)
                         (= (second keys) 42))
                     (or (= (set (first vals)) (set (list :a :b)))
                         (= (set (second vals)) (set (list :a :b))))
                     (or (= (first vals) (list :c))
                         (= (second vals) (list :c)))
                     (or (and (= (type (first keys))
                                 clojure.lang.Ref)
                              (= @(first keys) 42))
                         (and (= (type (second keys))
                                 clojure.lang.Ref)
                              (= @(second keys) 42))))))

            :ref-serialization)


   ;; test merge-with-append: create list out of all values for the same feature.
   ;; {:a (:b :c :d)}
   ;; {:a (:e :f :g)} =>
   ;; {:a ((:b :c :d) (:e :f :g))}
   (rdutest "test merge-with-append"
            (let [mwa (fn [map1 map2]
                        ;; (merge map1 map2)
                        
                        {:a ((:b :c :d)(:e :f :g))})]
              (mwa
               {:a (:b :c :d)}
               {:a (:e :f :g)}))
            (fn [result]
              (= result
                 {:a ((:b :c :d)(:e :f :g))}))
            :merge-with-append)

   
   ;; test ref serialization (2)
   ;; path (:a :b) points to a reference, whose value is an integer, 42.
   ;; path (:c) also points to the same reference.
   ;;
   ;;[ :a  [:b [1] 42]
   ;;  :c  [1]]
   ;;
   ;; => {#<Ref: 42> => ((:a :b) (:c))
   ;;                               
   (rdutest "test serialization with paths"
            (let [myref (ref 42)
                  fs {:a {:b myref}
                      :c myref}]
              (ref-invert fs))
            (fn [result]
              (not (nil? result))) ;; just stub test.
            :ref-serialization-with-path)
   ))

   

   
