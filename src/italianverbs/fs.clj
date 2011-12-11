(ns italianverbs.fs
  (:use [hiccup core page-helpers]
        [clojure.set]
        [italianverbs.rdutest])
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

(defn get-path [fs path]
  (if (> (.size path) 0)
    (get-path (get fs (first path))
              (rest path))
    fs))

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
      (if (= (type value) clojure.lang.PersistentArrayMap)
        (merge
         (first values)
         (merge-values (rest values)))
        (merge-atomically values))
        {})))

(defn merge-values-like-core [values]
  (let [value (first values)]
    (if value
      (if (= (type value) clojure.lang.PersistentArrayMap)
        (merge
         value
         (merge-values-like-core (rest values)))
        (merge-atomically-like-core values))
        {})))

(defn merge-values-like-core-nil-override [values]
  (let [value (first values)]
    (if value
      (if (= (type value) clojure.lang.PersistentArrayMap)
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

(defn mergec [& maps]
  (merge-like-core (list maps)))


(defn merge-and-apply [& maps]
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

;;  needed below for :merge-and-apply-test-with-fn-as-string test.
(defn myfn [map]
  (fs/m map
        {:a (+ 1 (:a map))
         :foo "bar"}))

(def tests
  {
   :recursive-merge
   (rdutest
    "Recursive merge of 3 maps."
    (let [map1 {:foo {:bar 99}}
          map2 {:foo {:baz 42}}
          map3 {:biff 12}]
      (merge map1 map2 map3))
    (fn [merge-result]
      (and
       (= (:bar (:foo merge-result)) 99)
       (= (:baz (:foo merge-result)) 42)
       (= (:biff merge-result) 12)))
    :recursive-merge)
   
   :recursive-merge-with-paths
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

   :atomic-fail
   (rdutest
    "Testing that merge(v1,v2)=fail if v1 != v2."
    (merge {:foo 42} {:foo 43})
    (fn [result]
      (= (:foo result) :fail))
    :atomic-fail)

   :atomic-merge
   (rdutest
    "Testing that merge-like-core(v1,v2)=v2 (overriding works)."
    (merge-like-core {:foo 42} {:foo 43})
    (fn [result]
      (= (:foo result) 43))
    :atomic-merge)

   :ignore-nil-values
   (rdutest
    "Ignore nils in values."
    (merge-like-core {:foo true} {:foo nil})
    (fn [result]
      (= (:foo result) true))
    :ignore-nil-values)

   :ignore-nil-values-2
   (rdutest
    "Ignore nils in values."
    (merge-like-core {:foo nil} {:foo nil})
    (fn [result]
      (= result {}))
    :ignore-nil-values-2)
   
   :nil-should-override-atomic
   (rdutest
    "{:a 42}{:a nil} => {:a nil}"
    (apply fs/merge-nil-override (list {:a 42}{:a nil}))
    (fn [map]
      (= (:a map) nil))
    :nil-should-override-atomic)

   :nil-should-override-map
   (rdutest
    "{:a {:foo 42}}{:a nil} => {:a nil}"
    (apply fs/merge-nil-override (list {:a {:foo 42}} {:a nil}))
    (fn [map]
      (= (:a map) nil))
    :nil-should-override)

   :merge-and-apply-test
   (rdutest
    "(apply the :fn function of a map on the result of running fs/m)"
    (merge-and-apply {:a 42 :fn (fn [map]
                                  (fs/m map
                                        {:a (+ 1 (:a map)) :foo "bar"}))}
                     {:b 99})
    (fn [map]
      (and (= (:foo map) "bar")
           (= (:a 43))))
    :merge-and-apply-test)
   
   :merge-and-apply-test-with-fn-as-string
   (rdutest
    "(apply the :fn value (after converting from a string to a function) of a map on the result of running fs/m)"
    (merge-and-apply {:a 42 :fn "myfn"}
                     {:b 99})
   (fn [map]
      (and (= (:foo map) "bar")
           (= (:a 43))))
    :merge-and-apply-test-with-fn-as-string)


   })








