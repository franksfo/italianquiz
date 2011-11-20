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

(defn- union-keys [maps]
  (let [map (first maps)]
    (if map
      (union
       (set (keys map))
       (union-keys (rest maps)))
      {})))

(defn- collect-values [maps keys]
  (let [key (first keys)]
    (if key
      (merge
       {key (mapcat (fn [eachmap]
                      (let [val (get eachmap key :notfound)]
                        (if (not (= val :notfound))
                          (list val))))
                    maps)}
       (collect-values maps (rest keys))))))

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

(defn- merge-values-like-core [values]
  (let [value (first values)]
    (if value
      (if (= (type value) clojure.lang.PersistentArrayMap)
        (merge
         (first values)
         (merge-values-like-core (rest values)))
        (merge-atomically-like-core values))
        {})))

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
      (merge
       {key (merge-values-like-core (get collected-map key))}
       (merge-r-like-core collected-map (rest keys)))
      {})))

(defn merge [& maps]
  "like clojure.core/merge, but works recursively."
  (let [keyset (union-keys maps)]
    (merge-r (collect-values maps keyset)
             (seq keyset))))

;; TODO: use merge-with http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/merge-with
(defn merge-like-core [& maps]
  "like clojure.core/merge, but works recursively, and works like it also in that the last value wins (see test 'atomic-merge' for usage.)"
  (let [keyset (union-keys maps)]
    (merge-r-like-core (collect-values maps keyset)
                       (seq keyset))))

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
       (= (:biff merge-result) 12))))
   
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
       (= (get-path merge-result '(:foo :baz)) 42))))

   :atomic-fail
   (rdutest
    "Testing that merge(v1,v2)=fail if v1 != v2."
    (merge {:foo 42} {:foo 43})
    (fn [result]
      (= (:foo result) :fail)))

   :atomic-merge
   (rdutest
    "Testing that merge-like-core(v1,v2)=v2."
    (merge-like-core {:foo 42} {:foo 43})
    (fn [result]
      (= (:foo result) 43)))




