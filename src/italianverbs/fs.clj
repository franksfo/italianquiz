(ns italianverbs.fs
  (:use [hiccup core page-helpers]
        [clojure.set])
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

(defn- merge-values [values]
  (let [value (first values)]
    (if value
      (if (= (type value) clojure.lang.PersistentArrayMap)
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
  "like clojure.core/merge, but works recursively."
  (let [keyset (union-keys maps)]
    (merge-r (collect-values maps keyset)
             (seq keyset))))

(defn test []
  "this should contain a list of all the tests for the fs package. each test can
  return a map or a list or a function. a function will be applied against an
  empty argument list"
  (list
   {:comment "recursive merge."
    :test (let [map1 {:foo {:bar 99}}
                map2 {:foo {:baz 42}}
                map3 {:biff 12}]
            (merge map1 map2 map3))
    :assert (= (let [map1 {:foo {:bar 99}}
                     map2 {:foo {:baz 42}}
                     map3 {:biff 12}]
                 (merge map1 map2 map3))
               {:foo {:baz 42, :bar 99}, :biff 12})}))

