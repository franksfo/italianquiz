(ns italianverbs.forest
  (:refer-clojure :exclude [get-in merge resolve find])
  (:use [clojure.set]
        [clojure.stacktrace]
        [italianverbs.generate]
        [italianverbs.grammar]
        [italianverbs.morphology :only (fo)]
        [italianverbs.ug])
  (:require
   [clojure.tools.logging :as log]
   [italianverbs.lev :as lev]
   [italianverbs.unify :refer :all]
   [italianverbs.unify :as unify]
   [italianverbs.config :as config]
   [italianverbs.html :as html]
   [italianverbs.search :as search]
   [clojure.string :as string]))

(defn choose-at-random [set & [distrib]]
  "choose one from amongst set using a probability distribution."
  (if (nil? distrib)
    (first (shuffle set))
    (first (shuffle set))))

;(def parents (list hh10 hh21))
(def parents (set (list 'hh10 'hh21)))
(def lex (set (list 'a 'b 'c)))

(defn in? [member of-set]
  (not (empty? (intersection (set (list member)) of-set))))

(defn one-tree [set]
  (let [choice (choose-at-random set)]
    (cond (in? choice parents)
          {:sch choice
           :a (one-tree set)
           :b (one-tree set)}
          true
          choice)))

(defn forest [set]
  "generate a lazy sequence of trees"
  (lazy-seq
   (cons
    (one-tree set)
    (forest set))))

(defn do-a-bunch []
  (take 5 (forest (union parents lex))))






