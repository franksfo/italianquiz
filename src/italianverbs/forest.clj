(ns italianverbs.forest
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:use [clojure.set]
        [clojure.stacktrace]
        [italianverbs.generate]
        [italianverbs.grammar]
        [italianverbs.lexicon]
        [italianverbs.morphology :only (fo)]
        [italianverbs.ug])
  (:require
   [clojure.core :as core]
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
;(def parents (set (list 'hh10 'hh21)))

;(def parents (set (list 'cc10)))
;(def lex (set (list 'io 'tu 'dormire)))

(def parents (set (list cc10)))
(def lex (union (it "io") (it "tu") (it "dormire")))

(defn in? [member of-set]
  (not (empty? (intersection (set (list member)) of-set))))

(defn one-tree [set]
  (let [choice (choose-at-random set)]
    (cond (in? choice parents)
          {:sch choice
           :h (one-tree set)
           :c (one-tree set)}
          true
          choice)))

(defn choose-head [parent set]
  {:h (choose-at-random set)})

(defn all-trees [tree-with-head set]
  (if (not (empty? set))
    (lazy-seq
     (cons
      (unifyc tree-with-head
              {:c (first set)})
      (all-trees tree-with-head (rest set))))))

(defn all-heads-and-parents [parent set]
  "generate all possible head-parent combinations."
  (lazy-seq
   (cons
    (unifyc parent
            {:h (first set)})
    (all-heads-and-parents parent set))))

(def upl (union parents lex))

(defn forest [set]
  "generate a lazy sequence of trees"
  (lazy-seq
   (cons
    (one-tree set)
    (forest set))))

(def pl (union parents lex))

(defn choose-at-random-with-depth [parents lex depth]
  (let [rand (rand-int 10)]
    (cond (= depth 0)
          (cond (> rand 3)
                (first (shuffle parents))
                true
                (first (shuffle lex)))
          (= depth 1)
          (cond (> rand 5)
                (first (shuffle parents))
                true
                (first (shuffle lex)))
          (= depth 2)
          (cond (> rand 7)
                (first (shuffle parents))
                true
                (first (shuffle lex)))
          true
          (cond (> rand 8)
                (first (shuffle parents))
                true
                (first (shuffle lex))))))

(defn h1d1 [parents lex & [depth]]
  "head-first,depth-first generation"
  (let [depth (if depth depth 0)
        choice (choose-at-random-with-depth parents lex depth)]
    (cond (in? choice parents)
          ;; this is a sub-tree: generate its head.
          (unifyc choice
                  {:head (h1d1 parents lex)})
          true
          ;; not a subtree: done.
          choice)))

(defn do-a-bunch []
  (take 5 (forest (union parents lex))))

;(h1d1 parents lex 0)