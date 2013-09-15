(ns italianverbs.grammar-workshop
  (:refer-clojure :exclude [get-in resolve])
  (:use [clojure.set :only (union)]
        [clojure.core :exclude (get-in resolve)]
        [italianverbs.generate :only (generate moreover-head moreover-comp gen14)]
        [italianverbs.grammar]
        [italianverbs.lexicon :only (it1 lexicon it en)]
        [italianverbs.lexiconfn :only (unify sem-impl)]
        [italianverbs.morphology :only (finalize fo italian-article)]
        [italianverbs.unify :only (copy fail? serialize get-in resolve)]
        )

  (:require [clojure.tools.logging :as log]
            [italianverbs.lexicon :as lex]
            [italianverbs.unify :as unify]
            [clojure.string :as string])
)

(defmacro myhh21 [head comp]
  `(do ~(log/info "myhh21 macro compile-time.")
       (gen15 (list hh21)
              ~head
              ~comp)))

(defmacro mycc10 [head comp]
  `(do ~(log/info "mycc10 macro compile-time.")
       (gen15 (list cc10)
              ~head
              ~comp)))

(defn my-sent []
  ;; parent: S -> NP VP
  (mycc10

   ;; VP -> V NP:
   (myhh21
    (filter (fn [candidate] ;; filter Vs to reduce number of candidates we need to filter.
              (and (not (= :notfound (get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                   (= (get-in candidate '(:synsem :cat)) :verb)))
            (lazy-shuffle hh21-heads))

    ;; object: NP -> Det N
    base-cc10-random)

   ;; subject: NP -> Det N
   base-cc10-random))

(defn take-sentences-randomly [n]
  (take n
        (my-sent)))



