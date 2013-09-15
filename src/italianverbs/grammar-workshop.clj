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


(defn generate-sentences-with-subjects-1 [subcat-info subject-heads subject-comps]
  "filter subject-heads by subcat-info"
  (base-cc10 (fn [subject-head]
                 (not (fail? (unify subcat-info subject-head))))
                subject-heads))

(defn generate-sentences-with-subjects [subcat-infos subject-heads subject-comps]
  "generate subjects: filter heads by subcat-infos."
  (if (not (empty? subcat-infos))
    (lazy-cat (generate-sentences-with-subjects-1 (first subcat-infos) subject-heads subject-comps)
              (generate-sentences-with-subjects (rest subcat-infos) subject-heads subject-comps))))

(defn thevps1 []
  (filter (fn [candidate]
            (and (not (= :notfound (unify/get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                 (= (unify/get-in candidate '(:synsem :cat)) :verb)))
          (shuffle hh21-heads))) ;; Verb

(defn thevps []
  (gen15 (list hh21)
         (filter (fn [candidate]
                   (and (not (= :notfound (unify/get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                        (= (unify/get-in candidate '(:synsem :cat)) :verb)))
                 (shuffle hh21-heads)) ;; Verb
         base-cc10-random)) ;; object NP

(defn take-sentences-randomly [n]
  (take n
        (let [vps
              ;; head: VP -> V NP
              (gen15 (list hh21)
                     (filter (fn [candidate]
                               (and (not (= :notfound (unify/get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                                    (= (unify/get-in candidate '(:synsem :cat)) :verb)))
                             (shuffle hh21-heads)) ;; Verb
                     base-cc10-random)] ;; object NP
          vps)))

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
  (mycc10 ;; parent: S -> NP VP

   ;; VP -> V NP:
   (gen15 (list hh21)  ;; V
          (filter (fn [candidate] ;; filter Vs to reduce number of candidates we need to filter.
                    (and (not (= :notfound (get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                         (= (get-in candidate '(:synsem :cat)) :verb)))
                  (lazy-shuffle hh21-heads))
          base-cc10-random) ;; object: NP -> Det N

   base-cc10-random)) ;; subject: NP -> Det N



