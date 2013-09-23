(ns italianverbs.grammar
  (:refer-clojure :exclude [get-in resolve])
  (:use [clojure.set :only (union intersection)]
        [clojure.core :exclude (get-in resolve merge)]
        [italianverbs.generate :only (generate moreover-head moreover-comp gen14)]
        [italianverbs.lexicon :only (it1)]
        [italianverbs.lexiconfn :only (unify sem-impl)]
        [italianverbs.morphology :only (finalize fo italian-article get-italian-1 get-italian)]
        [italianverbs.ug]
        [italianverbs.unify :only (copy fail? serialize get-in)]
        )

  (:require [clojure.tools.logging :as log]
            [italianverbs.lexicon :as lex]
            [italianverbs.unify :as unify]
            [clojure.string :as string])
)

(def tinylex
  (union ;(it1 "aiutare")
         (it1 "andare")
         (it1 "dormire")
         (it1 "la")
         (it1 "il")
         (it1 "io")
         (it1 "ragazzo")
         (it1 "ragazza")
         (it1 "un")
         (it1 "vedere")
        ))

(log/info "begin italian-english specifics.")

 ;; note that order of arguments to mycc10 is reverse of s-to-np-vp, because
;; (mycc10) and other generic functions always have their arguments head, then comp.
(defn s-to-np-vp [nps vp]
  (if (first nps)
    (lazy-cat (mycc10 vp (first nps))
              (s-to-np-vp (rest nps) vp))))

(defn vp-to-v-np [v nps]
  (if (first nps)
    (lazy-cat (myhh21 v (first nps))
              (vp-to-v-np v (rest nps)))))

(defn vp-to-propernoun-v [propernouns v]
  (mych21 v propernouns))

(def np-to-det-n
  (fn [filter]
    (do
      (log/info "looking for nouns..")
      (lazy-seq (base-cc10-random (merge filter))))))

(def proper-nouns
  ;; TODO: more compile-time filtering
  (lazy-shuffle cc10-comps))

(def pronouns
  ;; TODO: more compile-time filtering
  (filter (fn [lexeme]
            (and (= (get-in lexeme '(:synsem :cat)) :noun)
                 (= (get-in lexeme '(:synsem :subcat)) '())))
          lex/lexicon))

(defn sentences []
  (lazy-seq
   ;; parent: S -> NP VP
   (s-to-np-vp

    ;; Subject NP.
    (shuffle
     (list np-to-det-n
           proper-nouns))

    ;; VP.
    (let [expansion (first (take 1 (shuffle (list

                                             vp-to-v-np
                                             vp-to-propernoun-v

                                                  ))))]

      (cond (= expansion vp-to-v-np) ;; VP -> V NP
            (expansion
             (filter (fn [candidate]
                       ;; filter Vs to reduce number of candidates we need to filter:
                       ;; (only transitive verbs)
                       (and (not (= :notfound (unify/get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                            (= (unify/get-in candidate '(:synsem :cat)) :verb)))
                     (lazy-shuffle hh21-heads))

             ;; Object NP
             (shuffle
              (list np-to-det-n
                    proper-nouns)))

            (= expansion vp-to-propernoun-v) ;; VP -> Pronoun V
            (expansion

             ;; Object Pronoun
             (shuffle pronouns)

             (filter (fn [candidate]
                       ;; filter Vs to reduce number of candidates we need to filter:
                       ;; (only transitive verbs)
                       (and (not (= :notfound (unify/get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                            (= (unify/get-in candidate '(:synsem :cat)) :verb)))
                     (lazy-shuffle hh21-heads)))

            true (throw (Exception. (str "candidate function: " expansion " could not be handled (yet).")))
)))))

;; TODO: move to somewhere else that uses both grammar and lexicon (e.g. quiz or workbook): grammar itself should not depend on lexicon (lex/lexicon).
(defn random-sentence []
  (let [result
        (first (take 1 (sentences)))]
    (log/info "FO SAYS: " (fo result))
    result))

(defn random-sentences [n]
  (repeatedly n (fn [] (random-sentence))))

(defn speed-test [ & times]
  "TODO: show benchmark results and statistics (min,max,95%tile,stddev,etc)"
  (take 3 (repeatedly #(time (fo (random-sentence))))))

;(defn gen21 [heads comps]
;  (gen14 seed-phrases
;             heads
;             comps
;             sent-impl
;             0))

