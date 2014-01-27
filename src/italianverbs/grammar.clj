(ns italianverbs.grammar
  (:refer-clojure :exclude [get-in])

  (:require [clojure.tools.logging :as log]
            [italianverbs.forest :as forest]
            [italianverbs.lexicon :refer (lexicon)]
            [italianverbs.ug :refer :all]
            [italianverbs.unify :refer (get-in unifyc)]
            [clojure.string :as string]))

(log/info "begin italian-english specific lexical categories..")

(def adjectives ;; italian comp-last for now (hc11-comps)
  (filter (fn [lexeme]
            (= (get-in lexeme '(:synsem :cat)) :adjective))
          hc11-comps))

(def common-nouns
  (filter (fn [lexeme]
            (and (= (get-in lexeme '(:synsem :cat)) :noun)
                 (= (get-in lexeme '(:synsem :subcat :1 :cat)) :det)))
          cc10-heads))

(def dets
  (filter (fn [lexeme]
            (= (get-in lexeme '(:synsem :cat)) :det))
          cc10-comps))

(def prepositions
  ;; TODO: more compile-time filtering
  (filter (fn [lexeme]
            (= (get-in lexeme '(:synsem :cat)) :prep))
          all-in-lexicon))

(def propernouns-and-pronouns
  ;; TODO: more compile-time filtering
  (filter (fn [lexeme]
            (and (= (get-in lexeme '(:synsem :cat)) :noun)
                 (= (get-in lexeme '(:synsem :subcat)) '())))
          cc10-comps))

(def pronouns
  ;; TODO: more compile-time filtering
  (filter (fn [lexeme]
            (and (= (get-in lexeme '(:synsem :cat)) :noun)
                 (not (= (get-in lexeme '(:synsem :pronoun)) false))))
          propernouns-and-pronouns))

(def propernouns
  ;; TODO: more compile-time filtering
  (filter (fn [lexeme]
            (and (= (get-in lexeme '(:synsem :cat)) :noun)
                 (not (= (get-in lexeme '(:synsem :pronoun)) true))))
          propernouns-and-pronouns))

(def transitive-verbs
  (filter (fn [candidate]
            ;; filter Vs to reduce number of candidates we need to filter:
            ;; (only transitive verbs)
            (and (not (= :notfound (get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                 (= (get-in candidate '(:synsem :cat)) :verb)
                 (= (get-in candidate '(:synsem :subcat :2 :cat)) :noun)))
          hh21-heads))

(def intransitive-verbs
  (filter (fn [lexeme]
            (and (= (get-in lexeme '(:synsem :cat)) :verb)
                 (= (get-in lexeme '(:synsem :subcat :2)) '())))
          all-in-lexicon))

(def intensifiers
  (filter (fn [lexeme]
            (= (get-in lexeme '(:synsem :cat)) :intensifier))
          all-in-lexicon))

;; TODO: more filtering
(def adverbial-verbs
  (filter (fn [lexeme]
            (and (= (get-in lexeme '(:synsem :cat)) :verb)
                 (= (get-in lexeme '(:italian :infinitive)) "fare")))
          all-in-lexicon))

(def adverbs
  (filter (fn [lexeme]
            (= (get-in lexeme '(:synsem :cat)) :adverb))
          all-in-lexicon))

(def aux-verbs
  (filter (fn [candidate]
            (and (not (= :notfound (get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                 (= (get-in candidate '(:synsem :cat)) :verb)
                 (= (get-in candidate '(:synsem :aux)) true)))
          hh21-heads))

(def modal-verbs
  (filter (fn [candidate]
            (and (not (= :notfound (get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                 (= (get-in candidate '(:synsem :cat)) :verb)
                 (= (get-in candidate '(:synsem :aux) false) false)
                 (= (get-in candidate '(:synsem :subcat :2 :cat)) :verb)))
          hh21-heads))

(def sent-adverbs
  (filter (fn [candidate]
            (= (get-in candidate '(:synsem :cat)) :sent-modifier))
          hh10-heads))

(log/info "done italian-english specific lexical categories.")

;; TODO: s/unifyc/unify/

(def grammar (list (merge (unifyc cc10
                                  {:synsem {:infl :present
                                            :cat :verb
                                            :sem {:tense :present}}})
                          {:comment "s-present"})

                   (merge (unifyc cc10
                                  {:synsem {:infl :present
                                            :cat :verb
                                            :sem {:tense :past}}})
                          {:comment "s-aux"})

                   (merge (unifyc cc10
                                  {:synsem {:infl :futuro
                                            :cat :verb
                                            :sem {:tense :future}}})
                          {:comment "s-future"})

                   (merge (unifyc cc10
                                  {:synsem {:infl :imperfetto
                                            :cat :verb
                                            :sem {:tense :past}}})
                          {:comment "s-imperfetto"})

                   (merge (unifyc hh21
                                  {:synsem {:infl :infinitive
                                            :cat :verb}})
                          {:comment "vp-infinitive"})

                   (merge (unifyc hh21
                                  {:synsem {:infl :present
                                            :sem {:tense :present}
                                            :cat :verb}})
                          {:comment "vp-present"})

                   (merge (unifyc hh21
                                  {:synsem {:infl :present
                                            :sem {:tense :past}
                                            :cat :verb}})
                          {:comment "vp-aux"})

                   (merge (unifyc hh21
                                  {:synsem {:infl :futuro
                                            :cat :verb}})
                          {:comment "vp-future"})

                   (merge (unifyc hh21
                                  {:synsem {:infl :imperfetto
                                            :cat :verb}})
                          {:comment "vp-imperfetto"})

                   (merge (unifyc hh21
                                  {:synsem {:infl :past
                                            :cat :verb}})
                          {:comment "vp-past"})


                   (merge (unifyc cc10
                                  {:synsem {:cat :noun}
                                   :comp {:phrasal false}}) ;; rathole prevention
                          {:comment "noun-phrase"})

                   (merge (unifyc hc11
                                  (let [head-synsem {:cat :noun
                                                     :modified true}]
                                    {:synsem head-synsem
                                     :head {:synsem {:modified false}}
                                     :comp {:phrasal false ;; rathole prevention
                                            :synsem {:cat :adjective
                                                     :mod head-synsem}}}))
                          {:comment "nbar"})


                   (merge (unifyc hh10
                                  {:synsem {:cat :prep}})
                          {:comment "prepositional-phrase"})


))

;; this rule-cache is defined outside any function so that all functions can share
;; a single cache.
(def rule-cache (forest/build-lex-sch-cache grammar lexicon))

(defn generate [ & [head]]
  (let [head (if head head :top)]
    (first (take 1 (forest/lightning-bolt head lexicon (shuffle grammar) 0 rule-cache)))))

(defn sentence [ & [ with ]]
  (generate {:synsem {:cat :verb :subcat '()}}))

(defn nounphrase [ & [ with ]]
  (generate (first (take 1 (generate {:synsem {:cat :noun :subcat '()}})))))


