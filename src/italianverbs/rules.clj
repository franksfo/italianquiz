(ns italianverbs.rules
  (:refer-clojure :exclude [get-in merge resolve])
  (:require [clojure.tools.logging :as log]
            [clojure.core :as core]
            [italianverbs.forest :as forest]
            [italianverbs.generate :refer :all]
            [italianverbs.grammar :refer :all]
            [italianverbs.lexicon :refer :all]
            [italianverbs.lexiconfn :refer :all]
            [italianverbs.morphology :refer :all]
            [italianverbs.ug :refer :all]
            [italianverbs.unify :refer :all :exclude [unify]]))

;; TODO: remove rewrite-as.
;; TODO: fold this into grammar.clj.

;; possible expansions of sentence (for now, only declarative sentences):
(ns-unmap 'italianverbs.rules 'declarative-sentence)

;; TODO: translate the keyword ':present' into Italian.
(rewrite-as declarative-sentence
            {:schema 'cc10
             :constraints #{{:synsem {:infl :present
                                      :sem {:tense :present}}}
                            {:synsem {:infl :present
                                      :sem {:tense :past}}}
                            {:synsem {:infl :futuro
                                      :sem {:tense :futuro}}}
                            {:synsem {:infl :imperfetto
                                      :sem {:tense :past}}}}
             :label 'decl-sent
             :comp 'np
             :head 'vp})

(ns-unmap 'italianverbs.rules 'sentence-with-modifier)
(rewrite-as sentence-with-modifier {:schema 'hh10
                                    :label 'sentence-with-modifier-left
                                    :head 'sent-adverbs
                                    :comp 'declarative-sentence})

;; possible expansions of np (noun phrase):
;;
(ns-unmap 'italianverbs.rules 'np)
(rewrite-as np {:schema 'cc10
                :comp 'dets
                :head 'common-nouns})
(rewrite-as np {:schema 'cc10
                :comp 'dets
                :head 'nbar})
(rewrite-as np 'propernouns)
(rewrite-as np 'pronouns)

(ns-unmap 'italianverbs.rules 'nbar)
(rewrite-as nbar {:schema 'hc11
                  :comp 'adjectives
                  :head 'common-nouns})

;; possible expansions of vp (verb phrase):
;;
(ns-unmap 'italianverbs.rules 'vp)

(rewrite-as vp 'intransitive-verbs)
(rewrite-as vp 'modal-vp)
(rewrite-as vp 'past-vp)
(rewrite-as vp 'transitive-vp)

(ns-unmap 'italianverbs.rules 'trans-vp-with-pron)
(rewrite-as trans-vp-with-pron {:schema 'ch21
                                :label 'vp
                                :comp 'pronouns
                                :head 'transitive-verbs})

(rewrite-as vp 'trans-vp-with-pron)

(rewrite-as vp {:schema 'hh21
                :label 'vp-prep
                :comp 'pp
                :head 'vp-adv})

(rewrite-as vp {:schema 'hh21
                :label 'vp-intensifier
                :comp 'intensifier-phrase
                :head 'transitive-verbs}) ;; TODO: restrict to e.g. 'copula verbs' or 'essere'

(ns-unmap 'italianverbs.rules 'vp-adv)
(rewrite-as vp-adv {:schema 'hh32
                    :label 'vp-adv
                    :comp 'adverbs
                    :head 'adverbial-verbs})

(ns-unmap 'italianverbs.rules 'intensifier-phrase)
(rewrite-as intensifier-phrase {:schema 'hh21
                            :label 'intensifier-phrase
                            :comp 'adj-phrase
                            :head 'intensifiers})

(ns-unmap 'italianverbs.rules 'pp)
(rewrite-as pp {:schema 'hh10
                :label 'pp
                :comp 'vp
                :head 'prepositions})
(rewrite-as pp {:schema 'hh10
                :label 'pp
                :comp 'np
                :head 'prepositions})


(ns-unmap 'italianverbs.rules 'modal-vp)
(rewrite-as modal-vp {:schema 'hh21
                      :label 'modal-vp
                      :head 'modal-verbs
                      :comp 'intransitive-verbs})
(rewrite-as modal-vp {:schema 'hh21
                      :label 'modal-vp
                      :head 'modal-verbs
                      :comp 'transitive-vp})

;; possible expansions of transitive vp (verb phrase):
;;
;; undefine any previous values: TODO: should be a one-liner.
(ns-unmap 'italianverbs.rules 'transitive-vp)
(rewrite-as transitive-vp {:schema 'hh21
;                           :constraints #{{:synsem {:sem {:tense :past}
;                                                    :infl :past}}}
                           :comp 'np
                           :head 'transitive-verbs})
(ns-unmap 'italianverbs.rules 'past-vp)
(rewrite-as past-vp {:schema 'hh21
                     :label 'past-vp
                     :head 'aux-verbs
                     :comp 'intransitive-verbs})
(rewrite-as past-vp {:schema 'hh21
                     :label 'past-vp
                     :head 'aux-verbs
                     :comp 'transitive-vp})
(rewrite-as past-vp {:schema 'hh21
                     :label 'past-vp
                     :head 'aux-verbs
                     :comp 'modal-vp})

(ns-unmap 'italianverbs.rules 'adj-phrase)
(rewrite-as adj-phrase {:schema 'hh21
                        :label 'adj-phrase
                        :head 'adjectives
                        :comp 'pp})
+;; Working on this:
+;;
+;; (fo (take 1 (generate (shuffle adj-phrase) "adj-phase" :top sem-impl)))
+

;; -- aliases --
(def ds declarative-sentence)

(ns-unmap 'italianverbs.rules 'sents)
(rewrite-as sents 'ds)
(rewrite-as sents 'sentence-with-modifier)

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


