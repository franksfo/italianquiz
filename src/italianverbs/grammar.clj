(ns italianverbs.grammar
  (:refer-clojure :exclude [get-in])

  (:require [clojure.tools.logging :as log]
            [italianverbs.forest :as forest]
            [italianverbs.lexicon :refer (lexicon)]
            [italianverbs.ug :refer :all]
            [italianverbs.unify :refer (get-in unifyc)]
            [clojure.string :as string]))

(log/info "begin italian-english specific lexical categories..")

;; TODO: s/unifyc/unify/

(def grammar (list (merge (unifyc hh21
                                  {:synsem {:cat :adjective}})
                          {:comment "adjective-phrase"})
                                           
                   (merge (unifyc cc10
                                  {:synsem {:cat :noun}
                                   :comp {:phrasal false}}) ;; rathole prevention
                          {:comment "noun-phrase"})

                   (merge (unifyc hh21
                                  (let [head-synsem {:cat :intensifier
                                                     :modified true}]
                                    {:synsem head-synsem}))
                          {:comment "intensifier-phrase"})

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

                    (merge (unifyc cc10
                                  {:synsem {:infl :present
                                            :cat :verb
                                            :sem {:tense :present}}})
                          {:comment "s-present"})

                   (merge (unifyc hh21
                                  {:synsem {:infl :infinitive
                                            :cat :verb}})
                          {:comment "vp-infinitive"})

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

                   (merge (unifyc hh21
                                  {:synsem {:infl :present
                                            :sem {:tense :present}
                                            :cat :verb}})
                          {:comment "vp-present"})





))

;; This allows us to refer to individual grammar rules within grammar
;; by symbols like "vp-present" (e.g. (over vp-present lexicon)).
;; TODO: calling (.size) because (map) is lazy, and I want to realize
;; the sequence - must be a better way to loop over the grammar and realize the result.
(.size (map (fn [rule]
       (intern *ns* (symbol (:comment rule)) rule))
     grammar))

