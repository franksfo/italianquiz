(ns italianverbs.test.lexicon.english)

(require '[italianverbs.lexiconfn :refer [unify]])
(require '[italianverbs.pos :refer [adjective animal
                                    cat-of-pronoun common-noun
                                    comparative
                                    countable-noun determiner
                                    drinkable-noun
                                    non-comparative-adjective noun
                                    pronoun-acc sentential-adverb
                                    verb verb-aux]])
(require '[italianverbs.pos.english :refer :all])
(require '[italianverbs.unify :refer [dissoc-paths]])
(require '[italianverbs.lexicon.english :refer :all])

(def test-lexicon {"buy" {:synsem {:cat :verb
                                   :sem {:pred :comprare
                                         :subj {:human true}
                                         :obj {:buyable true}}}}})




