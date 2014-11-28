(ns italianverbs.lexicon.english)

(require '[italianverbs.lexiconfn :refer (unify)])
(require '[italianverbs.pos :refer (adjective animal
                                    cat-of-pronoun common-noun
                                    comparative
                                    countable-noun determiner
                                    drinkable-noun
                                    non-comparative-adjective noun
                                    pronoun-acc pronoun-noun sentential-adverb
                                    verb verb-aux)])
(require '[italianverbs.pos.english :refer :all])

(def lexicon-source
  {"a"
   {:synsem {:cat :det
             :def :indef
             :mass false
             :number :sing}}


   "book"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem {:pred :libro
                          :legible true
                          :speakable false
                          :mass false
                          :buyable true
                          :consumable false
                          :artifact true}}})

   "cat"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem (unify animal {:pred :gatto
                                        :pet true})}})


   "embrace"
   (unify transitive
          {:synsem {:sem {:pred :abbracciare
                          :subj {:human true}}}})


   "i"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :number :sing}
             :sem {:human true
                   :pred :io}
             :subcat '()}}

   "he"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :masc
                   :number :sing}
             :sem {:human true
                   :pred :lui}
             :subcat '()}}

   "hug"
   (unify transitive
          {:synsem {:sem {:pred :abbracciare
                          :subj {:human true}
                          :obj {:living true}}}})

   "read" ;; if this was a phonetic dictionary, there would be two entries for each pronounciation (i.e. both "reed" or "red" pronounciations)
   (let [common {:synsem {:sem {:pred :leggere
                                :discrete false
                                :subj {:human true}}}}]
     [(unify
       common
       transitive
       {:synsem {:sem {:obj {:legible true}}}})

      (unify
       common
       intransitive-unspecified-obj)])

   "she"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :fem
                   :number :sing}
             :sem {:human true
                   :pred :lei}
             :subcat '()}}

   "sleep"
   (unify
    intransitive
    {:synsem {:cat :verb
              :sem {:subj {:animate true}
                    :discrete false
                    :pred :dormire}}})

   "the"
   {:synsem {:cat :det
             :def :def
             :mass false}}

   "you"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :number :sing}
             :sem {:human true
                   :pred :tu}
             :subcat '()}}


})
