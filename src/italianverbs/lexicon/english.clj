(ns italianverbs.lexicon.english)

(require '[italianverbs.lexiconfn :refer (unify)])
(require '[italianverbs.pos :refer (adjective animal
                                    cat-of-pronoun common-noun
                                    comparative
                                    countable-noun determiner
                                    drinkable-noun
                                    non-comparative-adjective noun
                                    pronoun-acc sentential-adverb
                                    verb verb-aux)])
(require '[italianverbs.pos.english :refer :all])

(defn trans-intrans [spec]
  [(unify
    spec
    transitive)

   (unify
    spec
    intransitive-unspecified-obj)])

(def lexicon-source
  {"Antonia"
   {:synsem {:sem {:pred :antonia
                   :human true}
             :agr {:number :sing
                   :person :3rd
                   :gender :fem}
             :propernoun true}}
   "Antonio"
   {:synsem {:agr {:number :sing
                   :person :3rd
                   :gender :masc}
             :sem {:pred :antonio
                   :human true}
             :propernoun true}}

   "a"
   {:synsem {:cat :det
             :def :indef
             :mass false
             :number :sing}}


   "black"
   (unify adjective
          {:synsem {:cat :adjective
                    :sem {:pred :nero
                          :comparative false
                          :physical-object true
                          :human false}}})


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

   "change" (unify {:synsem {:sem {:pred :cambiare}}} transitive)
   "erase"  (unify {:synsem {:sem {:pred :cancellare}}} transitive)
   "love" (trans-intrans {:synsem {:sem {:pred :amare}}})
   "sing"  (unify {:synsem {:sem {:pred :cantare}}} transitive)
   "charge" (unify {:synsem {:sem {:pred :caricare}}} transitive)
   "upload"  (unify {:synsem {:sem {:pred :caricare}}} transitive)
   "dine"  (unify {:synsem {:sem {:pred :cenare}}} transitive)
   "have dinner"  (unify {:synsem {:sem {:pred :cenare}}} transitive
                         {:english {:present {:3sing "has dinner"}
                                    :past "had dinner"}})
   "eat dinner"  (unify {:synsem {:sem {:pred :cenare}}} transitive
                         {:english {:present {:3sing "eats dinner"}
                                    :past "ate dinner"}})
   "desire"  (unify {:synsem {:sem {:pred :desiderare}}} transitive)
   "paint"  (unify {:synsem {:sem {:pred :dipingere}}} transitive)
   "enter"  (unify {:synsem {:sem {:pred :entrare}}} transitive)
   "attend"  (unify {:synsem {:sem {:pred :frequentare}}} transitive)
   "(machines) work"  (unify {:english {:note "nonliving"} ;; TODO: add support in cloud for :note.
                              :synsem {:sem {:subj {:living false}
                                             :pred :funzionare}}} transitive)
   "(game) play" (unify {:synsem {:sem {:pred :giocare}}} transitive)
   "drive"  (unify {:synsem {:sem {:pred :guidare}}} transitive)
   "learn"  (unify {:synsem {:sem {:pred :imparare}}} transitive)
   "meet"  (unify {:synsem {:sem {:pred :incontrare}}} transitive)
   "teach"  (unify {:synsem {:sem {:pred :insegnare}}} transitive)
   "(human) work"  (unify {:synsem {:sem {:pred :lavorare}}} transitive)
   "send"  (unify {:synsem {:sem {:pred :mandare}}} transitive)
   "carry"  (unify {:synsem {:sem {:pred :portare}}} transitive)
   "wear"  (unify {:synsem {:sem {:pred :portare}}} transitive)
   "grab"  (unify {:synsem {:sem {:pred :prendere}}} transitive
                  {:english {:imperfetto "grabbing"
                             :past "grabbed"}})
   "take"  (unify {:synsem {:sem {:pred :prendere}}} transitive
                  {:english {:past "took"}})
   "receive"  (unify {:synsem {:sem {:pred :ricevere}}} transitive)
   "remember"  (unify {:synsem {:sem {:pred :ricordare}}} transitive)
   "respond"  (unify {:synsem {:sem {:pred :rispondere}}} transitive)
   "return" [
             (unify {:synsem {:sem {:pred :ritornare}}} transitive)

             (unify {:synsem {:sem {:pred :tornare}}} transitive)]

   "download"  (unify {:synsem {:sem {:pred :scaricare}}} transitive)
   "write"  (unify {:english {:past "wrote"}
                    :synsem {:sem {:pred :scrivere}}} transitive)
   "print"  (unify {:synsem {:sem {:pred :stampare}}} transitive)
   "study"  (unify {:synsem {:sem {:pred :studiare}}} transitive)
   "(music) play"  (unify {:synsem {:sem {:pred :suonare}}} transitive)
   "ask"  (unify {:synsem {:sem {:pred :chiedere}}} transitive)
   "use"  (unify {:synsem {:sem {:pred :usare}}} transitive)
   "see"  (unify {:synsem {:sem {:pred :vedere}}} transitive
                 {:english {:past "saw"
                            :past-participle "seen"}})

   "sell"  (unify {:synsem {:sem {:pred :vendere}}
                   :english {:past "sold"}}
                  transitive)

   "dog"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem (unify animal {:pred :cane
                                        :pet true})}})



   "drink"
   (let [common {:synsem {:sem {:pred :bere
                                :discrete false
                                :subj {:animate true}}}}]
     [(unify
       common
       transitive
       {:synsem {:sem {:obj {:drinkable true}}}})

      (unify
       common
       intransitive-unspecified-obj)])

   "eat"
   (let [common {:synsem {:sem {:pred :mangiare
                                :discrete false
                                :subj {:animate true}}}}]
     [(unify
       common
       transitive
       {:synsem {:sem {:obj {:edible true}}}})

      (unify
       common
       intransitive-unspecified-obj)])
   
   "embrace"
   (unify transitive
          {:synsem {:sem {:pred :abbracciare
                          :subject {:human true}
                          :obj {:human true}}}})

   "I"
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

;   "hug"
;   (unify transitive
;          {:synsem {:sem {:pred :abbracciare
;                          :subj {:human true}
;                          :obj {:animate true}}}})

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

   "mother"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem {:human true
                          :pred :madre
                          :child false}}})

   "red"
   (unify adjective
          {:synsem {:cat :adjective
                    :sem {:pred :rosso
                          :comparative false
                          :physical-object true
                          :human false}}})

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
                    :pred :dormire}}
     :english {:past "slept"}})

   "some"
   [{:synsem {:cat :det
              :def :partitivo
              :number :plur}}
    {:synsem {:cat :det
              :def :indef
              :number :sing}}]

   "speak"
   (let [common
         {:synsem {:essere false
                   :sem {:pred :parlare
                         :subj {:human true}}}}]
     (unify common transitive
            {:synsem {:obj {:speakable true}}})
     (unify common intransitive intransitive-unspecified-obj))

   "the"
   {:synsem {:cat :det
             :def :def
             :mass false}}

   "they"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :number :plur}
             :sem {:human true
                   :pred :loro}
             :subcat '()}}

   "we"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :number :plur}
             :sem {:human true
                   :pred :noi}
             :subcat '()}}

   "woman"
   (unify agreement-noun
          common-noun
          countable-noun
          {:english {:plur "women"}
           :synsem {:sem {:human true
                          :pred :donna
                          :child false}}})

   "you"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :number :sing}
             :sem {:human true
                   :pred :tu}
             :subcat '()}}

   "you all"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :number :plur}
             :sem {:human true
                   :pred :voi}
             :subcat '()}}


})
