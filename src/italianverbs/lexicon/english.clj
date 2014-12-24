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

(defn trans-intrans [spec & [opts]]
  [(unify
    spec
    transitive
    (if (:subj opts)
      {:synsem {:sem {:subj (:subj opts)}}}
      :top)
    (if (:obj opts) {:synsem {:sem {:obj (:obj opts)}}}
        :top))

   (unify
    spec
    (if (:subj opts)
      {:synsem {:sem {:subj (:subj opts)}}}
      :top)
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

   "ask"  (trans-intrans {:synsem {:sem {:pred :chiedere}}})

   "attend"  (trans-intrans {:synsem {:sem {:pred :frequentare}}})

   "be"
   (let [essere-common 
         (let [infl (ref :top)
               agr (ref :top)]
           {:synsem {:cat :verb
                     :subcat {:1 {:agr agr}}
                     :agr agr
                     :infl infl}
            :english {:agr agr
                      :infl infl
                      :present {:1sing "am"
                                :2sing "are"
                                :3sing "is"
                                :1plur "are"
                                :2plur "are"
                                :3plur "are"}
                          :past {:1sing "was"
                                 :2sing "were"
                                 :3sing "was"
                                 :1plur "were"
                                 :2plur "were"
                                 :3plur "were"}
                          :imperfetto "were being"}})]
     (unify essere-common
            {:synsem {:sem {:pred :essere}}}))

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

   "buy" (trans-intrans {:synsem {:sem {:pred :comprare}}
                         :english {:past "bought"}}

                        {:subj {:human true}
                         :obj {:buyable true}})

   "cat"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem (unify animal {:pred :gatto
                                        :pet true})}})

   "carry"  (trans-intrans {:synsem {:sem {:pred :portare}}
                            :english {:past "carried"}})

   "change" (trans-intrans {:synsem {:sem {:pred :cambiare}}}) ;; TODO: add reflexive sense

   "charge" (trans-intrans {:synsem {:sem {:pred :caricare}}})

   "come" (trans-intrans {:synsem {:sem {:pred :venire}}
                          :english {:past "came"}})

   "desire"  (trans-intrans {:synsem {:sem {:pred :desiderare}}})

   "dine"  (trans-intrans {:synsem {:sem {:pred :cenare}}})

   "drink" (trans-intrans
            {:synsem {:sem {:pred :bere
                            :discrete false}}
             :english {:past "drank"}}
            {:subj {:animate true}
             :obj {:drinkable true}})

   "drive"  (trans-intrans {:synsem {:sem {:pred :guidare}}
                            :english {:past "drove"}})

   "disappoint"  (trans-intrans {:synsem {:sem {:pred :deludere}}})
   "download"  (trans-intrans {:synsem {:sem {:pred :scaricare}}})

   "dog"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem (unify animal {:pred :cane
                                        :pet true})}})


   "eat"
   (trans-intrans
    {:english {:past "ate"}
     :synsem {:sem {:pred :mangiare
                    :subj {:animate true}}}}
    {:obj {:edible true}})

   "eat dinner"  (unify intransitive
                        {:synsem {:sem {:pred :cenare
                                        :subj {:human true}}}
                                 :english {:present {:3sing "eats dinner"}
                                           :past "ate dinner"}})

   "embrace"
   (trans-intrans {:synsem {:sem {:pred :abbracciare}}}
                  {:subj {:human true}}
                  {:obj {:human true}})

   "enter"  (trans-intrans {:synsem {:sem {:pred :entrare}}})

   "erase"  (trans-intrans {:synsem {:sem {:pred :cancellare}}})

   ;; TODO: account for "give" being ditransitive.
   "give" (trans-intrans {:synsem {:sem {:pred :dare}}
                          :english {:past "gave"}})

   "grab"  (trans-intrans {:synsem {:sem {:pred :prendere}}
                           :english {:imperfetto "grabbing"
                                     :past "grabbed"}})

   "have"
   (trans-intrans {:synsem {:cat :verb
                             :sem {:activity false
                                   :discrete false
                                   :pred :avere}}
                   :english {:english "have"
                             :present {:3sing "has"}
                             :past "had"}}
                  {:subj {:human true}
                   :obj {:buyable true}})

   "have dinner"  (trans-intrans {:synsem {:sem {:pred :cenare}}
                                  :english {:present {:3sing "has dinner"}
                                            :past "had dinner"
                                            :imperfetto "having dinner"}})

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

   "help"
   (trans-intrans {:synsem {:essere false
                            :sem {:pred :aiutare
                                  :activity true}}}
                  {:subj {:human true}
                   :obj {:human true}})


   "hug"
   (trans-intrans
          {:synsem {:sem {:pred :abbracciare}}}
          {:subj {:human true}
           :obj {:animate true}})

   "I"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :number :sing}
             :sem {:human true
                   :pred :io}
             :subcat '()}}

   "it"
   [{:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :number :sing}
              :sem {:pred :lui
                    :human false}
              :subcat '()}}
    {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :number :sing}
              :sem {:pred :lei
                    :human false}
              :subcat '()}}]

   "learn"  (trans-intrans {:synsem {:sem {:pred :imparare}}})

   "look for"  (trans-intrans {:synsem {:sem {:pred :cercare}}
                               :english {:imperfetto "looking for"
                                         :past "looked for"
                                         :present {:3sing "looks for"}}})

   "look up"  (trans-intrans {:synsem {:sem {:pred :cercare}}
                              :english {:imperfetto "looking up"
                                        :past "looked up"
                                        :present {:3sing "looks up"}}})
   "love" (trans-intrans {:synsem {:sem {:pred :amare}}}
                         {:subj {:human true}})

   "meet"  (trans-intrans {:synsem {:sem {:pred :incontrare}}
                           :english {:past "met"}})

   "mother"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem {:human true
                          :pred :madre
                          :child false}}})

   "paint"  (trans-intrans {:synsem {:sem {:pred :dipingere}}})
   "print"  (trans-intrans {:synsem {:sem {:pred :stampare}}})
   "play (games)" (trans-intrans {:synsem {:sem {:pred :giocare}}})
   "play (music)" (trans-intrans {:synsem {:sem {:pred :suonare}}})


   "read" ;; if this was a phonetic dictionary, there would be two entries for each pronounciation (i.e. both "reed" or "red" pronounciations)
   (trans-intrans {:english {:past "read (past)"}
                   :synsem {:sem {:pred :leggere
                                  :discrete false}}}
                  {:subj {:human true}}
                  {:obj {:legible true}})

   "receive"  (trans-intrans {:synsem {:sem {:pred :ricevere}}})
   "red"
   (unify adjective
          {:synsem {:cat :adjective
                    :sem {:pred :rosso
                          :comparative false
                          :physical-object true
                          :human false}}})
   "remember"  (trans-intrans {:synsem {:sem {:pred :ricordare}}})
   "respond"  (trans-intrans {:synsem {:sem {:pred :rispondere}}})
   "return" (flatten
             (list
              (trans-intrans {:synsem {:sem {:pred :ritornare}}})
              (trans-intrans {:synsem {:sem {:pred :tornare}}})))

   ;; TODO: search _within_ or _on_: depends on the object.
   ;;   "search"  (trans-intrans {:synsem {:sem {:pred :cercare}}})

   "see"  (trans-intrans {:synsem {:sem {:pred :vedere}}
                          :english {:past "saw"
                                    :past-participle "seen"}})

   "sell"  (trans-intrans {:synsem {:sem {:pred :vendere}}
                           :english {:past "sold"}})

   "send"  (trans-intrans {:synsem {:sem {:pred :mandare}}
                           :english {:past "sent"}})

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

   "sing"  (trans-intrans {:synsem {:sem {:pred :cantare}}
                           :english {:past "sang"}})

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
   (trans-intrans
    {:english {:past "spoke"}
     :synsem {:essere false
              :sem {:pred :parlare
                    :subj {:human true}}}}
    {:obj {:speakable true}})

   "study"  (trans-intrans {:synsem {:sem {:pred :studiare}}
                            :english {:past "studied"}})

   "teach"  (trans-intrans {:synsem {:sem {:pred :insegnare}}})

   "telephone" (trans-intrans {:synsem {:sem {:pred :telefonare}}})

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
             :sem {:pred :loro}
             :subcat '()}}

   "take"  (trans-intrans {:synsem {:sem {:pred :prendere}}
                           :english {:past "took"}})

   "upload"  (trans-intrans {:synsem {:sem {:pred :caricare}}})

   "use"  (trans-intrans {:synsem {:sem {:pred :usare}}})

   "we"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :number :plur}
             :sem {:human true
                   :pred :noi}
             :subcat '()}}

   "wear"  (trans-intrans {:english {:past "wore"}
                           :synsem {:sem {:pred :portare}}})

   "woman"
   (unify agreement-noun
          common-noun
          countable-noun
          {:english {:plur "women"}
           :synsem {:sem {:human true
                          :pred :donna
                          :child false}}})

   "work (human)"  (trans-intrans {:synsem {:sem {:pred :lavorare}}})

   "work (machines)" (trans-intrans {:english {:note "nonliving"} ;; TODO: add support in cloud for :note.
                                     :synsem {:sem {:subj {:living false}
                                                   :pred :funzionare}}})

   "write"  (trans-intrans {:english {:past "wrote"}
                            :synsem {:sem {:pred :scrivere}}})

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
