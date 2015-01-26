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

(defn intrans [spec & [opts]]
  [(unify
    spec
    transitive
    (if (:subj opts)
      {:synsem {:sem {:subj (:subj opts)}}}
      :top)
    (if (:obj opts) {:synsem {:sem {:obj (:obj opts)}}}
        :top))])

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


   "abandon" (trans-intrans {:synsem {:cat :verb
                                      :sem {:pred :abandon}}})


   "accept"  (trans-intrans {:synsem {:cat :verb
                                      :sem {:pred :accept}}})
   
   "accompany" (trans-intrans {:synsem {:cat :verb
                                        :sem {:pred :accompany}}
                               :english {:past "accompanied"}})


   "announce" (trans-intrans {:synsem {:cat :verb
                                       :sem {:pred :announce}}})

   "answer"  (trans-intrans {:synsem {:sem {:pred :answer}}})

   "ask"  (trans-intrans {:synsem {:sem {:pred :chiedere
                                         :subj {:human true}}}})

   "assure" (trans-intrans {:synsem {:cat :verb
                                     :sem {:pred :assure}}})

   "attend"  (trans-intrans {:synsem {:sem {:pred :frequentare}}})

   "avoid" (trans-intrans {:synsem {:sem {:pred :avoid}}})

   "bag" {:synsem {:cat :noun
                   :sem {:pred :bag
                         :place false}}}

   "base" (trans-intrans {:synsem {:cat :verb
                                   :sem {:pred :support}}})
   
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
                             :3plur "were"}}})]
     (unify essere-common
            {:synsem {:sem {:pred :essere}}}))


   "be missing"
   (let [essere-common 
         (let [infl (ref :top)
               agr (ref :top)]
           {:synsem {:cat :verb
                     :subcat {:1 {:agr agr}}
                     :agr agr
                     :infl infl}
            :english {:agr agr
                      :infl infl
                      :present {:1sing "am missing"
                                :2sing "are missing"
                                :3sing "is missing"
                                :1plur "are missing"
                                :2plur "are missing"
                                :3plur "are missing"}
                      :past {:1sing "was missing"
                             :2sing "were missing"
                             :3sing "was missing"
                             :1plur "were missing"
                             :2plur "were missing"
                             :3plur "were missing"}}})]
     (unify essere-common
            {:synsem {:sem {:pred :to-be-missing}}}))

   "bicycle" {:synsem {:cat :noun
                   :sem {:pred :bicycle
                         :place false}}}

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
                          :place false
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

   "car" {:synsem {:cat :noun
                   :sem {:pred :car
                         :place false}}}

   "carry"  (trans-intrans {:synsem {:sem {:pred :portare}}
                            :english {:past "carried"}})

   "change" (trans-intrans {:synsem {:sem {:pred :cambiare}}}) ;; TODO: add reflexive sense

   "charge" (trans-intrans {:synsem {:sem {:pred :caricare}}})

   "come" (trans-intrans {:synsem {:sem {:pred :venire}}
                          :english {:past "came"}})

   "comment" (trans-intrans {:synsem {:cat :verb
                                      :sem {:pred :comment}}})

   "conserve" (trans-intrans {:synsem {:cat :verb
                                       :sem {:pred :conserve}}})

   "consider" (trans-intrans  {:synsem {:cat :verb
                                        :sem {:pred :consider}}})

   "correspond" (trans-intrans  {:synsem {:cat :verb
                          :sem {:pred :correspond}}})

   "create" (trans-intrans  {:synsem {:cat :verb
                                      :sem {:pred :create}}})

   "cut" (trans-intrans  {:english {:past "cut"
                                    :participle "cutting"}
                          :synsem {:cat :verb
                                   :sem {:pred :cut}}})

   "decide" (trans-intrans  {:english {:imperfect "was deciding"}
                             :synsem {:cat :verb
                                      :sem {:pred :decide}}})

   "desire"  (trans-intrans {:synsem {:sem {:pred :desiderare}}})

   "dine"  (intrans {:synsem {:sem {:pred :cenare
                                    :subj {:human true}}}})

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


   "earn"  (trans-intrans {:synsem {:sem {:pred :earn
                                          :subj {:human true}}}})

   "eat"
   (trans-intrans
    {:english {:past "ate"}
     :synsem {:sem {:pred :mangiare
                    :subj {:animate true}}}}
    {:obj {:edible true}})

   "eat dinner"  (intrans
                  {:synsem {:sem {:pred :cenare
                                  :subj {:human true}}}
                   :english {:present {:3sing "eats dinner"}
                             :participle "eating dinner"
                             :past "ate dinner"}})

   "embrace"
   (trans-intrans {:synsem {:sem {:pred :abbracciare}}}
                  {:subj {:human true}}
                  {:obj {:human true}})

   "enjoy" {:synsem {:cat :verb
                     :sem {:pred :enjoy}}}

   "enter"  (trans-intrans {:synsem {:sem {:pred :enter}}})

   "erase"  (trans-intrans {:synsem {:sem {:pred :cancellare}}})

   "escape" (trans-intrans {:synsem {:sem {:pred :escape}}})

   "finish" (trans-intrans {:synsem {:cat :verb
                                     :sem {:pred :finish}}})

   "game" {:synsem {:cat :noun
                    :sem {:pred :game
                          :games true}}}

   ;; TODO: account for "give" being ditransitive.
   "give" (trans-intrans {:synsem {:sem {:pred :dare}}
                          :english {:past "gave"}})


   "go"
   (trans-intrans {:synsem {:cat :verb
                            :sem {:activity true
                                  :discrete false
                                  :pred :andare
                                  :subj {:animate true}}}
                   :english {:english "go"
                             :present {:3sing "goes"}
                             :participle "going"
                             :past "went"}})

   "grab"  (trans-intrans {:synsem {:sem {:pred :prendere}}
                           :english {:participle "grabbing"
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
                                            :participle "having dinner"}})

   "have to" (trans-intrans {:synsem {:sem {:pred :have-to}}
                             :english {:present {:1sing "have to"
                                                 :2sing "have to"
                                                 :3sing "has to"
                                                 :1plur "have to"
                                                 :2plur "have to"
                                                 :3plur "have to"}
                                       :future "will have to"
                                       :participle "having to"
                                       :past "had to"}})

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

   "hold"
   (trans-intrans {:synsem {:sem {:pred :tenere}}
                   :english {:past "held"}})

   "hug"
   (trans-intrans
          {:synsem {:sem {:pred :abbracciare}}
           :english {:past "hugged"
                     :participle "hugging"}}
          {:subj {:human true}
           :obj {:animate true}})

   "I (♂)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:gender :masc
                   :person :1st
                   :number :sing}
             :sem {:human true
                   :pred :io}
             :subcat '()}}

   "I (♀)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:gender :fem
                   :person :1st
                   :number :sing}
             :sem {:human true
                   :pred :io}
             :subcat '()}}

   "increase" {:synsem {:cat :verb
                        :sem {:pred :increase}}}

   "insure" {:synsem {:cat :verb
                      :sem {:pred :insure}}}

   "it (♂)"
   {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :number :sing}
              :sem {:pred :lui
                    :human false}
              :subcat '()}}

   "it (♀)"
    {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :number :sing}
              :sem {:pred :lei
                    :human false}
              :subcat '()}}

   "keep"
   [(trans-intrans {:synsem {:sem {:pred :tenere}}
                    :english {:past "kept"}})
    (trans-intrans {:synsem {:sem {:pred :keep-safe}}
                    :english {:note "keep something safe"
                              :past "kept"}})]


   "key" {:synsem {:cat :noun
                   :sem {:pred :key
                         :place false}}}

   "learn"  (trans-intrans {:synsem {:sem {:pred :imparare}}})

   "leave" {:english {:past "left"}
            :synsem {:cat :verb
                     :sem {:pred :leave-behind
                           :obj {:place false}}}}

   "look for"  (trans-intrans {:synsem {:sem {:pred :cercare}}
                               :english {:participle "looking for"
                                         :past "looked for"
                                         :present {:3sing "looks for"}}})

   "look up"  (trans-intrans {:synsem {:sem {:pred :cercare}}
                              :english {:participle "looking up"
                                        :past "looked up"
                                        :present {:3sing "looks up"}}})
   "love" (trans-intrans {:synsem {:sem {:pred :amare}}}
                         {:subj {:human true}})

   "lower" {:synsem {:cat :verb
                     :sem {:pred :lower}}}

   "meet"  (trans-intrans {:synsem {:sem {:pred :incontrare}}
                           :english {:past "met"}})

   "mother"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem {:human true
                          :pred :madre
                          :child false}}})

   "music" {:synsem {:cat :noun
                    :sem {:pred :music}}}

   "paint"  (trans-intrans {:synsem {:sem {:pred :dipingere}}})
   "print"  (trans-intrans {:synsem {:sem {:pred :stampare}}})

   "play" (concat

           (trans-intrans {:comment "We are talking about playing games or sports."
                           :synsem {:sem {:pred :giocare}}})
;                                          :obj {:games true}}}})

           (trans-intrans {:comment "We are talking about playing music or sounds."
                           :synsem {:sem {:pred :suonare}}}))
;                                          :obj {:music true}}}}))

   "preserve" {:synsem {:cat :verb
                        :sem {:pred :preserve}}}

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
   "respond"  (trans-intrans {:synsem {:sem {:pred :answer}}})
   "return" (flatten
             (list
              (trans-intrans {:synsem {:sem {:pred :ritornare}}})
              (trans-intrans {:synsem {:sem {:pred :tornare}}})))

   "run" (trans-intrans {:english {:past "ran"
                                   :participle "run"}
                         :synsem {:sem {:pred :run}}})

   ;; TODO: search _within_ or _on_: depends on the object.
   ;;   "search"  (trans-intrans {:synsem {:sem {:pred :cercare}}})

   "see"  (trans-intrans {:synsem {:sem {:pred :vedere}}
                          :english {:past "saw"
                                    :past-participle "seen"}})

   "sell"  (trans-intrans {:synsem {:sem {:pred :vendere
                                          :subj {:human true}}}
;                                          :obj {:human false}}}
                           :english {:past "sold"}})

   "send"  (trans-intrans {:synsem {:sem {:pred :mandare}}
                           :english {:past "sent"}})

   "share" {:synsem {:cat :verb
                     :sem {:pred :share}}}

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

   "show"  (trans-intrans {:synsem {:sem {:pred :show}}})

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
    {:english {:past "spoke"
               :past-participle "spoken"}
     :synsem {:sem {:pred :speak
                    :subj {:human true}
                    :obj {:speakable true}}}})

   "study"  (trans-intrans {:synsem {:sem {:pred :study}}
                            :english {:past "studied"}})

   "support" (trans-intrans {:synsem {:cat :verb
                                      :sem {:pred :support}}})

   "take advantage of" (trans-intrans {:english {:past "took advantage of"
                                                 :participle "taking advantage of"
                                                 :past-participle "taken advantage of"}
                                       :synsem {:cat :verb
                                                :sem {:pred :take-advantage-of}}})

   "teach"  (trans-intrans {:synsem {:sem {:pred :teach}}
                            :english {:past "taught"}})

   "telephone" (trans-intrans {:synsem {:sem {:pred :telefonare}}})

   "the"
   {:synsem {:cat :det
             :def :def
             :mass false}}



   "they (♂)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :masc
                   :number :plur}
             :sem {:human true
                   :pred :loro}
             :subcat '()}}

   "they (♀)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :fem
                   :number :plur}
             :sem {:human true
                   :pred :loro}
             :subcat '()}}


   "throw out"
   (trans-intrans {:synsem {:sem {:pred :throw-out}}
                   :english {:past "threw out"}})

   "take"  (trans-intrans {:synsem {:sem {:pred :prendere}}
                           :english {:past "took"}})

   "understand" (trans-intrans {:english {:past "understood"}
                                :synsem {:cat :verb
                                         :sem {:pred :understand}}})
                          

   "upload"  (trans-intrans {:synsem {:sem {:pred :caricare}}})

   "use"  (trans-intrans {:synsem {:sem {:pred :usare}}})

   "wait"  (trans-intrans {:synsem {:sem {:pred :wait-for}}})

   "we (♀)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :gender :fem
                   :number :plur}
             :sem {:human true
                   :pred :noi}
             :subcat '()}}

   "we (♂)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :gender :masc
                   :number :plur}
             :sem {:human true
                   :pred :noi}
             :subcat '()}}

   "wear"  (trans-intrans {:english {:past "wore"}
                           :synsem {:sem {:pred :portare}}})


   "win"  (trans-intrans {:synsem {:sem {:pred :win
                                         :subj {:human true}}}
                          :english {:past "won"}})

   "woman"
   (unify agreement-noun
          common-noun
          countable-noun
          {:english {:plur "women"}
           :synsem {:sem {:human true
                          :pred :donna
                          :child false}}})

   "work" [(trans-intrans {:synsem {:sem {:pred :work-human
                                          :subj {:human true}}}
                           :english {:note "human"}})

           (trans-intrans {:english {:note "nonliving or machines"} ;; TODO: add support in cloud for :note.
                           :synsem {:sem {:subj {:living false
                                                 :human false ;; should not need to add human=false and animate=false: living=false should suffice.
                                                 :animate false}
                                          :pred :work-nonhuman}}})]
   
   "write"  (trans-intrans {:english {:past "wrote"
                                      :past-participle "written"}
                            :synsem {:sem {:pred :scrivere}}})

   "you (♂)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :gender :masc
                   :number :sing}
             :sem {:human true
                   :pred :tu}
             :subcat '()}}

   "you (♀)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :gender :fem
                   :number :sing}
             :sem {:human true
                   :pred :tu}
             :subcat '()}}


   "you all (♂)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :gender :masc
                   :number :plur}
             :sem {:human true
                   :pred :voi}
             :subcat '()}}

   "you all (♀)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :gender :fem
                   :number :plur}
             :sem {:human true
                   :pred :voi}
             :subcat '()}}


})
