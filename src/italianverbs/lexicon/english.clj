(ns italianverbs.lexicon.english)

(require '[italianverbs.lexiconfn :refer [unify]])
(require '[italianverbs.pos :refer [adjective animal
                                    common-noun
                                    countable-noun]])
(require '[italianverbs.pos.english :refer [agreement-noun]])

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

   "abandon" {:synsem {:cat :verb
                       :sem {:pred :abandon}}}

   "accept"  {:synsem {:cat :verb
                       :sem {:pred :accept}}}
   
   "accompany" {:synsem {:cat :verb
                         :sem {:pred :accompany}}
                :english {:past "accompanied"}}


   "announce" {:synsem {:cat :verb
                        :sem {:pred :announce}}}

   "answer" {:synsem {:cat :verb
                      :sem {:pred :answer
                            :subj {:human true}}}}

   "ask" {:synsem {:cat :verb
                   :sem {:pred :chiedere
                         :subj {:human true}}}}

   "assure" {:synsem {:cat :verb
                      :sem {:pred :assure}}}

   "attend" {:synsem {:cat :verb
                      :sem {:pred :frequentare}}}

   "avoid" {:synsem {:cat :verb
                     :sem {:pred :avoid}}}

   "bag" {:synsem {:cat :noun
                   :sem {:pred :bag
                         :place false}}}

   "base" {:synsem {:cat :verb
                    :sem {:pred :support}}}
   
   "be"
   {:synsem {:cat :verb
             :sem {:pred :essere}}
    :english {:english "be"
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
                     :3plur "were"}}}

   "be missing"
   {:english {:english "be missing"
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
                     :3plur "were missing"}}
    :synsem {:cat :verb
             :sem {:pred :to-be-missing}}}

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

   "buy" {:synsem {:cat :verb
                   :sem {:pred :comprare
                         :subj {:human true}
                         :obj {:buyable true}}}
          :english {:past "bought"
                    :present {:3sing "buys"}}}

   "car" {:synsem {:cat :noun
                   :sem {:pred :car
                         :place false}}}

   "carry" {:synsem {:cat :verb
                     :sem {:pred :portare}}
            :english {:past "carried"}}

   "cat"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem (unify animal {:pred :gatto
                                        :pet true})}})

   "change" {:synsem {:cat :verb
                      :sem {:pred :cambiare}}} ;; TODO: add reflexive sense

   "charge" {:synsem {:cat :verb
                      :sem {:pred :caricare}}}

   "come" {:synsem {:cat :verb
                    :sem {:pred :venire}}
           :english {:past "came"}}

   "comment" {:synsem {:cat :verb
                       :sem {:pred :comment}}}

   "conserve" {:synsem {:cat :verb
                        :sem {:pred :conserve}}}

   "consider" {:synsem {:cat :verb
                        :sem {:pred :consider}}}

   "correspond" {:synsem {:cat :verb
                          :sem {:pred :correspond}}}

   "create" {:synsem {:cat :verb
                      :sem {:pred :create}}}

   "cut" {:english {:past "cut"
                    :participle "cutting"}
          :synsem {:cat :verb
                   :sem {:pred :cut}}}

   "decide" {:synsem {:cat :verb
                      :sem {:pred :decide}}}

   "desire" {:synsem {:cat :verb
                      :sem {:pred :desire}}}

   "develop" {:synsem {:cat :verb
                       :sem {:pred :develop}}}

   "dine" {:synsem {:cat :verb
                    :sem {:pred :cenare
                          :subj {:human true}}}}

   "drink" {:synsem {:cat :verb
                     :sem {:pred :bere
                           :discrete false
                           :subj {:animate true}
                           :obj {:drinkable true}}}}

   "drive" {:synsem {:cat :verb
                     :sem {:pred :guidare}}
            :english {:past "drove"}}

   "disappoint" {:synsem {:cat :verb
                          :sem {:pred :deludere}}}
   "download" {:synsem {:cat :verb
                        :sem {:pred :scaricare}}}

   "dog"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem (unify animal {:pred :cane
                                        :pet true})}})


   "earn"  {:synsem {:cat :verb
                     :sem {:pred :earn
                           :subj {:human true}}}}

   "eat"
   {:english {:past "ate"}
    :synsem {:cat :verb
             :sem {:pred :mangiare
                   :subj {:animate true}
                   :obj {:edible true}}}}

   "eat dinner" {:synsem {:cat :verb
                          :sem {:pred :cenare
                                :subj {:human true}}}
                 :english {:present {:3sing "eats dinner"}
                           :participle "eating dinner"
                           :past "ate dinner"}}

   "embrace"
   {:synsem {:cat :verb
             :sem {:pred :abbracciare}
             :subj {:human true}
             :obj {:human true}}}


   "enjoy" {:english {:present {:3sing "enjoys"}}
            :synsem {:cat :verb
                     :sem {:pred :enjoy}}}
  
   "enter"  {:synsem {:cat :verb
                      :sem {:pred :enter}}}

   "erase"  {:synsem {:cat :verb
                      :sem {:pred :cancellare}}}

   "escape" {:synsem {:cat :verb
                      :sem {:pred :escape}}}

   "exist" {:synsem {:cat :verb
                     :sem {:pred :exist}}}

   "express" {:synsem {:cat :verb
                       :sem {:pred :express}}}

   "finish" {:synsem {:cat :verb
                      :sem {:pred :finish}}}

   "form" {:synsem {:cat :verb
                    :sem {:pred :form}}}

   "game" {:synsem {:cat :noun
                    :sem {:pred :game
                          :games true}}}

   ;; TODO: account for "give" being ditransitive.
   "give" {:synsem {:cat :verb
                    :sem {:pred :dare}}
           :english {:past "gave"}}

   "go"
   [{:synsem {:cat :verb
              :sem {:activity true
                    :discrete false
                    :pred :andare
                    :subj {:animate true}}}
     :english {:english "go"
               :present {:3sing "goes"}
               :participle "going"
               :past "went"}}]

   "grab"  {:synsem {:cat :verb
                     :sem {:pred :prendere}}
            :english {:participle "grabbing"
                      :past "grabbed"}}

   "have" {:synsem {:cat :verb
                    :sem {:activity false
                          :discrete false
                          :pred :avere
                          :subj {:human true}
                          :obj {:buyable true}}}
           :english {:english "have"
                     :present {:3sing "has"}
                             :past "had"}}


   "have dinner" {:synsem {:cat :verb
                            :sem {:pred :cenare}}
                   :english {:present {:3sing "has dinner"}
                             :past "had dinner"
                             :participle "having dinner"}}

   "have to" {:synsem {:cat :verb
                       :sem {:pred :have-to}}
              :english {:present {:1sing "have to"
                                  :2sing "have to"
                                  :3sing "has to"
                                  :1plur "have to"
                                  :2plur "have to"
                                  :3plur "have to"}
                        :future "have to"
                        :participle "having to"
                        :past "had to"}}

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
   {:synsem {:cat :verb
             :essere false
             :sem {:pred :aiutare
                   :activity true
                   :subj {:human true}
                   :obj {:human true}}}}

   "hold"
   {:synsem {:cat :verb
             :sem {:pred :tenere}}
    :english {:past "held"}}

   "hope"
   {:synsem {:cat :verb
             :sem {:pred :hope}}}
   
   "hug"
   {:synsem {:sem {:pred :abbracciare
                   :subj {:human true}
                   :obj {:animate true}}}
    :english {:past "hugged"
              :participle "hugging"}}

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

   "is missed" {:synsem {:sem {:pred :mancare}}
                :english {:participle "being missed"
                          :future "be missed"
                          :english "be missed"
                          :conditional "be missed"
                          :past {:english "was missed"
                                 :2sing "were missed"
                                 :2plur "were missed"
                                 :3plur "were missed"}
                          :present {:2sing "are missed"
                                    :3sing "is missed"
                                    :2plur "are missed"
                                    :3plur "are missed"}}}

   "it (♂)"
   {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :number :sing}
              :sem {:pred :lui
                    :gender :masc
                    :human false}
              :subcat '()}}

   "it (♀)"
    {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :number :sing}
              :sem {:pred :lei
                    :gender :fem
                    :human false}
              :subcat '()}}

   "keep"
   [{:synsem {:cat :verb
              :sem {:pred :tenere}}
     :english {:past "kept"}}
    {:synsem {:cat :verb
              :sem {:pred :keep-safe}}
     :english {:note "(something safe)"
               :past "kept"}}]

   "key" {:synsem {:cat :noun
                   :sem {:pred :key
                         :place false}}}

   "learn" {:synsem {:cat :verb
                     :sem {:pred :imparare}}}

   "leave" {:english {:past "left"}
            :synsem {:cat :verb
                     :sem {:pred :leave-behind
                           :obj {:place false}}}}

   "listen to" {:synsem {:cat :verb
                         :sem {:pred :listen-to}}
                :english {:participle "listening to"
                          :past "listened to"
                          :present {:3sing "listens to"}}}

   "look for" {:synsem {:cat :verb
                        :sem {:pred :cercare}}
               :english {:participle "looking for"
                         :past "looked for"
                         :present {:3sing "looks for"}}}

   "look up" {:synsem {:cat :verb
                       :sem {:pred :cercare}}
              :english {:participle "looking up"
                        :past "looked up"
                        :present {:3sing "looks up"}}}

   "love" {:synsem {:cat :verb
                    :sem {:pred :amare
                          :subj {:human true}}}}

   "lower" {:synsem {:cat :verb
                     :sem {:pred :lower}}}

   "meet"  {:synsem {:sem {:pred :incontrare}}
            :english {:past "met"}}

   "mother"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem {:human true
                          :pred :madre
                          :child false}}})

   "music" {:synsem {:cat :noun
                    :sem {:pred :music}}}

   "paint"  {:synsem {:cat :verb
                      :sem {:pred :dipingere}}}

   ;; TODO: 3sing present exception used below to avoid "playies" is not an exception: it's a rule: y->ys.
   ;; the exceptional case is when "ys" is not used (e.g. "tries").
   "play" [{:comment "We are talking about playing games or sports."
            :english {:present {:3sing "plays"}}
            :synsem {:sem {:pred :giocare
                           :subj {:human true}
                           :obj {:games true}}}}

           {:comment "We are talking about playing music or sounds."
            :english {:present {:3sing "plays"}}
            :synsem {:sem {:pred :suonare
                           :subj {:human true}
                           :obj {:music true}}}}]

   "preserve" {:synsem {:cat :verb
                        :sem {:pred :preserve}}}


   "print"  {:synsem {:cat :verb
                      :sem {:pred :stampare}}}

   "read" ;; if this was a phonetic dictionary, there would be two entries for each pronounciation (i.e. both "reed" or "red" pronounciations)
   {:english {:past "read (past)"}
    :synsem {:cat :verb
             :sem {:pred :leggere
                   :discrete false
                   :subj {:human true}
                   :obj {:legible true}}}}
   
   "receive"  {:synsem {:cat :verb
                        :sem {:pred :ricevere}}}
   "red"
   (unify adjective
          {:synsem {:cat :adjective
                    :sem {:pred :rosso
                          :comparative false
                          :physical-object true
                          :human false}}})

   "remember"  {:synsem {:cat :verb
                         :sem {:pred :ricordare}}}

   "respond"  {:synsem {:cat :verb
                        :sem {:pred :answer}}}

   "return" [{:synsem {:cat :verb
                       :sem {:pred :ritornare}}}
             {:synsem {:cat :verb
                       :sem {:pred :tornare}}}]

   "run" {:english {:past "ran"
                    :participle "running"
                    :past-participle "run"}
          :synsem {:cat :verb
                   :sem {:pred :run}}}

   ;; TODO: search _within_ or _on_: depends on the object.
   ;;   "search"  {:synsem {:sem {:pred :cercare}}})

   "see"  {:synsem {:cat :verb
                    :sem {:pred :vedere}}
           :english {:past "saw"
                     :past-participle "seen"}}

   "sell"  {:synsem {:cat :verb
                     :sem {:pred :vendere
                           :subj {:human true}
                           :obj {:human false}}}
            :english {:past "sold"}}

   "send"  {:synsem {:cat :verb
                     :sem {:pred :mandare}}
                           :english {:past "sent"}}

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

   "show" {:synsem {:cat :verb
                    :sem {:pred :show}}}

   "sing" {:synsem {:cat :verb
                    :sem {:pred :cantare}}
           :english {:past "sang"}}

   "sleep" {:synsem {:cat :verb
                     :sem {:subj {:animate true}
                           :discrete false
                           :pred :dormire}}
            :english {:past "slept"}}

   "some"
   [{:synsem {:cat :det
              :def :partitivo
              :number :plur}}
    {:synsem {:cat :det
              :def :indef
              :number :sing}}]

   "speak"
   {:english {:past "spoke"
              :past-participle "spoken"}
    :synsem {:cat :verb
             :sem {:pred :speak
                   :subj {:human true}
                   :obj {:speakable true}}}}

   "study"  {:synsem {:cat :verb
                      :sem {:pred :study}}
             :english {:past "studied"}}

   "support" {:synsem {:cat :verb
                       :sem {:pred :support}}}


   "take advantage of" {:english {:past "took advantage of"
                                  :participle "taking advantage of"
                                  :past-participle "taken advantage of"}
                        :synsem {:cat :verb
                                 :sem {:pred :take-advantage-of}}}

   "talk"
   {:synsem {:cat :verb
             :sem {:pred :talk
                   :subj {:human true}}}}

   "teach"  {:synsem {:cat :verb
                      :sem {:pred :teach}}
                            :english {:past "taught"
                                      :present {:3sing "teaches"}}}

   "telephone" {:synsem {:cat :verb
                         :sem {:pred :telefonare}}}

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
             :sem {:gender :masc
                   :human true
                   :pred :loro}
             :subcat '()}}

   "they (♀)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :fem
                   :number :plur}
             :sem {:gender :fem
                   :human true
                   :pred :loro}
             :subcat '()}}

   "throw" {:english {:past "threw"}
            :synsem {:cat :verb
                     :sem {:pred :throw}}}

   "throw out"
   {:synsem {:cat :verb
             :sem {:pred :throw-out}}
    :english {:past "threw out"
              :participle "throwing out"}}

   "take"  {:synsem {:cat :verb
                     :sem {:pred :prendere}}
            :english {:past "took"}}

   "understand" {:english {:past "understood"}
                 :synsem {:cat :verb
                          :sem {:pred :understand}}}

   "upload"  {:synsem {:cat :verb
                       :sem {:pred :caricare}}}

   "use"  {:synsem {:cat :verb
                    :sem {:pred :usare}}}

   "wait"  {:synsem {:cat :verb
                     :sem {:pred :wait-for}}}

   "we (♀)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :gender :fem
                   :number :plur}
             :sem {:human true
                   :gender :fem
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
                   :gender :masc
                   :pred :noi}
             :subcat '()}}

   "wear"  {:english {:past "wore"}
            :synsem {:cat :verb
                     :sem {:pred :portare}}}
   
   "win"  {:synsem {:cat :verb
                    :sem {:pred :win
                          :subj {:human true}}}
           :english {:past "won"
                     :participle "winning"}}
   
   "woman"
   (unify agreement-noun
          common-noun
          countable-noun
          {:english {:plur "women"}
           :synsem {:sem {:human true
                          :pred :donna
                          :child false}}})

   "work" [
           {:synsem {:cat :verb
                     :sem {:pred :work-human
                           :subj {:human true}}}
            :english {:note "(human)"}}
           
           {:english {:note "nonliving or machines"} ;; TODO: add support in UI for :note.
            :synsem {:cat :verb
                     :sem {:subj {:living false
                                  :human false ;; should not need to add human=false and animate=false: living=false should suffice.
                                  :animate false}
                           :pred :work-nonhuman}}}]
   
   "write"  {:english {:past "wrote"
                       :past-participle "written"}
             :synsem {:cat :verb
                      :sem {:pred :scrivere}}}
   
   "you (♂)"
   {:note "♂"
    :target :it ;; Italian makes gender distinction for agreement with verbs and adjectives..
    :synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :gender :masc
                   :number :sing}
             :sem {:human true
                   :pred :tu}
             :subcat '()}}

   "you (♀)"
   {:note "♀"
    :target :it ;; Italian makes gender distinction for agreement with verbs and adjectives..
    :synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :gender :fem
                   :number :sing}
             :sem {:human true
                   :pred :tu}
             :subcat '()}}

   "you"
   {:target :es ;; ..but Spanish does not.
    :synsem {:cat :noun
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
             :subcat '()}}})


