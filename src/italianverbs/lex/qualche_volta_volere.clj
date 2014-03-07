(ns italianverbs.lex.qualche_volta_volere
  (:require
   [italianverbs.lexiconfn :refer :all]
   [italianverbs.pos :refer :all]))

(def qualche_volta-volere
  (list
      {:synsem {:cat :sent-modifier
                :sem {:pred :qualche-volta-past}
                :subcat {:1 {:infl :present
                             :sem {:activity true
                                   :tense :past}
                             :subcat '()}}}
       :italian "qualche volta"
       :english "sometimes"}

      {:synsem {:cat :sent-modifier
                :sem {:pred :qualche-volta-present}
                :subcat {:1 {:infl :present
                             :sem {:activity true
                                   :tense :present}
                             :subcat '()}}}
       :italian "qualche volta"
       :english "sometimes"}

      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :3rd
                      :number :sing}
                :sem {:human false
                      :animate false ;; otherwise we get weird things like "something will see my ladder".
                      :place false ;; otherwise we get "i went to something"
                      :pred :qualcuno}
                :subcat '()}
       :english "something"
       :italian "qualcosa"}

      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :3rd
                      :number :sing}
                :sem (unify human {:pred :qualcuno})
                :subcat '()}
       :english "someone"
       :italian "qualcuno"}

      {:synsem {:cat :det
                :def :partitivo
                :mass false
                :number :sing}
       :italian "qualche"
       :english "some"}

;      {:italian "quando"
;       :english "when"
;       :synsem {:cat :temporal-glue
;                :sem {:pred :quando}
;                :subcat {:1 {:cat :verb
;                             :infl :imperfetto
;                             :sem {:activity true}
;                             :subcat '()}
;                         :2 {:cat :verb
;                             :infl :present
;                             :sem {:aspect :passato
;                                   :discrete true
;                                   :tense :past}
;                             :subcat '()}}}}

      {:synsem {:cat :det
                :def :demonstrativo
                :gender :fem
                :number :sing}
       :italian "quella"
       :english "that"}

      {:synsem {:cat :det
                :def :demonstrativo
                :gender :fem
                :number :plur}
       :italian "quelle"
       :english "those"}

      {:synsem {:cat :det
                :def :demonstrativo
                :gender :masc
                :number :plur}
       :italian "quelli"
       :english "those"}

      {:synsem {:cat :det
                :def :demonstrativo
                :gender :masc
                :number :sing}
       :italian "quello"
       :english "that"}

      {:synsem {:cat :det
                :def :demonstrativo
                :gender :fem
                :number :sing}
       :italian "questa"
       :english "this"}

      {:synsem {:cat :det
                :def :demonstrativo
                :gender :fem
                :number :plur}
       :italian "queste"
       :english "these"}

      {:synsem {:cat :det
                :def :demonstrativo
                :gender :masc
                :number :plur}
       :italian "questi"
       :english "these"}

      {:synsem {:cat :det
                :def :demonstrativo
                :gender :masc
                :number :sing}
       :italian "questo"
       :english "this"}

      ;; non-comparative
      (unify adjective
             {:synsem {:sem {:pred :ricco
                             :comparative false
                             :mod {:human true}}} ;; TODO between with comparative/non-comparative rather than duplicating.
              :italian {:italian "ricco"}
              :english {:english "rich"}})

      ;; comparative:
      (let [complement-complement-sem (ref {:human true}) ;; only humans can be rich.
            complement-sem (ref {:pred :di
                                 :mod complement-complement-sem})
            subject-sem (ref {:human true})] ;; only humans can be rich.
        (unify adjective
               comparative
               {:synsem {:sem {:pred :ricco
                               :arg1 subject-sem
                               :arg2 complement-complement-sem}
                         :subcat {:1 {:cat :noun
                                      :sem subject-sem}
                                  :2 {:cat :prep
                                      :sem complement-sem}}}
                :italian {:italian "ricco"}
                :english {:english "rich"}}))

      (unify
       {:italian {:infinitive "ridere"
                  :irregular {:passato "riso"}}
        :english {:infinitive "to laugh"
                  :irregular {:past "laughed"}}
        :synsem {:cat :verb
                 :essere false
                 :sem {:subj {:human true}
                       :pred :ridere
                       :discrete true
                       }}})

      (unify proper-noun
             {:synsem {:sem {:pred :roma
                             :buyable false
                             ;; getting tired of people "making Rome" sentences.
                             :actifact false

                                        ;                            :artifact true ;; but wasn't built in a day..
                             :city true}
                       :agr {:number :sing
                             :person :3rd
                             :gender :masc}}
              :italian {:italian "Roma"}
              :english {:english "Rome"}})

      (unify adjective
             {:synsem {:cat :adjective
                       :sem {:pred :rosso
                             :comparative false
                             :mod {:physical-object true
                                   :human false}}}
              :italian {:italian "rosso"}
              :english {:english "red"}})

      ;; non-comparative
      (unify adjective
             {:synsem {:cat :adjective
                       :sem {:pred :robusto
                             :comparative false
                             :activity false
                             :mod {:animate true}}}
              :italian {:italian "robusto"
                        :cat :adjective}
              :english {:english "large-built"
                        :cat :adjective}})

      (unify adjective
             {:synsem {:cat :adjective
                       :sem {:pred :rumorosa
                             :comparative false
                           :mod {:animate true}}}
              :italian {:italian "rumoroso"
                        :cat :adjective}
              :english {:english "noisy"
                        :cat :adjective}})

      ;; comparative:
      (let [complement-complement-sem (ref {:human true}) ;; only animals can be noisy.
            complement-sem (ref {:pred :di
                                 :mod complement-complement-sem})
            subject-sem (ref {:animate true})] ;; only animals can be noisy.
        (unify adjective
               comparative
               {:synsem {:sem {:pred :semplice
                               :arg1 subject-sem
                               :arg2 complement-complement-sem}
                         :subcat {:1 {:cat :noun
                                      :sem subject-sem}
                                  :2 {:cat :prep
                                      :sem complement-sem}}}
                :italian {:italian "rumoroso"}
                :english {:english "noisy"}}))

      (unify agreement-noun
             common-noun
             countable-noun
             masculine-noun
             {:synsem {:sem human}}
             {:synsem {:sem {:pred :ragazzo
                             :child false}}
              :italian {:italian "ragazzo"}
              :english {:english "guy"}})

;      (unify agreement-noun
;             common-noun
;             countable-noun
;             masculine-noun
;             {:synsem {:sem human}}
;             {:synsem {:sem {:pred :ragazzino
;                             :child true}}
;              :italian {:italian "ragazzino"}
;              :english {:english "little boy"}})

;      (unify agreement-noun
;             common-noun
;             countable-noun
;             feminine-noun
;             {:synsem {:sem human}}
;             {:synsem {:sem {:pred :ragazzina
;                             :child true}}
;              :italian {:italian "ragazzina"}
;              :english {:english "little girl"}})

      (unify agreement-noun
             common-noun
             countable-noun
             feminine-noun
            {:synsem {:sem human}}
            {:synsem {:sem {:pred :ragazza
                            :child false}}
             :italian {:italian "ragazza"}
             :english {:english "girl"}})

      (unify
       transitive
       {:italian {:infinitive "ricordare"}
        :english {:infinitive "to remember"
                  :irregular {:past-participle "remembered"}}
        :synsem {:essere false
                 :sem {:subj {:human true}
                       :obj {:animate true}
                       :pred :ricordare}}})

      (unify
       transitive
       {:italian {:infinitive "ricordare"}
        :english {:infinitive "to remember"
                  :irregular {:past-participle "remembered"}}
        :synsem {:essere false
                 :sem {:subj {:human true}
                       :obj {:legible true}
                       :pred :ricordare}}})

      (unify
       transitive
       {:italian {:infinitive "scrivere"}
        :english {:infinitive "to write"
                  :irregular {:past "wrote"}}
        :synsem {:essere false
                 :sem {:pred :scrivere
                       :subj {:human true}
                       :obj {:legible true
                             :writable true}}}})

      (unify
       transitive
       {:italian {:infinitive "seguire"}
        :english {:infinitive "to follow"}
        :synsem {:essere false
                 :sem {:pred :seguire
                       :subj {:animate true}
                       :obj {:animate true}}}})


      ;; comparative:
      (let [complement-complement-sem (ref {:human true}) ;; only humans can be naive.
            complement-sem (ref {:pred :di
                                 :mod complement-complement-sem})
            subject-sem (ref {:human true})] ;; only humans can be naive.
        (unify adjective
               comparative
               {:synsem {:sem {:pred :semplice
                               :arg1 subject-sem
                               :arg2 complement-complement-sem}
                         :subcat {:1 {:cat :noun
                                      :sem subject-sem}
                                  :2 {:cat :prep
                                      :sem complement-sem}}}
                :italian {:italian "semplice"}
                :english {:english "naive"}}))

      ;; non-comparative:
      (unify adjective
             {:synsem {:cat :adjective
                       :sem {:pred :semplice
                             :comparative false
                             :mod {:human true}}}
              :italian {:italian "semplice"
                        :cat :adjective}
              :english {:english "naive"
                        :cat :adjective}})


      (unify (:agreement noun)
             (:common noun)
             (:countable noun)
             (:feminine noun)
             {:synsem {:sem {:pred :spiaggia
                             :buyable false
                             :artifact false
                             :city false
                             :place true}}
              :italian {:italian "spiaggia"}
              :english {:english "beach"}})

      ;; stamattina (this morning) could be either
      ;; past or present.
      (unify {:italian "stamattina"
              :synsem {:cat :sent-modifier
                       :sem {:pred :stamattina}
                       :subcat {:1 {:subcat '()
                                    :infl :futuro
                                    :sem {:tense :future
                                          :activity true}}}}
              :english "this morning"})

      (unify {:italian "stamattina"
              :synsem {:cat :sent-modifier
                       :sem {:pred :stamattina}
                       :subcat {:1 {:subcat '()
                                    :sem {:tense :past  ;; could be passato prossimo (avere/essere + verb) or imperfetto
                                          :activity true}}}}
              :english "this morning"})

      {:english {:infinitive "to hang out"
                 :irregular {:past "hung out"
                             :present {:3sing "hangs out"}}}
       :italian "stendere"
       :synsem {:aux false
                :cat :verb
                :modal false
                :sem {:pred :stendere
                      :subj {:human true}
                      :obj {:pred :bucato}}
                :subcat {:2 {:cat :noun}}}}

      (unify (:agreement noun)
             (:common noun)
             (:countable noun)
             (:feminine noun)
             {:synsem {:sem {:pred :strada
                             :buyable false ;; a street's too big/valuable to own.
                             :artifact true
                             :city false
                             :place true}}
              :italian {:italian "strada"}
              :english {:english "street"}})

      ;; stradale
      (unify (:agreement noun)
             (:common noun)
             (:countable noun)
             (:masculine noun)
             {:synsem {:sem {:pred :stradale
                             :buyable false ;; a road's too big/valuable to own.
                             :artifact true
                             :city false
                             :place true}}
              :italian {:italian "stradale"}
              :english {:english "road"}})


      (unify agreement-noun
             common-noun
             feminine-noun
             countable-noun
             {:synsem {:sem {:artifact true
                             :consumable false
                             :legible false
                             :speakable false
                             :pred :scala}}
              :italian {:italian "scala"}
              :english {:english "ladder"}})

      (unify agreement-noun
             common-noun
             masculine-noun
             countable-noun
             {:synsem {:sem {:pred :sciopero
                             :human false ;; should not need this: material=false.
                             :buyable false ;; should not need this: material=false.
                             :drinkable false ;; should not need this: material=false.
                             :edible false  ;; should not need this: material=false.
                             :legible false
                             :artifact false
                             :material false
                             :political true
                             :animate false;; should not need this: material=false.
                             :speakable false;; should not need this: material=false.
                             }}}
             {:italian {:italian "sciopero"}
              :english {:english "strike"}})

      (unify agreement-noun
             common-noun
             feminine-noun
             countable-noun
             {:synsem {:sem {:furniture true
                             :pred :sedia}}
              :italian {:italian "sedia"}
              :english {:english "chair"}})

      (unify
       (:transitive verb)
       {:italian {:infinitive "sostenere"
                  :irregular {:passato "sostenuto"
                              :present {:1sing "sostengo"
                                        :2sing "sostengi"
                                        :3sing "sostenga"
                                        :1plur "sosteniamo"
                                        :2plur "sostenete"
                                        :3plur "sostengono"}}}
        :english {:infinitive "to support"}

        :synsem {:essere false
                 :sem {:pred :sostenere
                       :activity true
                       :discrete false
                       :subj {:human true}
                       :obj {:sem {:human true
                                   :political true}}}}})

      (unify
       {:italian {:infinitive "sognare"}
        :english {:infinitive "to dream"
                  :irregular {:past "dreamt"}}
        :synsem {:cat :verb
                 :essere false
                 :sem {:subj {:animate true}
                       :discrete false
                       :pred :sognare}}})

      (unify agreement-noun
             common-noun
             masculine-noun
             countable-noun
             {:synsem {:agr {:number :plur}
                       :sem {:human true
                             :pred :suoceri}}
              :italian {:italian "suoceri"}
              :english {:english "parent-in-law" ;; note that this form is never used
                        ;; because the agreement is fixed as plural, so only the
                        ;; following irregular plural form will be used.
                        :irregular {:plur "parents-in-law"}}})



      (unify agreement-noun
             common-noun
             feminine-noun
             countable-noun
             {:synsem {:sem {:artifact true
                             :consumable true
                             :legible true
                             :speakable true
                             :pred :stravaganza}}
              :italian {:italian "stravaganza"}
              :english {:english "extravagant thing"}})

      (unify agreement-noun
             common-noun
             countable-noun
             masculine-noun
             {:synsem {:sem human}}
             {:synsem {:sem {:pred :studente
                             :child false}}} ;; could be true, but not always. TODO: create separate sense for child=true.
             {:italian {:italian "studente"}
              :english {:english "student"}})

      (unify agreement-noun
             common-noun
             masculine-noun
             countable-noun
             {:synsem {:sem {:furniture true
                             :pred :tavolo}}
              :italian {:italian "tavolo"}
              :english {:english "table"}})

      {:synsem {:cat cat-of-pronoun
                :pronoun true
                :case disjunctive-case-of-pronoun
                :agr {:person :2nd
                      :polite false
                      :number :sing}
                :sem (unify human {:pred :tu})
                :subcat '()}
       :english "you"
       :italian {:italian "te"
                 :cat cat-of-pronoun
                 :case disjunctive-case-of-pronoun}}

      {:synsem {:cat pronoun-noun
                :pronoun true
                :case pronoun-acc
                :agr {:person :2nd
                      :number :sing}
                :sem (unify human {:pred :tu})
                :subcat '()}
       :english "you"
       :italian {:italian "ti"
                 :initial true
                 :cat pronoun-noun
                 :case pronoun-acc}}

      (unify agreement-noun
             common-noun
             feminine-noun
             countable-noun
             {:synsem {:sem {:furniture true
                             :pred :tovaglia}}
              :italian {:italian "tovaglia"}
              :english {:english "tablecloth"
                        ;; regular morphology would give us "tableclothes", so:
                        :irregular {:plur "tablecloths"}}})

      ;; something's wrong with conjugation of this verb.
                                        ;(def telefonare
                                        ;  (unify
                                        ;   transitive
                                        ;   {:italian {:italian "telefonare"}
                                        ;    :english {:english "to call"}
                                        ;    :synsem {:essere false
                                        ;             :sem {:pred :telefonare
                                        ;                   :subj {:human true}
                                        ;                   :obj {:human true}}}}))



      {:synsem {:cat :sent-modifier
                :sem {:pred :tre-giorni-fa}
                :subcat {:1 {:subcat '()
                             :sem {:tense :past
                                   :activity true}}}}
       :italian "tre giorni fa"
       :english "three days ago"}

      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:gender :fem
                      :person :2nd
                      :number :sing}
                :sem (unify human {:pred :tu})
                :subcat '()}
       :english {:english "you"
                 :note " (&#x2640;)"} ;; unicode female symbol
       :italian "tu"}

      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:gender :masc
                      :person :2nd
                      :number :sing}
                :sem (unify human {:pred :tu})
                :subcat '()}
       :english {:english "you"
                 :note " (&#x2642;)"} ;; unicode male symbol
       :italian "tu"}

      {:synsem {:cat :det
                :def :indef
                :mass false
                :gender :masc
                :number :sing}
       :italian "un"
       :english "a"}

      {:synsem {:cat :det
                :def :indef
                :mass false
                :gender :fem
                :number :sing}
       :italian "una"
       :english "a"}

      (unify agreement-noun
             common-noun
             countable-noun
             masculine-noun
             {:synsem {:sem human}}
             {:synsem {:sem {:pred :uomo
                             :child false}}
              :italian {:irregular {:plur "uomini"}
                        :italian "uomo"}
              :english {:irregular {:plur "men"}
                        :english "man"}})

      (unify
       transitive
       {:italian {:infinitive "vedere"
                  :irregular {:passato "visto"}}
        :english {:infinitive "to see"
                  :irregular {:past "saw"
                              :past-participle "seen"}}
        :synsem {:essere false
                 :sem {:pred :vedere
                       :activity false ;; "seeing" is not a continuous act but rather an instantaneous one.
                       ;; "watching" is the continuous counterpart of "seeing"
                       ;; TODO: check against Italian usage
                       :subj {:animate true}
                       :obj {:visible true}}}})

    (unify
     transitive
     {:italian {:infinitive "vendere"}
      :english {:infinitive "to sell"
                :irregular {:past "sold"}}
      :synsem {:essere false
               :sem {:pred :vendere
                     :subj {:human true}
                     :obj {:buyable true}}}})

    (unify
     venire-common
     {:synsem {:cat :verb
               :essere true
               :sem {:pred :venire
                     :activity true
                     :motion true
                     :subj {:animate true}}}})

    ;; TODO: add venire which takes PP with "a" (e.g. he comes to Venice)

    (unify
     transitive-but-with-prepositional-phrase-instead-of-noun
     venire-common
     (let [complement-subject (ref {:animate true})
           complement-semantics (ref {:subj complement-subject
                                      :activity true
                                      :motion false ;; otherwise we get weird things like "he came to go".
                                      :deliberate true ;;;; you come somewhere to do something that you intended to do, not something that you do accidentally.
                                      :pred :per})] ;; takes 'per' as proposition.
       {:synsem {:essere true
                 :sem {:pred :venire
                       :subj complement-subject
                       :obj complement-semantics}
                 :subcat {:2 {:sem complement-semantics}}}}))

    {:synsem {:cat pronoun-noun
              :pronoun true
              :case pronoun-acc
              :agr {:person :2nd
                    :number :plur}
              :sem (unify human {:pred :voi})
              :subcat '()}
     :english "you all"
     :italian {:italian "vi"
               :initial true
               :cat pronoun-noun
               :case pronoun-acc}}

    (unify
     {:italian {:infinitive "vivere"}
      :english {:infinitive "to live"
                :irregular {:past "lived"}}
      :synsem {:cat verb
               :essere true
               :sem {:pred :vivere
                     :deliberate false
                     ;; TODO: activity=false for now, but other senses of 'vivere' could be activities, e.g.
                     ;; adding PP (e.g. "vivevano in Milano (was living in Milano)")
                     :activity false
                     :discrete false
                     :subj {:animate true}}}}) ;; TODO: change to living-thing: (e.g. plants are living but not animate)

      (unify drinkable-noun
             agreement-noun
             masculine-noun
             {:italian {:italian "vino"}
              :english {:english "wine"}
              :synsem {:sem {:pred :vino
                             :artifact true}}})
      {:synsem {:cat cat-of-pronoun
                :pronoun true
                :case disjunctive-case-of-pronoun
                :agr {:person :2nd
                      :number :plur}
                :sem (unify human {:pred :voi})
                :subcat '()}
       :english "you all"
       :italian {:italian "voi"
                 :cat cat-of-pronoun
                 :case disjunctive-case-of-pronoun}}


      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :2nd
                      :number :plur}
                :sem (unify human {:pred :voi})
                :subcat '()}
       :italian "voi"
       :english "you all"}

      (unify
       verb-subjective
       modal
       {:italian {:infinitive "volere"
                  :irregular {:present {:1sing "voglio"
                                        :2sing "vuoi"
                                        :3sing "vuole"
                                        :1plur "vogliamo"
                                        :2plur "volete"
                                        :3plur "vogliono"}
                              :past "voluto"}}
        :english {:infinitive "to want to"
                  :irregular {:present {:1sing "want to"
                                        :2sing "want to"
                                        :3sing "wants to"
                                        :1plur "want to"
                                        :2plur "want to"
                                        :3plur "want to"}
                              :past "wanted to"}}
        :synsem {:essere false
                 :sem {:pred :volere
                       :activity true
                       :discrete false
                       :subj {:animate true}}}})
))

