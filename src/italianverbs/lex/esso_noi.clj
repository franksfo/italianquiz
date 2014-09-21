(ns italianverbs.lex.esso_noi
  (:require
   [italianverbs.lexiconfn :refer :all]
   [italianverbs.pos :refer :all]))

(def esso-noi
  (list
    {:synsem {:cat cat-of-pronoun
              :pronoun true
              :case disjunctive-case-of-pronoun
              :agr {:person :3rd
                    :number :sing}
              :sem (unify {:human false
                           :place false
                           :pred :esso})
              :subcat '()}
       :english "it"
       :italian {:italian "esso"
                 :cat cat-of-pronoun
                 :case disjunctive-case-of-pronoun}}

      ;; non human masculine and feminine forms
      {:synsem {:cat cat-of-pronoun
                :pronoun true
                :case disjunctive-case-of-pronoun
                :agr {:person :3rd
                      :gender :masc
                      :number :plur}
                :sem {:human false
                      :place false
                      :pred :essi}
                :subcat '()}
       :english "them"
       :italian {:italian "essi"
                 :cat cat-of-pronoun
                 :case disjunctive-case-of-pronoun}}

          ;; fare (to do)
          (unify
           transitive
           fare-common
           {:synsem {:subcat {:2 {:cat :noun}
                              :3 '()}}}
           {:english {:infinitive "to do"
                      :irregular {:past-participle "done"
                                  :past "did"
                                  :present {:1sing "do"
                                            :2sing "do"
                                            :3sing "does"
                                            :1plur "do"
                                            :2plur "do"
                                            :3plur "do"}}}
            :synsem {:cat :verb
                     :infl :infinitive
                     :sem {:pred :fare
                           :example "fare i compiti"
                           :subj {:human true}
                           :obj {:activity true}}}})

          ;; fare (to make)
          (unify
           transitive
           fare-common
           {:synsem {:subcat {:2 {:cat :noun}
                              :3 '()}}}
           {:english {:infinitive "to make"
                      :irregular {:past "made"}}
            :synsem {:cat :verb
                     :essere false
                     :sem {:pred :fare
                           :example "fare il pane"
                           :discrete false
                           :mod nil ;; to avoid matching:
                           ;; (generate (unify s-past {:synsem {:sem {:pred :fare :mod {:pred :bene}}}})).
                           :subj {:human true}
                           :obj {:artifact true}}}})

          ;; fare (to do well to): e.g. "tu hai fatto bene a vendere la casa"
          (let [adverb-semantics (ref {:pred :top})
                subject-semantics (ref {:human true})
                prepositional-semantics (ref {:subj subject-semantics
                                              :mod {:pred :a}})]
            (unify
             verb-subjective
             fare-common
             {:synsem {:subcat {:1 {:sem subject-semantics}
                                :2 {:cat :prep
                                    :sem prepositional-semantics}
                                :3 {:cat :adverb
                                    :sem adverb-semantics}}
                       :cat :verb
                       :infl :infinitive
                       :sem {:pred :fare
                             :example "fare bene a vendere la casa"
                             :mod adverb-semantics
                             :subj subject-semantics
                             :obj prepositional-semantics}}
              :english {:infinitive "to do"
                        :irregular {:past-participle "done"
                                    :past "did"
                                    :present {:1sing "do"
                                              :2sing "do"
                                              :3sing "does"
                                              :1plur "do"
                                              :2plur "do"
                                              :3plur "do"}}}}))

          (unify agreement-noun
                 common-noun
                 countable-noun
                 masculine-noun
                 {:synsem {:sem {:pred :fiore
                                 :activity false
                                 :animate false
                                 :artifact false
                                 :buyable true
                                 :consumable false
                                 :speakable false}}
                  :italian {:italian "fiore"}
                  :english {:english "flower"}}
                 {:synsem {:subcat {:1 {:cat :det}}}})

          ;; non-comparative
          ;; TODO: add comparative
          (unify adjective
                 {:synsem {:sem {:pred :gentile
                                 :comparative false
                                 :human true}} ;; sono gli umani possono essere gentile.
                  :italian {:italian "gentile"}
                  :english {:english "kind"}})

          (unify proper-noun
                 {:synsem {:sem {:pred :giorgio
                                 :human true}
                           :agr {:number :sing
                                 :person :3rd
                                 :gender :masc}}
                  :italian "Giorgio"
                  :english "Giorgio"})


          (unify agreement-noun
                 common-noun
                 countable-noun
                 masculine-noun
                 {:synsem {:sem {:activity false
                                 :animate false
                                 :legible false
                                 :artifact false
                                 :buyable false
                                 :speakable false
                                 :place false
                                 :pred :giorno}}}
                 {:italian {:italian "giorno"}
                  :english {:english "day"}})


          (unify agreement-noun
                 common-noun
                 countable-noun
                 masculine-noun
                 {:synsem {:sem {:legible true
                                 :artifact true
                                 :buyable true
                                 :speakable false
                                 :pred :giornale}}}
                 {:italian {:italian "giornale"}
                  :english {:english "newspaper"}})

          (unify agreement-noun
                 common-noun
                 countable-noun
                 masculine-noun
                 {:synsem {:sem (unify animal {:pred :gatto
                                               :pet true})}
                  :italian {:italian "gatto"}
                  :english {:english "cat"}})

          (unify agreement-noun
                 common-noun
                 countable-noun
                 masculine-noun
                 {:synsem {:sem (unify animal {:pred :granchio
                                               :pet false})} ;; I had hermit crabs as pets..
                  :italian {:italian "granchio"
                            :irregular {:plur "granchi"}}
                  :english {:english "crab"}})

          {:synsem {:cat :det
                    :def :def
                    :gender :masc
                    :number :plur}
           :italian "i"
           :english "the"}

          {:synsem {:cat :det
                    :def :possessive
                    :gender :masc
                    :number :plur
                    :sem {:person :1st
                          :number :sing}}
           :italian "i miei"
           :english "my"}


      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :plur
                :sem {:number :sing
                      :person :2nd}}
       :italian "i tuoi"
       :english "your"}

      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :plur
                :sem {:number :plur
                      :person :2nd}}
       :italian "i vostri"
       :english "your (pl) "}

      (unify sentential-adverb
             {:synsem {:sem {:pred :ieri}
                       :subcat {:1 {:sem {:tense :past
                                          :activity true}}}}
              :italian "ieri"
              :english "yesterday"})

      (unify determiner
             {:synsem {:cat :det
                       :def :def
                       :gender :masc
                       :number :sing}
              :italian "il"
              :english "the"})

      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :sing
                :sem {:number :plur
                      :person :3rd}}
       :italian "il loro"
       :english "their"}

      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :sing
                :sem {:number :plur
                      :person :1st}}
       :italian "il nostro"
       :english "our"}

      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :sing
                :sem {:number :sing
                      :person :1st}}
       :italian "il mio"
       :english "my"}

      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :sing
                :sem {:gender :fem
                      :number :sing
                      :person :3rd}}
       :italian "il suo"
       :english "her"}

      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :sing
                :sem {:gender :masc
                      :number :sing
                      :person :3rd}}
       :italian "il suo"
       :english "his"}

      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :sing
                :sem {:number :sing
                      :person :2nd}}
       :italian "il tuo"
       :english "your"}

      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :sing
                :sem {:number :plur
                      :person :2nd}}
       :italian "il vostro"
       :english "your (pl) "}

      ;; TODO: check that imperfect past: 'we were going mad' works.
      {:synsem {:cat :verb
                :essere true
                :sem {:discrete false
                      :pred :impazzire
                      :subj {:animate true}}}
       :italian {:infinitive "impazzire"
                 :irregular {:present {:1sing "impazzisco"
                                       :2sing "impazzisci"
                                       :3sing "impazzisce"
                                       :1plur "impazziamo"
                                       :2plur "impazzite"
                                       :3plur "impazziscono"}}}
       :english {:infinitive "to go mad"
                 :irregular {:imperfect {:default "was going mad"
                                         :2sing "were going mad"
                                         :plur "were going mad"}
                             :past "went mad"
                             :part-participle "gone mad"
                             :present {:3sing "goes mad"}}}}

      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :1st
                      :number :sing}
                :sem (unify human {:pred :io})
                :subcat '()}
       :english "I"
       :italian "io"}

      (unify sentential-adverb
             {:synsem {:sem {:pred :laltro-ieri}
                       :subcat {:1 {:sem {:tense :past
                                          :activity true}}}}
              :italian "l'altro ieri"
              :english "the day before yesterday"})

      ;; note: no gender: "loro" in either case of masc or fem.
      {:synsem {:cat cat-of-pronoun
                :pronoun true
                :case disjunctive-case-of-pronoun
                :agr {:person :3rd
                      :number :plur}
                :sem (unify human {:pred :lui})
                :subcat '()}
       :english "them"
       :italian {:italian "loro"
                 :cat cat-of-pronoun
                 :case disjunctive-case-of-pronoun}}

      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :3rd
                      :gender :masc
                      :number :sing}
                :sem (unify human {:pred :lui})
                :subcat '()}
       :english "he"
       :italian "lui"}

      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :3rd
                      :gender :masc
                      :number :sing}
                :sem {:human false
                      :pred :lui}
                :subcat '()}
       :english {:english "it"
                 :note "(&#x2642;)"}
       :italian "lui"}

      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :3rd
                      :gender :fem
                      :number :sing}
                :sem (unify human {:pred :lei})
                :subcat '()}
       :english "she"
       :italian "lei"}

      {:english {:english "letter"}
       :italian {:italian "lettera"}
       :synsem {:agr {:gender :fem}
                :cat :noun
                :sem {:legible true
                      :pred :lettera
                      :writable true}}}

      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :3rd
                      :number :plur}
                :sem (unify human {:pred :loro})
                :subcat '()}
       :italian "loro"
       :english "they"}

      {:synsem {:cat :det
                :def :def
                :gender :fem
                :number :sing}
       :italian "la"
       :english "the"}

      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing
                :sem {:number :plur
                      :person :3rd}}
       :italian "la loro"
       :english "their"}

      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing
                :sem {:number :sing
                      :person :1st}}
       :italian "la mia"
       :english "my"}

      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing
                :sem {:number :plur
                      :person :1st}}
       :italian "la nostra"
       :english "our"}

      ;; TODO: add pronominal "la sua (hers)" and "la sua (his)"
      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing
                :sem {:gender :fem
                      :number :sing
                      :person :3rd}}
       :italian "la sua"
       :english "her"}

      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing
                :sem {:gender :masc
                      :number :sing
                      :person :3rd}}
       :italian "la sua"
       :english "his"}

      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing
                :sem {:number :sing
                      :person :2nd}}
       :italian "la tua"
       :english "your"}

      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing
                :sem {:number :plur
                      :person :2nd}}
       :italian "la vostra"
       :english "your (pl)"}

      {:synsem {:cat pronoun-noun
                :pronoun true
                :case pronoun-acc
                :agr {:gender :masc
                      :person :3rd
                      :number :sing}
                :sem (unify human {:pred :lo})
                :subcat '()}
       :english "him"
       :italian {:italian "lo"
                 :initial true
                 :pronoun true
                 :cat noun
                 :case pronoun-acc}}

      {:synsem {:cat pronoun-noun
                :pronoun true
                :case pronoun-acc
                :agr {:gender :masc
                      :person :3rd
                      :number :sing}
                :sem {:human false
                      :place false
                      :pred :lo}
                    :subcat '()}
       :english {:english "it"
                 :note " (&#x2642;)"}
       :italian {:italian "lo"
                 :initial true
                 :pronoun true
                 :cat pronoun-noun
                 :case pronoun-acc}}

      {:synsem {:cat pronoun-noun
                :pronoun true
                :case pronoun-acc
                :agr {:gender :fem
                      :person :3rd
                      :number :sing}
                :sem (unify human {:pred :lei})
                :subcat '()}
       :english "her"
       :italian {:italian "la"
                 :initial true
                 :cat pronoun-noun
                 :case pronoun-acc}}

      {:synsem {:cat pronoun-noun
                :pronoun true
                :case pronoun-acc                      
                :agr {:gender :fem
                      :person :3rd
                          :number :sing}
                :sem {:human false
                      :place false ;; "they go to it (loro vanna a la)" sounds strange
                      :pred :lei}
                :subcat '()}
       :english {:english "it"
                 :note "(&#x2640;)"} ;; unicode female symbol
       :italian {:italian "la"
                 :initial true
                 :cat pronoun-noun
                 :case pronoun-acc}}


      (unify
       intransitive
       {:italian {:infinitive "lavorare"}
        :english {:infinitive "to work"}
        :synsem {:essere false
                 :sem {:subj {:human true
                              :child false}
                       :discrete false
                       :pred :lavorare}}})

      {:synsem {:cat pronoun-noun
                :pronoun true
                :case pronoun-acc
                :agr {:gender :fem
                      :person :3rd
                      :number :plur}
                :sem (unify human {:pred :lei})
                :subcat '()}
       :english {:english "them"
                 :note " (&#x2640;) "}
       :italian {:italian "le"
                 :initial true
                 :cat pronoun-noun
                 :case pronoun-acc}}
      {:synsem {:cat :det
                :def :def
                :gender :fem
                :number :plur}
       :italian "le"
       :english "the"}

      (unify
       transitive
       {:italian {:infinitive "leggere"
                  :irregular {:passato-stem "lett"}}

        :english {:infinitive "to read" ;; spelled "read" but pronounced like "reed".
                  :irregular {:past "read (past)"
                              :note "(past)"}} ;; spelled "read" but pronounced like "red".
        :synsem {:essere false
                 :sem {:pred :leggere
                       :discrete false
                       :subj {:human true}
                   :obj {:legible true}}}})

      {:synsem {:cat pronoun-noun
                :pronoun true
                :case pronoun-acc
                :agr {:gender :masc
                      :person :3rd
                      :number :plur}
                :sem (unify human {:pred :lui})
                :subcat '()}
       :english {:english "them"
                 :note " (&#x2642;) "} ;; unicode male
       :italian {:italian "li"
                 :initial true
                 :cat pronoun-noun
                 :case pronoun-acc}}

      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :plur
                :sem {:number :plur
                      :person :2nd}}
       :italian "le vostre"
       :english "your (pl)"}

      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :plur
                :sem {:number :sing
                      :person :1st}}
       :italian "le mie"
       :english "my"}

      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :plur
                :sem {:number :sing
                      :person :2nd}}
       :italian "le tue"
       :english "your"}

      {:synsem {:cat cat-of-pronoun
                :pronoun true
                :case disjunctive-case-of-pronoun
                :agr {:gender :fem
                      :person :2nd
                      :polite true
                      :number :sing}
                :sem (unify human {:pred :lei})
                :subcat '()}
       :english "her"
       :italian {:italian "lei"
                 :cat cat-of-pronoun
                 :case disjunctive-case-of-pronoun}}

      {:synsem {:cat cat-of-pronoun
                :pronoun true
                :case disjunctive-case-of-pronoun
                :agr {:gender :fem
                      :person :3rd
                      :number :sing}
                :sem (unify human {:pred :lei})
                :subcat '()}
       :english "her"
       :italian {:italian "lei"
                 :cat cat-of-pronoun
                 :case disjunctive-case-of-pronoun}}
 
      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :3rd
                      :gender :fem
                      :number :sing}
                :sem {:human false
                      :pred :lei}
                :subcat '()}
       :english {:english "it"
                 :note "(&#x2640;)"} ;; unicode female
       :italian "lei"}

      (unify agreement-noun
             common-noun
             countable-noun
             masculine-noun
             {:synsem {:sem {:pred :libro
                             :legible true
                             :speakable false
                             :mass false
                             :buyable true
                             :consumable false
                             :artifact true}}
              :italian {:italian "libro"}
              :english {:english "book"}})

      (unify agreement-noun
             common-noun
             countable-noun
             feminine-noun
             {:synsem {:sem human}}
             {:synsem {:sem {:pred :madre
                             :child false}}
              :italian {:italian "madre"}
              :english {:english "mother"}})


      (unify
       transitive
       {:italian {:infinitive "mangiare"}
        :english {:infinitive "to eat"
                  :irregular {:past "ate"}}
        :synsem {:essere false
                 :sem {:pred :mangiare
                       :subj {:animate true}
                       :obj {:edible true}}}})

      (unify agreement-noun
             common-noun
             countable-noun
             masculine-noun
             {:synsem {:sem {:pred :mare
                             :buyable false ;; a seaside's too big to own.
                             :artifact false
                             :city false
                             :place true}}
              :italian {:italian "mare"}
              :english {:english "seaside"}}
             {:synsem {:subcat {:1 {:cat :det
                                    :number :sing
                                    :def :def}}}})


      {:synsem {:cat cat-of-pronoun
                :pronoun true
                :case disjunctive-case-of-pronoun
                :agr {:person :1st
                      :number :sing}
                :sem (unify human {:pred :io})
                :subcat '()}
       :english "me"
       :italian {:italian "me"
                 :pronoun true
                 :cat cat-of-pronoun
                 :case disjunctive-case-of-pronoun}}

      {:synsem {:cat pronoun-noun
                :pronoun true
                :case pronoun-acc
                :agr {:person :1st
                      :number :sing}
                :sem (unify human {:pred :io})
                :subcat '()}
       :english "me"
       :italian {:italian "mi"
                 :initial true
                 :pronoun true
                 :cat pronoun-noun
                 :case pronoun-acc}}

      (unify (:agreement noun)
             (:common noun)
             (:countable noun)
             (:feminine noun)
             {:synsem {:sem {:pred :mela
                             :edible true
                             :animate false
                             :artifact false}}
              :italian {:italian "mela"}
              :english {:english "apple"}})

      (unify (:agreement noun)
             (:common noun)
             (:countable noun)
             (:feminine noun)
             {:synsem {:sem {:pred :melanzana
                             :edible true
                             :animate false
                             :artifact false}}
              :italian {:italian "melanzana"}
              :english {:english "eggplant"}})

      (let [human (ref :top)
            animate (ref :top)
            subj-semantics (ref {:human human
                              :animate animate})
            obj-semantics (ref {:comparative true
                                :human human
                                :animate animate})
            adj-semantics (ref :top)]
        {:synsem {:cat :intensifier
                  :sem {:pred :meno
                        :modifies adj-semantics
                        :arg1 subj-semantics
                        :arg2 obj-semantics}
                  :subcat {:1 {:cat :noun
                               :sem subj-semantics}
                           :2 {:cat :adjective
                               :sem {:arg1 subj-semantics
                                     :pred adj-semantics
                                     :arg2 obj-semantics}}}}
         :italian "meno"
         :english "less"
         })

      {:english "market"
       :italian "mercato"
       :synsem {:agr {:gender :masc}
                :cat :noun
                :sem {:buyable false ;; you can buy things *at* the market, but not the whole market.
                      :place true
                      :pred :mercato}}}

      {:synsem {:cat cat-of-pronoun
                :pronoun true
                :case disjunctive-case-of-pronoun
                :agr {:gender :masc
                      :person :3rd
                      :number :sing}
                :sem (unify human {:pred :lui})
                :subcat '()}
       :english "him"
       :italian {:italian "lui"
                 :cat cat-of-pronoun
                 :case disjunctive-case-of-pronoun}}

      (unify proper-noun
             {:synsem {:sem {:pred :milano
                             :buyable false
                             :artifact false
                             :city true}
                       :agr {:number :sing
                             :person :3rd
                             :gender :masc}}
              :italian {:italian "Milano"}
              :english {:english "Milan"}})

      (unify proper-noun
             {:synsem {:sem {:pred :napoli
                             :buyable false
                             :artifact false
                             ;; getting tired of people "making Napoli":
                                        ;                            :artifact true
                             :city true}
                       :agr {:number :sing
                             :person :3rd
                             :gender :masc}}
              :italian {:italian "Napoli"}
              :english {:english "Naples"}})

      ;; non-comparative
      ;; TODO: add comparative
      (unify adjective
             {:synsem {:cat :adjective
                       :sem {:pred :nero
                             :comparative false
                             :physical-object true
                             :human false}}
              :italian {:italian "nero"
                        :cat :adjective}
              :english {:english "black"
                        :cat :adjective}})

      {:synsem {:cat cat-of-pronoun
                :pronoun true
                :case disjunctive-case-of-pronoun
                :agr {:person :1st
                      :number :plur}
                :sem (unify human {:pred :noi})
                :subcat '()}
       :english "us"
       :italian {:italian "noi"
                 :pronoun true
                 :cat cat-of-pronoun
                 :case disjunctive-case-of-pronoun}}

      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :1st
                      :number :plur}
                :sem (unify human {:pred :noi})
                :subcat '()}
       :english "we"
       :italian "noi"}

      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :1st
                      :number :plur}
                :sem (unify human {:pred :gianni-e-io})
                :subcat '()}
       :english "John and I"
       :italian "Gianni e io"}

      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :1st
                      :number :plur}
                :sem (unify human {:pred :tu-e-io})
                :subcat '()}
       :english "you and I"
       :italian "tu e io"}

      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :1st
                      :number :plur}
                :sem (unify human {:pred :lui-e-io})
                :subcat '()}
       :english "he and I"
       :italian "lui e io"}

      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :1st
                      :number :plur}
                :sem (unify human {:pred :voi-e-io})
                :subcat '()}
       :english "You guys and I"
       :italian "voi e io"}

      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :1st
                      :number :plur}
                :sem (unify human {:pred :lei-tu-e-io})
                :subcat '()}
       :english "She, you and I"
       :italian "lei, tu e io"}

      ;; copied:
      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :2nd
                      :number :plur}
                :sem (unify human {:pred :lei-e-tu})
                :subcat '()}
       :english "she and you"
       :italian "lei e tu"}


      ;; copied:
      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :2nd
                      :number :plur}
                :sem (unify human {:pred :tu-e-maria})
                :subcat '()}
       :english "you and Mary"
       :italian "tu e Maria"}


      ;; copied:
      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :2nd
                      :number :plur}
                :sem (unify human {:pred :tu-e-cane})
                :subcat '()}
       :english "you and the dog"
       :italian "tu e il cane"}

      ;; copied:
      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :3rd
                      :number :plur}
                :sem (unify human {:pred :cane-e-gianni})
                :subcat '()}
       :english "the dog and John"
       :italian "il cane e Gianni"}

      ;; copied:
      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :3rd
                      :number :plur}
                :sem (unify human {:pred :lui-e-lei})
                :subcat '()}
       :english "he and she"
       :italian "lui and lei"}


))

