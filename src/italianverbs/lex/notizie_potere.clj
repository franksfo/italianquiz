(ns italianverbs.lex.notizie_potere
  (:require
   [italianverbs.lexiconfn :refer :all]))

(def notizie-potere
  (list

      (unify agreement-noun
             common-noun
             feminine-noun
             {:synsem {:sem {:pred :notizie
                             :buyable false
                             :legible true}}
              ;; "notizia" would probably also work, since it
              ;; would be pluralized by morphology to "notizie".
              :italian {:italian "notizie"}
              :english {:english "new"}} ;; "news" (will be pluralized by morphology to "news").
             {:synsem {:subcat {:1 {:cat :det
                                    :number :plur
                                    :def :def}}}})

      (unify adjective
             {:italian {:italian "nuovo"
                        :initial true}
              :english {:english "new"}
              :synsem {:cat :adjective
                       :sem {:pred :nuovo
                             :mod {:artifact true}}}})

      {:synsem {:cat :noun
                :pronoun true
                :agr {:case :nom
                      :person :3rd
                      :number :sing}
                :sem (unify human {:pred :ognuno})
                :subcat '()}
       :english "everyone"
       :italian "ognuno"}

      (unify proper-noun
             {:synsem {:sem {:pred :paola
                             :human true}
                       :agr {:number :sing
                             :person :3rd
                             :gender :fem}}
              :italian "Paola"
              :english "Paola"})

      (unify agreement-noun
             common-noun
             countable-noun
             feminine-noun
             {:synsem {:sem {:legible true
                             :buyable false
                             :speakable true
                             :pred :parola}}}
             {:italian {:italian "parola"}
              :english {:english "word"}})

      ;; inherently singular.
      (unify agreement-noun
             common-noun
             masculine-noun
             {:synsem {:sem (sem-impl {:pred :pane
                                       :edible true
                                       :artifact true})}
              :italian {:italian "pane"}
              :english {:english "bread"}}
             {:synsem {:subcat {:1 {:cat :det
                                    :number :sing
                                    :def :def}}}})

      (let [parlare
            (unify
             transitive
             {:italian {:infinitive "parlare"}
              :english {:infinitive "to speak"
                        :irregular {:past "spoke"}}
              :synsem {:essere false
                       :sem {:pred :parlare
                             :subj {:human true}
                             :obj {:speakable true}}}})]
        parlare)

      ;; inherently singular.
      (unify agreement-noun
             common-noun
             feminine-noun
             {:synsem {:sem (sem-impl {:pred :pasta
                                       :edible true
                                       :artifact true})}
              :italian {:italian "pasta"}
              :english {:english "pasta"}}
             {:synsem {:subcat {:1 {:cat :det
                                    :number :sing
                                    :def :def}}}}
           )

      (unify
       intransitive
       {:italian {:infinitive "pensare"}
        :english {:infinitive "to think"
                  :irregular {:past "thought"}}
        :synsem {:essere false
                 :sem {:pred :pensare
                       :discrete false
                       :subj {:human true}}}})

      (unify agreement-noun
             common-noun
             countable-noun
             feminine-noun
             {:synsem {:sem human}}
             {:synsem {:sem {:pred :professoressa
                             :child false}}
              :italian {:italian "professoressa"}
              :english {:english "professor"
                        :note " (&#x2640;)"}}) ;; unicode female symbol

      (unify agreement-noun
             common-noun
             countable-noun
             masculine-noun
             {:synsem {:sem human}}
             {:synsem {:sem {:pred :professore
                             :child false}}}
             {:italian {:italian "professore"}
              :english {:english "professor"
                        :note " (&#x2642;)"}}) ;; unicode male symbol

      (let [complement-semantics (ref {:pred :per})]
        (unify
         {:synsem {:cat :prep
                   :sem complement-semantics
                   :subcat {:1 {:cat :verb
                                :sem complement-semantics
                                :infl :infinitive
                                :subcat {:1 :top
                                         :2 '()}}
                            :2 '()}}
          :italian "per"
          :english ""}))

      ;; perdere
      (unify
       (:transitive verb)
       {:italian {:infinitive "perdere"
                  :irregular {:passato "perso"}}
        :english {:infinitive "to lose"
                  :irregular {:past "lost"
                              :past-participle "lost"}}
        :synsem {:essere false
                 :sem {:pred :perdere
                       :activity false
                       :deliberate false ;; you don't lose things on purpose.
                       :discrete true
                       :subj {:human true}
                       :obj {:animate false}}}}) ;; mostly you don't lose humans or animals, or you do, and it's too sad to put in here.

      ;; non-comparative
      ;; TODO: add comparative
      (unify adjective
             {:synsem {:cat :adjective
                       :sem {:pred :piccolo
                             :comparative false
                             :mod {:physical-object true
                                   :mass false}}}
              :italian {:italian "piccolo"
                        :cat :adjective}
              :english {:english "small"
                        :cat :adjective}})

      ;; "pizza" can be either mass or countable.
      (unify agreement-noun
             common-noun
             feminine-noun
             {:synsem {:sem {:pred :pizza
                             :edible true
                             :artifact true}}
              :italian {:italian "pizza"}
              :english {:english "pizza"}})

      (let [human (ref :top)
            animate (ref :top)]
        {:synsem {:sem {:human human
                        :animate animate}
                  :cat :intensifier
                  :subcat {:1 {:cat :adjective
                               :sem {:comparative true
                                     :human human
                                     :animate animate}}
                           :2 '()}}
         :italian "più"
         :english "more" ;; TODO: should be translated as "-er" (e.g. "richer")
         })

      (let [human (ref :top)
            animate (ref :top)
            subj-semantics (ref {:human human
                                 :animate animate})
            obj-semantics (ref {:comparative true
                                :human human
                                :animate animate})
            adj-semantics (ref :top)]
        {:synsem {:cat :intensifier
                  :sem {:pred :più
                        :modifies adj-semantics
                        :arg1 subj-semantics
                        :arg2 obj-semantics}
                  :subcat {:1 {:cat :noun
                               :sem subj-semantics}
                           :2 {:cat :adjective
                               :sem {:arg1 subj-semantics
                                     :pred adj-semantics
                                     :arg2 obj-semantics}}}}
         :italian "più"
         :english "more"
         })

      (let [pred-of-complement (ref :top)]
        (unify
         verb-subjective
         modal
         {:synsem {:infl {:not :imperfetto}}} ;; disabling imperfetto because it sounds unnatural: "he was being able to.."
         {:italian {:infinitive "potere"
                    :irregular {:present {:1sing "posso"
                                          :2sing "puoi"
                                          :3sing "può"
                                          :1plur "possiamo"
                                          :2plur "potete"
                                          :3plur "possono"}}}
          :english {:infinitive "to be able to"
                    :irregular {:past "could have"
                                ;; TODO: enhance morphology.clj to handle one irregular for all agreements: {:present "can"}.
                                :present {:1sing "can"
                                          :2sing "can"
                                          :3sing "can"
                                          :1plur "can"
                                          :2plur "can"
                                          :3plur "can"}}}
          :synsem {:subcat {:2 {:sem {:pred pred-of-complement}}}
                   :essere false
                   :sem {:pred {:pred pred-of-complement
                                :mod :potere}
                         :activity false
                         :discrete false
                         :subj {:animate true}}}}))

))


