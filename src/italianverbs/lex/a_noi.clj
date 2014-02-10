(ns italianverbs.lex.a_noi
  (:refer-clojure :exclude [get-in merge resolve find])
  (:require
   [clojure.set :refer (union)]
   [clojure.tools.logging :as log]
   [italianverbs.lexiconfn :refer :all]
   [italianverbs.morphology :refer :all] ;; for debugging
   [italianverbs.unify :refer (fail? serialize)]))

(def a-noi
  (list

   (let [location (ref {:place true})]
     {:synsem {:cat :prep
               :sem {:pred :a
                     :obj location
                     :comparative false}
               :subcat {:1 {:cat :noun
                            :subcat '()
                            :sem location}
                        :2 '()}}
      :italian {:initial true
                :italian "a"}
      :english "to"})


   {:synsem {:cat :prep
             :sem {:pred :in}
             :subcat {:1 {:cat :noun
                          :sem {:city true}}}}
    ;; this overrides the prep-phrase's extends, which are too general
    ;; for this lexical entry "a"/"in".
    :extend {:prep-phrase {:a {:head :prepositions
                               :comp :proper-nouns}}}
    :italian "a"
    :english "in"}

   ;; e.g. "a ridere": tu hai fatto bene a ridere (you did well to laugh)"
   (let [complement-semantics (ref {:pred :a
                                    :mod {:pred :a}})]
     {:synsem {:cat :prep
               :sem complement-semantics
               :subcat {:1 {:cat :verb
                            :sem complement-semantics
                            :infl :infinitive
                            :subcat {:1 :top
                                     :2 '()}}
                        :2 '()}}
      :italian "a"
      :english ""})

     (unify (:agreement noun)
            (:drinkable noun)
            (:feminine noun)
            {:italian {:italian "acqua"}
             :english {:english "water"}
             :synsem {:sem {:artifact false
                            :animate false
                            :pred :acqua}}})

     (let [is-place (ref {:place true}) ;; only places can be crowded.
           than-this (ref {:pred :di
                           :mod is-place})]
       (unify adjective
              comparative
              {:synsem {:sem {:pred :affolato
                              :arg1 is-place
                              :arg2 is-place}
                        :subcat {:1 {:cat :noun
                                     :sem is-place}
                                 :2 {:cat :prep
                                     :sem than-this}}}
               :italian {:italian "affolato"
                         :initial true}
               :english "crowded"}))

     ;; non-comparative
     (unify adjective
            subcat0
            {:synsem {:cat :adjective
                      :sem {:pred :affolato
                            :comparative false
                            :mod {:place true}}} ;; only places can be crowded.
             :italian {:italian "affolato"
                       :cat :adjective}
             :english {:english "crowded"
                       :cat :adjective}})

      (unify
       transitive
       {:italian {:infinitive "aiutare"}
        :english {:infinitive "to help"}
        :synsem {:essere false
                 :sem {:pred :aiutare
                       :activity true
                       :subj {:human true}
                       :obj {:human true}}}})

     ;; non-comparative:
     (let [subject-sem (ref {:human true}) ;; only humans can be tall.
           subject-agr (ref :top)
           ] 
       (unify (unify adjective
                     non-comparative-adjective
                     {:synsem {:cat :adjective
                               :sem {:pred :alto
                                     :comparative false
                                     :arg1 subject-sem
                                     :mod {:human true}}
                               :subcat {:1 {:cat :noun
                                            :agr subject-agr
                                            :sem subject-sem}
                                        :2 '()}}
                      :italian {:italian "alto"
                                :agr subject-agr}
                      :english {:english "tall"}})))

     ;; comparative:
     (let [complement-complement-sem (ref {:human true}) ;; only humans can be tall.
           complement-sem (ref {:pred :di
                                :mod complement-complement-sem})
           subject-sem (ref {:human true})] ;; only humans can be tall.
       (unify adjective
              comparative
              {:synsem {:sem {:pred :alto
                              :arg1 subject-sem
                              :arg2 complement-complement-sem}
                        :subcat {:1 {:cat :noun
                                     :sem subject-sem}
                                 :2 {:cat :prep
                                     :sem complement-sem}}}
               :italian {:italian "alto"}
               :english {:english "tall"}}))


     (unify agreement-noun
            common-noun
            countable-noun
            masculine-noun
            {:synsem {:sem {:pred :amico
                            :human true
                            :child false}}
             :italian {:italian "amico"}
             :english {:english "friend"}})

      (unify
      transitive
      {:italian {:infinitive "amare"}
       :english {:infinitive "to love"
                 :irregular {:past "loved"}}
       :synsem {:essere false
                :sem {:pred :amare
                      :activity false
                      :discrete false
                      :subj {:human true}
                      :obj {:animate true}}}})

     ;; "andare"-intransitive
     (unify
      intransitive
      andare-common)

     ;; "andare" that takes a prepositional phrase
     (unify
      verb-subjective
      andare-common
      (let [place-sem (ref {:place true
                            :pred :a})]
        {:synsem {:sem {:location place-sem}
                  :subcat {:2 {:sem place-sem
                               :subcat '()
                               :cat :prep}}}})
      {:note "andare-pp"})

     (unify proper-noun
            {:synsem {:sem {:pred :antonio
                            :human true}
                      :agr {:number :sing
                            :person :3rd
                            :gender :masc}}
             :italian "Antonio"
             :english "Antonio"})

     (unify proper-noun
            {:synsem {:sem {:pred :antonia
                            :human true}
                      :agr {:number :sing
                            :person :3rd
                            :gender :fem}}
             :italian "Antonia"
             :english "Antonia"})

     ;; "avere": to possess something buyable
     (unify
      transitive
      avere-common
      {:synsem {:sem {:pred :avere
                      :activity false
                      :discrete false
                      :subj {:human true}
                      :obj {:buyable true}}}})

     ;; "avere": auxiliary-verb: takes intransitive verb (1 arg)
     (unify
      verb-aux-type
      verb-subjective
      avere-common
      {:synsem {:infl :present
                :subcat {:2 {:essere false}}}
       :english {:hidden true}})

     ;; non-comparative
     (unify adjective
            subcat0
            {:synsem {:sem {:pred :bello
                            :comparative false
                            :mod :top}} ;; for now, no restrictions on what can be beautiful.
             :italian {:italian "bello"}
             :english {:english "beautiful"}})

     (let [complement-complement-sem (ref :top) ;; for now no restrictions
           complement-sem (ref {:pred :di
                                :mod complement-complement-sem})
           subject-sem (ref :top)] ;; subject can be anything.
       (unify adjective
              comparative
              {:synsem {:sem {:pred :bello
                              :arg1 subject-sem
                              :arg2 complement-complement-sem}
                        :subcat {:1 {:cat :noun
                                     :sem subject-sem}
                                 :2 {:cat :prep
                                     :sem complement-sem}}}
               :italian {:italian "bello"}
               :english {:english "beautiful"}}))

     {:synsem {:cat :adverb
               :sem {:pred :bene}}
      :italian {:italian "bene"}
      :english {:english "well"}}

     ;; bere
     (unify
      (:transitive verb)
      {:italian {:infinitive "bere"
                 :irregular {:passato "bevuto"
                             :imperfetto {:1sing "bevevo"
                                          :2sing "bevevi"
                                          :3sing "beveva"
                                          :1plur "bevevamo"
                                          :2plur "bevevate"
                                          :3plur "bevevano"}
                             :present {:1sing "bevo"
                                       :2sing "bevi"
                                       :3sing "beve"
                                       :1plur "beviamo"
                                       :2plur "bevete"
                                       :3plur "bevano"}}}
       :english {:infinitive "to drink"
                 :irregular {:past "drank"}}
       :synsem {:essere false
                :sem {:pred :bere
                      :subj {:animate true}
                      :obj {:drinkable true}}}})

     ;; non-comparative
     ;; TODO: add comparative
     (unify adjective
            {:synsem {:cat :adjective
                      :sem {:pred :bianco
                            :comparative false
                            :mod {:physical-object true
                                  :human false}}}
             :italian {:italian "bianco"
                       :irregular {:masc {:plur "bianchi"}
                                   :fem {:plur "bianche"}}
                       :cat :adjective}
             :english {:english "white"
                       :cat :adjective}})

     (unify agreement-noun
            drinkable-noun
            feminine-noun
            {:italian {:italian "birra"}
             :english {:english "beer"}
             :synsem {:sem {:pred :birra
                            :artifact true}}})


     (unify agreement-noun
            common-noun
            countable-noun
            masculine-noun
            {:synsem {:sem {:pred :braccio
                            :part-of-human-body true}}
             ;; adding "bracci" as irregular because
             ;; current morphology.clj would otherwise return
             ;; "braccii".
             ;; TODO: might not be an exception so much
             ;; as a ortho-pholological rule "io" -plur-> "i"
             :italian {:italian "braccio"
                       :irregular {:plur "bracci"}}
             :english {:english "arm"}})

     ;; non-comparative
     ;; TODO: add comparative
     (unify adjective
            {:synsem {:cat :adjective
                      :sem {:pred :brutto
                            :comparative false
                            :mod :top}} ;; for now, no restrictions on what can be ugly.
             :italian {:italian "brutto"
                       :cat :adjective}
             :english {:english "ugly"
                       :cat :adjective}})

     ;; inherently plural
     (unify agreement-noun
           common-noun
           countable-noun
           masculine-noun
           {:synsem {:sem {:pred :calzoni
                           :artifact true
                           :speakable false
                           :legible false
                           :consumable false
                           :clothing true}}}
           {:italian {:italian "calzoni"}
            :english {:english "trouser"}}
           {:synsem {:subcat {:1 {:cat :det
                                  :number :plur
                                  :def :def}}}})

    (unify agreement-noun
           common-noun
           countable-noun
           feminine-noun
           {:synsem {:sem {:pred :camicia
                           :artifact true
                           :speakable false
                           ;; (although an exception would be tshirts with writing on them):
                           :legible false
                           :consumable false
                           :clothing true}}}
           {:italian {:italian "camicia"}
            :english {:english "shirt"}})

    (unify agreement-noun
           common-noun
           countable-noun
           masculine-noun
           {:synsem {:sem (unify animal {:pred :cane :pet true})}
            :italian {:italian "cane"}
            :english {:english "dog"}})

    (unify agreement-noun
           common-noun
           countable-noun
           feminine-noun
           {:synsem {:sem {:pred :casa
                           :activity false ;; should not need this probably: should be inferrable from  :place==true or perhaps :artifact==true.
                           :buyable true
                           :artifact true
                           :place true}}
            :italian {:italian "casa"}
            :english {:english "house"}})

     (unify adjective
            {:synsem {:cat :adjective
                      :sem {:pred :cattivo
                            :comparative false
                            :mod {:human true;; TODO:should not need this because child => human.
                                  :child true}}}
             :italian {:italian "cattivo"
                       :cat :adjective}
             :english {:english "naughty"
                       :cat :adjective}})


    {:synsem {:cat pronoun-noun
              :pronoun true
              :agr {:case pronoun-acc
                    :person :1st
                    :number :plur}
              :sem (unify human {:pred :noi})
                    :subcat '()}
     :english "us"
     :italian {:italian "ci"
               :initial true
               :cat pronoun-noun
               :case pronoun-acc}}

    (unify agreement-noun
           common-noun
           feminine-noun
           {:synsem {:sem {:pred :cipolla
                           :edible true
                           :animate false
                           :artifact false}}
            :italian {:italian "cipolla"}
            :english {:english "onion"}})

      ;; cercare
      (unify
       (:transitive verb)
       {:italian {:infinitive "cercare"}
        :english {:infinitive "to look for"
                  :irregular {:past "looked for"
                              :imperfetto-suffix "looking for"
                              :past-participle "looked for"
                              :present {:1sing "look for"
                                        :2sing "look for"
                                        :3sing "looks for"
                                        :1plur "look for"
                                        :2plur "look for"
                                        :3plur "look for"}}}
        :synsem {:essere false
                 :sem {:pred :cercare
                       :activity true
                       :discrete false
                       :subj {:human true}
                       :obj {:physical-object true}}}})

    (unify agreement-noun
           common-noun
           countable-noun
           feminine-noun
           {:synsem {:sem {:pred :città
                           :buyable false  ;; can't buy a city (unless you're a billionaire like Mike Bloomberg)
                           :artifact true ;;
                           :city true}}
            :italian {:italian "città"}
            :english {:english "city"}}
           {:synsem {:subcat {:1 {:cat :det
                                  :def :def}}}})

    (unify agreement-noun
           common-noun
           countable-noun
           masculine-noun
           {:synsem {:sem {:pred :compito
                           :legible true
                           :speakable false
                           :buyable false
                           :artifact true
                           :activity true}}
            :italian {:italian "compito"}
            :english {:english "homework assignment"}})

    (unify
     transitive
     {:italian {:infinitive "comprare"}
      :english {:infinitive "to buy"
                :irregular {:past "bought"}}
      :synsem {:essere false
               :sem {:pred :comprare
                     :subj {:human true}
                     :obj {:buyable true}}}})

    (let [complement-complement-sem (ref {:human true})
           complement-sem (ref {:pred :di
                                :mod complement-complement-sem})
           subject-sem (ref {:place true})]
       (unify adjective
              comparative
              {:synsem {:sem {:pred :contento
                              :arg1 subject-sem
                              :arg2 complement-complement-sem}
                        :subcat {:1 {:cat :noun
                                     :sem subject-sem}
                                 :2 {:cat :prep
                                     :sem complement-sem}}}
               :italian "contento"
               :english "happy"}))

     ;; non-comparative
     (unify adjective
            {:synsem {:cat :adjective
                      :sem {:pred :contento
                            :comparative false
                            :mod {:human true}}}
             :italian {:italian "contento"
                       :cat :adjective}
             :english {:english "happy"
                       :cat :adjective}})

    (let [complement-complement-sem (ref {:human true}) ;; only humans can be short.
          complement-sem (ref {:pred :di
                               :mod complement-complement-sem})
          subject-sem (ref {:human true})] ;; only humans can be short.
      (unify adjective
             comparative
             {:synsem {:sem {:pred :corto
                             :arg1 subject-sem
                             :arg2 complement-complement-sem}
                       :subcat {:1 {:cat :noun
                                    :sem subject-sem}
                                :2 {:cat :prep
                                    :sem complement-sem}}}
              :italian {:italian "corto"}
              :english {:english "short"}}))

    ;; non-comparative
    (unify adjective
           {:synsem {:cat :adjective
                     :sem {:pred :corto
                           :comparative false
                           :mod {:human true}}}
            :italian {:italian "corto"
                      :cat :adjective}
            :english {:english "short"
                      :cat :adjective}})

    (unify
     transitive
     {:italian {:infinitive "deludere"
                :irregular {:passato "deluso"}}
      :english {:infinitive "to disappoint"
                :irregular {:past "disappointed"}}
      :synsem {:essere false
               :sem {:subj {:human true}
                     :obj {:human true}
                     :deliberate false
                     :discrete true
                     :activity false
                     :pred :deludere}}})
    ;; non-comparative
    ;; TODO: add comparative
    (unify adjective
           {:synsem {:cat :adjective
                     :sem {:pred :difficile
                           :comparative false
                           ;; "difficile" has a very specific list of things it can modify:
                           :mod {:drinkable false
                                 :human false
                                 :animate false
                                 :buyable false
                                 :legible true
                                 :activity true
                                 :artifact true
                                 :physical-object true
                                 :edible false}}}
            :italian {:italian "difficile"
                      :cat :adjective}
            :english {:english "difficult"
                      :cat :adjective}})

    {:synsem {:cat :prep
              :sem {:pred :di
                    :comparative true}
              :subcat {:1 {:cat :noun
                           :subcat '()
                           :def {:not :partitivo} ;; avoid alliteration like "di delle ragazze (of some women)"
                           :agr {:case :disj} ;; pronouns must be disjunctive (me/te/lui/lei...etc)
                           ;; non-pronouns will unify with this constraint.
                           }
                       :2 '()}}
     :italian {:italian "di"
               :initial true}
     :english "than"}

    {:synsem {:cat :det
              :def :partitivo
              :number :plur
              :gender :masc}
       :italian "di i"
     :english "some"}

    {:synsem {:cat :det
              :def :partitivo
              :number :sing
              :mass true
              :gender :fem}
     :italian "di la"
     :english "some"}

    {:synsem {:cat :det
              :def :partitivo
              :number :plur
              :gender :fem}
     :italian "di le"
     :english "some"}

    {:synsem {:cat :det
              :def :partitivo
              :number :sing
              :mass true
              :gender :masc}
     :italian "di il"
     :english "some"}

    (unify sentential-adverb
           {:synsem {:cat :sent-modifier
                     :sem {:pred :domani}
                     :subcat {:1 {:infl :futuro
                                  :sem {:tense :futuro}
                                  :subcat '()}}}
            :italian "domani"
            :english "tomorrow"})

    (unify sentential-adverb
           {:synsem {:cat :sent-modifier
                     :sem {:pred :dopodomani}
                     :subcat {:1 {:infl :futuro
                                  :sem {:tense :future}
                                  :subcat '()}}}
            :italian "dopodomani"
            :english "the day after tomorrow"})

    (unify agreement-noun
           common-noun
           countable-noun
           feminine-noun
           {:synsem {:sem human}}
           {:synsem {:sem {:pred :donna
                           :child false}}
            :italian {:italian "donna"}
            :english {:irregular {:plur "women"}
                      :english "woman"}})

    (unify
     intransitive
     {:italian {:infinitive "dormire"}
      :english {:infinitive "to sleep"
                :irregular {:past "slept"}}
      :synsem {:essere false
               :sem {:subj {:animate true}
                     :discrete false
                     :pred :dormire}}})

    (unify agreement-noun
           common-noun
           countable-noun
           masculine-noun
           {:synsem {:sem human}}
           {:synsem {:sem {:pred :dottore
                           :child false}}
            :italian {:italian "dottore"}
            :english {:english "doctor"}})

    (unify
     verb-subjective
     modal
     {:italian {:infinitive "dovere"
                :irregular {:present {:1sing "devo"
                                      :2sing "devi"
                                      :3sing "deve"
                                      :1plur "dobbiamo"
                                      :2plur "dovete"
                                      :3plur "devono"}}}
      :english {:infinitive "to have to"
                :irregular {:past "had to"
                            :present {:1sing "have to"
                                      :2sing "have to"
                                      :3sing "has to"
                                      :1plur "have to"
                                      :2plur "have to"
                                      :3plur "have to"}}}
      :synsem {:essere false ;; "io ho dovato..", not "io sono dovato.."
               :sem {:pred :dovere
                     :activity false
                     :discrete false
                     :subj {:human true} ;; TODO: relax this constraint: non-human things can be subject of dovere.
                     }}})

    {:synsem {:cat cat-of-pronoun
              :pronoun true
              :agr {:case disjunctive-case-of-pronoun
                    :person :3rd
                    :gender :fem
                    :number :plur}
              :sem {:human false
                    :place false
                    :pred :essi}
              :subcat '()}
     :english "them"
     :italian {:italian "esse"
               :cat cat-of-pronoun
               :case disjunctive-case-of-pronoun}}

    (let [subject (ref {:cat :noun})
          comp-sem (ref
                    {:activity false
                     :comparative false ;; to avoid *"essere alto di.." (but "essere meno alto di.." is ok)
                     :discrete false})]

      ;; essere: adjective
      ;; TODO: unify essere-adjective and essere-intensifier into one lexical entry.
      (let [gender (ref :top)
            number (ref :top)
            subject (ref {:cat :noun
                          :agr {:gender gender
                                :number number}})]
        (unify
         verb-subjective
         essere-common
         {:synsem {:subcat {:1 {:agr {:gender gender
                                      :number number}}}}}
         {:notes "essere-adjective"
          :synsem {:cat :verb
                   :subcat {:1 subject
                            :2 {:cat :adjective
                                :agr {:gender gender
                                      :number number}
                                :sem comp-sem
                                :subcat {:1 subject
                                         :2 '()}}}
                   :sem {:pred :essere
                         :obj comp-sem}}})))

      ;; essere: copula
      (let [gender (ref :top)
            number (ref :top)
            human (ref :top)]
        (unify
         transitive
         essere-common
         {:notes "copula" ;; significant only for debugging.
          :synsem {:cat :verb
                   :subcat {:1 {:cat :noun
                                :agr {:gender gender
                                      :number number}}
                            :2 {:cat :noun
                                :pronoun {:not true} ;; accusative pronouns cause unbounded depth-first searches on the subject side. (TODO: not sure if this problem is still present)
                                :def {:not :demonstrativo}
                                :agr {:gender gender
                                      :number number}}}
                   :sem {:pred :essere
                         :activity false
                         :discrete false
                         :subj {:human human}
                         :obj {:human human}}}}))

      ;; essere: intensifier
      ;; this is for e.g "essere più alto di quelle donne belle (to be taller than those beautiful women)"
      (let [subject (ref {:cat :noun})
            comp-sem (ref
                      {:activity false
                       :discrete false})]
        (unify
         verb-subjective
         essere-common
         {:notes "essere-intensifer"
          :synsem {:cat :verb
                   :subcat {:1 subject
                            :2 {:cat :intensifier
                                :sem comp-sem
                                :subcat {:1 subject
                                         :2 '()}}}
                   :sem {:pred :intensifier
                         :obj comp-sem}}}))

      (unify
       verb-aux-type
       verb-subjective
       essere-common
       {:notes "essere-aux"}
       {:synsem {:infl :present ;; TODO: consider relaxing this by allowing other inflections.
                 :subcat {:2 {:essere true}}}
        :english {:infinitive "to be" ;; just for documentation purposes: never reaches surface string due to :hidden=true.
                  :hidden true}}) ;; gets removed by morphological rules.

      {:synsem {:cat cat-of-pronoun
                    :pronoun true
                    :agr {:case disjunctive-case-of-pronoun
                          :person :3rd
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
                :agr {:case disjunctive-case-of-pronoun
                      :person :3rd
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

          ;; fare (to do well to): e.g. "tu ha fatto bene a vendere la casa"
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
                                 :mod {:human true}}} ;; sono gli umani possono essere gentile.
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
                 {:synsem {:sem {:animate false
                                 :legible false
                                 :artifact false
                                 :buyable false
                                 :speakable false
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
                    :number :plur}
           :italian "i miei"
           :english "my"}


      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :plur}
       :italian "i tuoi"
       :english "your"}

      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :plur}
       :italian "i vostri"
       :english "your (pl) "}

      (unify sentential-adverb
             {:synsem {:sem {:pred :ieri}
                       :subcat {:1 {:sem {:tense :past
                                          :activity true}}}}
              :italian "ieri"
              :english "yesterday"})

      {:synsem {:cat :det
                :def :def
                :gender :masc
                :number :sing}
       :italian "il"
       :english "the"}

      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :sing}
       :italian "il loro"
       :english "their"}

      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :sing}
       :italian "il nostro"
       :english "our"}

      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :sing}
       :italian "il mio"
       :english "my"}

      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :sing}
       :italian "il suo"
       :english "his"}

      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :sing}
       :italian "il suo"
       :english "her"}

      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :sing}
       :italian "il tuo"
       :english "your"}

      {:synsem {:cat :det
                :def :possessive
                :gender :masc
                :number :sing}
       :italian "il vostro"
       :english "your (pl) "}


      ;; working on: "mi sono comprato un nuovo cellulare"
      (unify agreement-noun
             masculine-noun
             common-noun
             countable-noun
      {:synsem {:cat :noun
                :sem {:pred :cellulare
                      :artifact true
                      :consumable false
                      :writable false
                      :speakable false}}
       :english {:english "mobile phone"}
       :italian {:italian "cellulare"}})

      {:synsem {:cat :fail ; :noun ;; disabling until more constraints are put on usage of it (TODO).
                :pronoun true
                :agr {:case :nom
                      :person :3rd
                      :number :sing}
                :sem (unify human {:pred :chiunque
                                   :elective-existential true})
                :subcat '()}
         :english "anyone"
       :italian "chiunque"}

      {:synsem {:cat :noun
                :pronoun true
                :agr {:case :nom
                      :person :1st
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
                :agr {:case disjunctive-case-of-pronoun
                      :person :3rd
                      :number :plur}
                :sem (unify human {:pred :lui})
                :subcat '()}
       :english "them"
       :italian {:italian "loro"
                 :cat cat-of-pronoun
                 :case disjunctive-case-of-pronoun}}

      {:synsem {:cat :noun
                :pronoun true
                :agr {:case :nom
                      :person :3rd
                      :gender :masc
                      :number :sing}
                :sem (unify human {:pred :lui})
                :subcat '()}
       :english "he"
       :italian "lui"}

      {:synsem {:cat :noun
                :pronoun true
                :agr {:case :nom
                      :person :3rd
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
                :agr {:case :nom
                      :person :3rd
                      :gender :fem
                      :number :sing}
                :sem (unify human {:pred :lei})
                :subcat '()}
       :english "she"
       :italian "lei"}

      {:synsem {:cat :noun
                :pronoun true
                :agr {:case :nom
                      :person :3rd
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
                :number :sing}
       :italian "la loro"
       :english "their"}

      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing}
       :italian "la mia"
       :english "my"}

      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing}
       :italian "la nostra"
       :english "our"}

      ;; TODO: add pronominal "la sua (hers)" and "la sua (his)"
      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing}
       :italian "la sua"
       :english "her"}

      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing}
       :italian "la sua"
       :english "his"}

      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing}
       :italian "la tua"
       :english "your"}

      {:synsem {:cat pronoun-noun
                :pronoun true
                :agr {:case pronoun-acc
                      :gender :masc
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
                :agr {:case pronoun-acc
                      :gender :masc
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
                :agr {:case pronoun-acc
                      :gender :fem
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
                :agr {:case pronoun-acc
                      :gender :fem
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
                :agr {:case pronoun-acc
                      :gender :fem
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
                  :irregular {:passato "letto"}}
        :english {:infinitive "to read" ;; spelled "read" but pronounced like "reed".
                  :irregular {:past "read"
                              :note "(past)"}} ;; spelled "read" but pronounced like "red".
        :synsem {:essere false
                 :sem {:pred :leggere
                       :discrete false
                       :subj {:human true}
                   :obj {:legible true}}}})

      {:synsem {:cat pronoun-noun
                :pronoun true
                :agr {:case pronoun-acc
                      :gender :masc
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
                :number :sing}
       :italian "la vostra"
       :english "your (pl)"}

      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :plur}
       :italian "le vostre"
       :english "your (pl)"}

      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :plur}
       :italian "le mie"
       :english "my"}

      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :plur}
       :italian "le tue"
       :english "your"}

      {:synsem {:cat cat-of-pronoun
                :pronoun true
                :agr {:case disjunctive-case-of-pronoun
                      :gender :fem
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
                :agr {:case disjunctive-case-of-pronoun
                      :gender :fem
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
                :agr {:case :nom
                      :person :3rd
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
                :agr {:case disjunctive-case-of-pronoun
                      :person :1st
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
                :agr {:case pronoun-acc
                      :person :1st
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

      {:synsem {:cat cat-of-pronoun
                :pronoun true
                :agr {:case disjunctive-case-of-pronoun
                      :gender :masc
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
                             ;; getting tired of people "making Napoli"
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
                             :mod {:physical-object true
                                   :human false}}}
              :italian {:italian "nero"
                        :cat :adjective}
              :english {:english "black"
                        :cat :adjective}})

      {:synsem {:cat cat-of-pronoun
                :pronoun true
                :agr {:case disjunctive-case-of-pronoun
                      :person :1st
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
                :agr {:case :nom
                      :person :1st
                      :number :plur}
                :sem (unify human {:pred :noi})
                :subcat '()}
       :english "we"
       :italian "noi"}
))

