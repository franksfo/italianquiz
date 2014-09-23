(ns italianverbs.lex.a_essere
  (:require
   [italianverbs.lexiconfn :refer :all]
   [italianverbs.pos :refer :all]))

(def a-essere
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
                            :place true}} ;; only places can be crowded.
             :italian {:italian "affolato"
                       :cat :adjective}
             :english {:english "crowded"
                       :cat :adjective}})

     {:italian {:infinitive "aiutare"}
      :english {:infinitive "to help"}
      :synsem {:essere false
               :disable :fail
               :sem {:pred :aiutare
                     :activity true
                     :subj {:human true}
                     :obj {:human true}}}}

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
                                     :human true}
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

      (unify transitive
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
      andare-common
      {:synsem {:sem {:location '()}}})

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

     {:synsem {:agr {:number :sing
                     :person :3rd
                     :gender :masc}
               :sem {:pred :antonio
                     :human true}
               :propernoun true}
      :italian "Antonio"
      :english "Antonio"}

     {:synsem {:sem {:pred :antonia
                     :human true}
               :agr {:number :sing
                     :person :3rd
                     :gender :fem}
               :propernoun true}
      :italian "Antonia"
      :english "Antonia"}

     ;; 1. "avere": to possess something buyable
     (unify
      transitive
      avere-common
      {:synsem {:sem {:pred :avere
                      :activity false
                      :discrete false
                      :subj {:human true}
                      :obj {:buyable true}}}})

     ;; 2. "avere" that takes a transitive verb: e.g. "io l'ho vista (i saw her)"
     (let [agr-of-obj-of-main-verb (ref :top)]
       (unify
        verb-aux
        verb-subjective
        avere-common
        {:note "avere(aux): takes trans"
         :synsem {:infl :present
                  :subcat {:2 {:agr agr-of-obj-of-main-verb
                               :subcat {:2 {:agr agr-of-obj-of-main-verb}}
                               :essere false}}}
         :english {:hidden true}}))

     ;; 3. "avere" that takes an intransitive verb or a transitive verb within a VP
     ;;    with the object (e.g. "io ho dormito (i slept)" or "io ho [mangiato la pizza] (i ate the pizza)"
     ;; "avere": auxiliary-verb: takes 2 args:
     ;; 1. subject that is the same as the subject of 2.
     ;; 2. an intransitive verb.
     (unify
      verb-aux
      verb-subjective
      avere-common
      {:note "avere(aux): takes intrans"
       :synsem {:infl :present
                :subcat {:2 {:essere false
                             :subcat {:1 :top
                                      :2 '()}}}}
       :english {:hidden true}})

     ;; non-comparative
     (unify adjective
            subcat0
            {:synsem {:sem {:pred :bello
                            :comparative false
                            } ;; for now, no restrictions on what can be beautiful.
             :italian {:italian "bello"}
             :english {:english "beautiful"}}})

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
                             :futuro-stem "berr"
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
                            :physical-object true
                            :human false}}
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
                            }} ;; for now, no restrictions on what can be ugly.
             :italian {:italian "brutto"
                       :cat :adjective}
             :english {:english "ugly"
                       :cat :adjective}})

     {:italian "bucato"
      :english "laundry"
      :synsem {:cat :noun
               :agr {:gender :masc
                     :number :sing}
               :sem {:animate false
                     :drinkable false
                     :edible false
                     :legible false
                     :mass false
                     :pred :bucato
                     :speakable false}
               :subcat {:1 {:def :def}}}}

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
                            :human true;; TODO:should not need this because child => human.
                            :child true}}
             :italian {:italian "cattivo"
                       :cat :adjective}
             :english {:english "naughty"
                       :cat :adjective}})

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
                      :place false
                      :speakable false}}
       :english {:english "mobile phone"}
       :italian {:italian "cellulare"}})

      {:synsem {:cat :fail ; :noun ;; disabling until more constraints are put on usage of it (TODO).
                :pronoun true
                :case :nom
                :agr {:person :3rd
                      :number :sing}
                :sem (unify human {:pred :chiunque
                                   :elective-existential true})
                :subcat '()}
         :english "anyone"
       :italian "chiunque"}

    {:synsem {:cat pronoun-noun
              :pronoun true
              :case pronoun-acc
              :agr {:person :1st
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
                            :human true}}
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
                           :human true}}
            :italian {:italian "corto"
                      :cat :adjective}
            :english {:english "short"
                      :cat :adjective}})

    {:italian {:infinitive "dare"
               :futuro-stem "dar"}
     :english {:infinitive "to give"
               :irregular {:past "gave"}}
     :synsem {:cat :verb
              :essere false
              :sem {:subj {:human true}
                    :iobj {:animate true}
                    :obj {:buyable true}
                    :pred :dare}}}
               

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
                           :drinkable false
                           :human false
                           :animate false
                           :buyable false
                           :legible true
                           :activity true
                           :artifact true
                           :physical-object true
                           :edible false}}
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
                           :case :disj ;; pronouns must be disjunctive (me/te/lui/lei...etc)
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

    {:italian {:infinitive "dormire"}
     :english {:infinitive "to sleep"
               :irregular {:past "slept"}}
     :synsem {:cat :verb
              :essere false
              :sem {:subj {:animate true}
                    :discrete false
                    :pred :dormire}}}

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
                :drop-e true
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
      :synsem {:modal true
               :sem {:pred :dovere
                     :activity false
                     :discrete false
                     :subj {:human true} ;; TODO: relax this constraint: non-human things can be subject of dovere.
                     }}})

    {:synsem {:cat cat-of-pronoun
              :pronoun true
              :case disjunctive-case-of-pronoun
              :agr {:person :3rd
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

    ;; essere: adjective
    ;; TODO: unify essere-adjective and essere-intensifier into one lexical entry.
    (let [gender (ref :top)
          number (ref :top)]
      (unify
       essere-common
       {:notes "essere-adjective"
        :synsem {:cat :verb
                 :sem {:pred :essere
                       :subj :top
                       :obj :top}
                 :subcat {:1 {:cat :noun
                              :agr {:gender gender
                                    :number number}}
                          :2 {:cat :adjective
                              :sem {:comparative false}
                              :subcat {:1 :top
                                       :2 '()}
                              :agr {:gender gender
                                    :number number}}}}}))

    ;; essere: copula ;; note that we don't enforce agreement the same here as we do in essere-adjective: TODO: try to make more consistent.
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
    (let [gender (ref :top)
          number (ref :top)
          subject (ref {:agr {:gender gender
                              :number number}
                        :cat :noun})
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

    (unify essere-common
           verb-aux
           verb-subjective
           {:english {;; :infinitive value is just for documentation purposes: never reaches surface string due to 
                      ;; :hidden=true immediately below it.
                      :infinitive "to be"
                      :hidden true} ;; gets removed by morphological rules.
            :notes "essere-aux"})))
