(ns italianverbs.lexicon
  (:refer-clojure :exclude [get-in merge resolve find])
  (:require
   [clojure.core :as core]
   [clojure.set :refer :all]
   [clojure.tools.logging :as log]
   [italianverbs.lexiconfn :refer :all]
   [italianverbs.unify :refer :all :exclude (unify)]))

;; WARNING: clear blows away entire lexicon in backing store (mongodb or some other db).
(clear!)

(def lexicon

  ;; this filter is for debugging purposes to restrict lexicon to particular entries, if desired.
  ;; default shown is (not (nil? entry)) i.e. no restrictions except that an entry must be non-nil.
  ;;  (currently there is one nil below: "chiunque (anyone)").
  (filter (fn [entry]
            (or false
                (not (nil? entry))))

          ;; TODO: move this fn to lexiconfn: keep any code out of the lexicon proper.
          ;; this (map) adds, to each lexical entry, a copy of the serialized form of the entry.
          (map (fn [entry]
                 (if (fail? entry)
                   (log/warn (str "Ignoring this lexeme because (fail?=true): " entry))
                   ;; else, not fail, so add to lexicon.
                   (do
                     (log/debug (str "serializing entry: " entry))
                     (let [italian (get-in entry '(:italian))
                           entry
                           (conj
                            {:italian (if (string? italian)
                                        {:italian italian}
                                        italian)}
                    (dissoc
                     (if (not (= :none (get entry :serialized :none)))
                       (conj {:serialized (serialize entry)}
                             entry)
                       (conj {:serialized (serialize (dissoc entry :serialized))}
                         entry))
                     :italian))]
                       (log/debug (str "successfully serialized: " entry))
                       (implied entry)))))
               (list


            {:synsem {:cat :prep
                      :sem {:mod {:pred :a}
                         :comparative false}
                   :subcat {:1 {:cat :noun
                                :subcat '()
                                :sem {:place true}}
                            :2 '()}}
             :italian {:initial true
                       :italian "a"}
             :english "to"}

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
              {:synsem {:sem {:pred :affolato
                              :comparative true
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

     ;; non-comparative:
     (unify adjective
            subcat0
            {:synsem {:cat :adjective
                      :sem {:pred :alto
                            :comparative false
                            :mod {:human true}}}
             :italian {:italian "alto"}
             :english {:english "tall"}})

     (unify agreement-noun
            common-noun
            countable-noun
            masculine-noun
            {:synsem {:sem {:pred :amico
                            :human true
                            :child false}}
             :italian {:italian "amico"}
             :english {:english "friend"}})


     (unify proper-noun
            {:synsem {:sem {:pred :antonio
                            :human true}
                      :agr {:number :sing
                            :person :3rd
                            :gender :masc}}
             :italian "Antonio"
             :english "Antonio"})


     ;; comparative:
     (let [complement-complement-sem (ref {:human true}) ;; only humans can be tall.
           complement-sem (ref {:pred :di
                                :mod complement-complement-sem})
           subject-sem (ref {:human true})] ;; only humans can be tall.
       (unify adjective
              {:synsem {:sem {:pred :alto
                              :comparative true
                              :arg1 subject-sem
                              :arg2 complement-complement-sem}
                        :subcat {:1 {:cat :noun
                                     :sem subject-sem}
                                 :2 {:cat :prep
                                     :sem complement-sem}}}
               :italian {:italian "alto"}
               :english {:english "tall"}}))

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
              {:synsem {:sem {:pred :bello
                              :comparative true
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
              {:synsem {:sem {:pred :contento
                              :comparative true
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
             {:synsem {:sem {:pred :corto
                             :comparative true
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

    ;; using di (1,2) to debug:
    ;; * Questo professore è meno difficile di lo (This professor (♂) is less difficult than him).
    ;; (should be lui, not lo).
    {:synsem {:cat :prep
              :sem {:pred :di
                    :mod {:pred :di} ;; so that "venire" cannot match.
                    :comparative true}
              :subcat {:1 {:cat :verb ;; switched from :noun to :verb: to avoid "di" subcatting first arg as
                           ;; if it were a verb, e.g. "Paola di la ragazza".
                           :subcat '()
                           :def {:not :partitivo} ;; avoid alliteration like "di delle ragazze (of some women)"
                           :agr {:case :disj} ;; pronouns must be disjunctive (me/te/lui/lei...etc)
                           ;; non-pronouns will unify with this constraint.
                           ;; TODO: remove this constraint: for debugging only.
                           :sem {:human true}}
                       :2 '()}}
     :italian {:italian "di"
               :initial true}
     :english "than"}

    {:synsem {:cat :prep
              :sem {:pred :di
                    :comparative true}
              :subcat {:1 {:cat :noun
                           :subcat '()
                           :def {:not :partitivo} ;; avoid alliteration like "di delle ragazze (of some women)"
                           :agr {:case :disj} ;; pronouns must be disjunctive (me/te/lui/lei...etc)
                           ;; non-pronouns will unify with this constraint.
                           ;; TODO: remove this constraint: for debugging only.
                           :sem {:human true}}
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
                     :discrete false})]

      ;; essere: adjective
      ;; TODO: unify essere-adjective and essere-intensifier into one lexical entry.
      (unify
       essere-common
       {
        :notes "essere-adjective"
        :synsem {:cat :verb
                 :subcat {:1 subject
                          :2 {:cat :adjective
                              :sem comp-sem
                              :subcat {:1 subject
                                       :2 '()}}}
                 :sem {:pred :essere
                       :obj comp-sem}}}))

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
                                :pronoun {:not true} ;; accusative pronouns cause unbounded depth-first searches on the subject side.
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


      ;; inherently plural.
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

      {:italian {:italian "nuovo"
                 :initial true}
       :english {:english "new"}
       :synsem {:cat :adjective
                :sem {:pred :nuovo
                      :mod {:artifact true}}}}

      {:italian {:italian "nuovo"
                 :initial false}
       :english {:english "new"}
       :synsem {:cat :adjective
                :sem {:pred :nuovo
                      :mod {:artifact true}}}}

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

      (let [complement-semantics (ref {:mod {:pred :per}})]
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
                :agr {:case :nom
                      :person :3rd
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
                :agr {:case :nom
                      :person :3rd
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
               {:synsem {:sem {:pred :ricco
                         :comparative true
                               :arg1 subject-sem
                               :arg2 complement-complement-sem}
                         :subcat {:1 {:cat :noun
                                      :sem subject-sem}
                                  :2 {:cat :prep
                                      :sem complement-sem}}}
                :italian {:italian "ricco"}
                :english {:english "rich"}}))

      (unify
       intransitive
       {:italian {:infinitive "ridere"
                  :irregular {:passato "riso"}}
        :english {:infinitive "to laugh"
                  :irregular {:past "laughed"}}
        :synsem {:essere false
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
               {:synsem {:sem {:pred :semplice
                               :comparative true
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
               {:synsem {:sem {:pred :semplice
                               :comparative true
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
       intransitive
       {:italian {:infinitive "sognare"}
        :english {:infinitive "to dream"
                  :irregular {:past "dreamt"}}
        :synsem {:essere false
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
                :agr {:case disjunctive-case-of-pronoun
                      :person :2nd
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
                :agr {:case pronoun-acc
                      :person :2nd
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
                :agr {:case :nom
                      :gender :fem
                      :person :2nd
                      :number :sing}
                :sem (unify human {:pred :tu})
                :subcat '()}
       :english {:english "you"
                 :note " (&#x2640;)"} ;; unicode female symbol
       :italian "tu"}

      {:synsem {:cat :noun
                :pronoun true
                :agr {:case :nom
                      :gender :masc
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
     intransitive
     venire-common
     {:synsem {:essere true
               :sem {:pred :venire
                     :activity true
                     :motion true
                     :subj {:animate true}}}})

    (unify
     transitive-but-with-prepositional-phrase-instead-of-noun
     venire-common
     (let [complement-subject (ref {:animate true})
           complement-semantics (ref {:subj complement-subject
                                      :activity true
                                      :motion false ;; otherwise we get weird things like "he came to go".
                                      :deliberate true ;;;; you come somewhere to do something that you intended to do, not something that you do accidentally.
                                      :mod {:pred :per}})] ;; takes 'per' as proposition.
       {:synsem {:essere true
                 :sem {:pred :venire
                       :subj complement-subject
                       :obj complement-semantics}
                 :subcat {:2 {:sem complement-semantics}}}}))

    {:synsem {:cat pronoun-noun
              :pronoun true
              :agr {:case pronoun-acc
                    :person :2nd
                    :number :plur}
              :sem (unify human {:pred :voi})
              :subcat '()}
     :english "you all"
     :italian {:italian "vi"
               :initial true
               :cat pronoun-noun
               :case pronoun-acc}}

    (unify
     intransitive
     {:italian {:infinitive "vivere"}
      :english {:infinitive "to live"
                :irregular {:past "lived"}}
      :synsem {:essere true
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
                :agr {:case disjunctive-case-of-pronoun
                      :person :2nd
                      :number :plur}
                :sem (unify human {:pred :voi})
                :subcat '()}
       :english "you all"
       :italian {:italian "voi"
                 :cat cat-of-pronoun
                 :case disjunctive-case-of-pronoun}}


      {:synsem {:cat :noun
                :pronoun true
                :agr {:case :nom
                      :person :2nd
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
                       :subj {:animate true}}}})))))

;; (def tinylex (list (it "Napoli") (it "lui") (it "pensare")))
;;(def tinylex (list (it "Napoli"))); (it "lui"))); (it "pensare")))
;;(def tinylex (list (it "Napoli") (it "pensare") (it "Roma") (it "sognare") (it "dormire") (it "tavolo") (it "gatto") (it "lui") (it "lei")))
;;(def tinylex (list (it "Napoli") (it "Roma") (it "io")))
;(def tinylex (list (it "gatto") (it "uomo")))

;; TODO: what is this doing? looks expensive.
;(map (fn [lexeme]
;       (let [italian (:italian lexeme)
;             english (:english lexeme)]
;         (add italian english
;              lexeme)))
;     lexicon)

(def adjs-final-in-italian ;; e.g. "blanco" in "nero blanco"
  (filter (fn [lex] (= (get-in lex '(:synsem :cat)) :adjective)) lexicon))
(def adjs ;; backwards compatibility: use adjs-final-in-italian when the more
  ;; specific use is done.
  adjs-final-in-italian)
(def adjs-initial-in-italian  ;; e.g. "nuovo" in "nuovo cellulare"
  (filter (fn [lex]
            (and
             (= (get-in lex '(:italian :initial)) :true)
             (= (get-in lex '(:synsem :cat)) :adjective)))
          lexicon))

;; TODO: move to lexiconfn.
(defn lookup [query]
  (lookup-in query lexicon))

(defn it [italian]
  "same as it but no type conversion of singleton sets to take the first member."
  (let [result
        (union (set (lookup {:italian italian}))
               (set (lookup {:italian {:infinitive italian}}))
               (set (lookup {:italian {:infinitive {:infinitive italian}}}))
               (set (lookup {:italian {:italian italian}}))
               (set (lookup {:italian {:irregular {:passato italian}}})))]
    result))

(def it1 it) ; backwards compatibility

(defn en [english]
  (lookup {:english english}))

;; TODO: get 'aiutare' working again.
(def tinylex
  (union ;(it "aiutare")
         (it "andare")
         (it "dormire")
         (it "la")
         (it "il")
         (it "io")
         (it "ragazzo")
         (it "ragazza")
         (it "un")
         (it "vedere")
        ))

;; grammar builds a bunch of filtered lazy-seqs.
;; if lexicon changes, these lazy-seqs should be re-initialized
;; by reloading grammar.
;(load "grammar")
