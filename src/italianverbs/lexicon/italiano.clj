(ns italianverbs.lexicon.italiano
  (:refer-clojure :exclude [get-in]))

(require '[clojure.tools.logging :as log])
(require '[italianverbs.lexiconfn :refer (compile-lex map-function-on-map-vals)])
(require '[italianverbs.morphology.italiano :refer (agreement analyze exception-generator phonize italian-specific-rules)])
(require '[italianverbs.morphology.italiano :as m])
(require '[italianverbs.pos :refer (agreement-noun 
                                    cat-of-pronoun common-noun
                                    comparative
                                    countable-noun determiner
                                    drinkable-noun non-comparative-adjective noun
                                    pronoun-acc sentential-adverb
                                    verb verb-aux)])
(require '[italianverbs.pos.italiano :refer (adjective
                                             intransitive intransitive-unspecified-obj
                                             feminine-noun masculine-noun
                                             transitive verb-subjective)])
(require '[italianverbs.unify :refer (get-in unifyc)])
(require '[italianverbs.unify :as unify])

(defn trans-intrans [spec & [opts]]
  [(unifyc
    spec
    transitive
    (if (:subj opts)
      {:synsem {:sem {:subj (:subj opts)}}}
      :top)
    (if (:obj opts) {:synsem {:sem {:obj (:obj opts)}}}
        :top))

   (unifyc
    spec
    (if (:subj opts)
      {:synsem {:sem {:subj (:subj opts)}}}
      :top)
    intransitive-unspecified-obj)])

(def lexicon-source
  {
   "Antonia"
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
   [(let [location (ref {:place true})]
      {:synsem {:cat :prep
                :sem {:pred :a
                      :obj location
                      :comparative false}
                :subcat {:1 {:cat :noun
                             :subcat '()
                             :sem location}
                         :2 '()}}})

       {:synsem {:cat :prep
             :sem {:pred :in}
             :subcat {:1 {:cat :noun
                          :sem {:city true}}}}}


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
                            :2 '()}}})]

   "abbracciare"
   (trans-intrans {:synsem {:essere false
                            :sem {:pred :abbracciare
                                  :activity false
                                  :discrete false}}}
                  {:subj {:human true}
                   :obj {:animate true}})

   "acqua"
   (unifyc (:agreement noun)
          (:drinkable noun)
          (:feminine noun)
          {:synsem {:sem {:artifact false
                          :animate false
                          :pred :acqua}}})

   "affolato"

   [;; comparative
    (let [is-place (ref {:place true}) ;; only places can be crowded.
          than-this (ref {:pred :di
                          :mod is-place})]
      (unifyc adjective
             comparative
             {:synsem {:sem {:pred :affolato
                             :arg1 is-place
                             :arg2 is-place}
                       :subcat {:1 {:cat :noun
                                    :sem is-place}
                                :2 {:cat :prep
                                    :sem than-this}}}}))
    ;; non-comparative
    (unifyc adjective
           {:synsem {:cat :adjective
                     :sem {:pred :affolato
                           :comparative false
                           :place true}}})
                           

    ] ;; only places can be crowded.

   "aiutare"
   (trans-intrans {:synsem {:essere false
                            :sem {:pred :aiutare
                                  :activity true}}}
                  {:subj {:human true}
                   :obj {:human true}})

   "alto"

   [;; non-comparative:
    (let [subject-sem (ref {:human true}) ;; only humans can be tall.
          subject-agr (ref :top)] 
      (unifyc adjective
             non-comparative-adjective
             {:synsem {:cat :adjective
                       :sem {:pred :alto
                             :comparative false
                             :arg1 subject-sem
                             :human true}
                       :subcat {:1 {:cat :noun
                                    :agr subject-agr
                                    :sem subject-sem}
                                :2 '()}}}))

    ;; comparative:
    (let [complement-complement-sem (ref {:human true}) ;; only humans can be tall.
          complement-sem (ref {:pred :di
                               :mod complement-complement-sem})
          subject-sem (ref {:human true})] ;; only humans can be tall.
      (unifyc adjective
             comparative
             {:synsem {:sem {:pred :alto
                             :arg1 subject-sem
                             :arg2 complement-complement-sem}
                       :subcat {:1 {:cat :noun
                                    :sem subject-sem}
                                :2 {:cat :prep
                                    :sem complement-sem}}}}))]

   "amare"
   (trans-intrans {:synsem {:essere false
                            :sem {:pred :amare
                                  :activity false
                                  :discrete false}}}
                  {:subj {:human true}})

   "amico"
   (unifyc agreement-noun
          common-noun
          countable-noun
          masculine-noun
          {:synsem {:sem {:pred :amico
                          :human true
                          :child false}}})
   "andare"
   (map (fn [each]
          (unifyc
           each
           ;; common part of all andare lexemes:
           {:italiano {:italiano "andare"
                       :essere true
                       :drop-e true
                       :present {:1sing "vado"
                                 :2sing "vai"
                                 :3sing "va"
                                 :1plur "andiamo"
                                 :2plur "andate"
                                 :3plur "vanno"}}
            :synsem {:essere true
                     :sem {:subj {:animate true}
                           :activity false ;; because "I was going when (something happened) .." sounds weird.
                           :pred :andare
                           :discrete false
                           :motion false}}}))
        (list
          ;; "andare"-intransitive
          (unifyc
           intransitive
           {:synsem {:sem {:location '()}}}

           ;; "andare" that takes a prepositional phrase
           (let [place-sem (ref {:place true
                                 :pred :a})]
             {:synsem {:sem {:location place-sem}
                       :subcat {:2 {:sem place-sem
                                    :subcat '()
                                    :cat :prep}}}
              :note "andare-pp"}))))

   "avere"
   (let [avere-common {:synsem {:essere false
                                :cat :verb}
                       :italiano {:italiano "avere"
                                  :drop-e true
                                  :present {:1sing "ho"
                                            :2sing "hai"
                                            :3sing "ha"
                                            :1plur "abbiamo"
                                            :2plur "avete"
                                            :3plur "hanno"}}}]
     [(unifyc ;; 1. "avere": to possess something buyable
       transitive
       avere-common
       {:note "avere (possess)"
        :synsem {:sem {:pred :avere
                       :activity false
                       :discrete false
                       :subj {:human true}
                       :obj {:buyable true}}}})


      ;; 2. avere: unspecified object
      (unifyc
       avere-common
       verb-subjective
       intransitive-unspecified-obj
       {:note "avere (possess): unspecified object"
        :synsem {:sem {:pred :avere
                       :activity false
                       :discrete false
                       :subj {:human true}}}})


      ;; 3. "avere" that takes a transitive verb: e.g. "io l'ho vista (i saw her)"
      (let [agr-of-obj-of-main-verb (ref :top)]
        (unifyc
         verb-aux
         verb-subjective
         avere-common
        {:note "avere(aux): takes trans"
         :synsem {:infl :present
                  :subcat {:2 {:agr agr-of-obj-of-main-verb
                               :subcat {:2 {:agr agr-of-obj-of-main-verb
                                            :pronoun true}}
                               :essere false}}}}))

      ;; 4. "avere" that takes an intransitive verb or a transitive verb within a VP
      ;;    with the object (e.g. "io ho dormito (i slept)" or "io ho [mangiato la pizza] (i ate the pizza)"
      ;; "avere": auxiliary-verb: takes 2 args:
      ;; 1. subject that is the same as the subject of 2.
      ;; 2. an intransitive verb.
      (let [agr-of-subj-of-main-verb (ref :top)]
        (unifyc
         verb-aux
         verb-subjective
         avere-common
         {:note "avere(aux): takes intrans"
          :synsem {:infl :present
                   :subcat {:1 {:agr agr-of-subj-of-main-verb}
                            :2 {:essere false
                                :agr agr-of-subj-of-main-verb
                                :subcat {:1 {:agr agr-of-subj-of-main-verb}
                                         :2 '()}}}}}))])


   "bello"

   [;; non-comparative
    (unifyc adjective
           {:synsem {:sem {:pred :bello
                           :comparative false
                           }}}) ;; for now, no restrictions on what can be beautiful.
    
    (let [complement-complement-sem (ref :top) ;; for now no restrictions
          complement-sem (ref {:pred :di
                               :mod complement-complement-sem})
          subject-sem (ref :top)] ;; subject can be anything.
      (unifyc adjective
             comparative
             {:synsem {:sem {:pred :bello
                             :arg1 subject-sem
                             :arg2 complement-complement-sem}
                       :subcat {:1 {:cat :noun
                                    :sem subject-sem}
                                :2 {:cat :prep
                                    :sem complement-sem}}}}))]

   "bene"
   {:synsem {:cat :adverb
             :sem {:pred :bene}}}

   "bere"
   (let [bere-common
         {:italiano {:passato "bevuto"
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
                               :3plur "bevono"}}
          :synsem {:essere false
                   :sem {:pred :bere
                         :subj {:animate true}}}}]
     (trans-intrans bere-common
                    {:obj {:drinkable true}}))

   "bianco"
   (unifyc adjective
          {:synsem {:cat :adjective
                    :sem {:pred :bianco
                          :comparative false
                          :physical-object true
                          :human false}}
           :italiano {:masc {:plur "bianchi"}
                      :fem {:plur "bianche"}
                      :cat :adjective}})

   "birra"
   (unifyc agreement-noun
          drinkable-noun
          feminine-noun
          {:synsem {:sem {:pred :birra
                          :artifact true}}})


   "braccio"
   (unifyc agreement-noun
          common-noun
          countable-noun
          masculine-noun
          {:synsem {:sem {:pred :braccio
                          :part-of-human-body true}}
           ;; adding "bracci" as irregular because
           ;; current morphology.clj would otherwise return
           ;; "braccii".
           ;; TODO: might not be an exception so much
           ;; as a ortho-pholological rule "io" -plur-> "ia"
           :italiano {:plur "bracci"}})

   "brutto"
   ;; non-comparative
   ;; TODO: add comparative
   (unifyc adjective
          {:synsem {:cat :adjective
                    :sem {:pred :brutto
                          :comparative false
                          }} ;; for now, no restrictions on what can be ugly.
           :italiano {:cat :adjective}})

   "bucato"
   {:synsem {:cat :noun
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

   "calzoni"
   ;; inherently plural
   (unifyc agreement-noun
          common-noun
          countable-noun
          masculine-noun
          {:synsem {:sem {:pred :calzoni
                          :artifact true
                          :speakable false
                          :legible false
                          :consumable false
                          :clothing true}}})

   "cambiare" (trans-intrans {:synsem {:sem {:pred :cambiare}}})
   "cancellare" (unifyc {:synsem {:sem {:pred :cancellare}}} transitive)
   "cantare" (unifyc {:synsem {:sem {:pred :cantare}}} transitive)
   "caricare" (unifyc {:synsem {:sem {:pred :caricare}}} transitive)
   "cenare" (unifyc intransitive
                   {:synsem {:essere false
                             :sem {:subj {:human true}
                                   :pred :cenare}}})
   "desiderare" (unifyc {:synsem {:sem {:pred :desiderare}}} transitive)
   "dipingere" (unifyc transitive
                      {:synsem {
                                :sem {
                                      :pred :dipingere
                                      }
                                }
                       :italiano {:passato "dipinto"}
                       }
                      )
   "entrare" (unifyc intransitive
                    {:synsem {:essere true
                              :sem {:pred "entrare"}}})

   "frequentare" (unifyc {:synsem {:sem {:pred :frequentare}}} transitive)

   "funzionare" (unifyc intransitive
                       {:synsem {:essere false
                                 :sem {:pred :funzionare
                                       :subj {:human false}}}})
   "giocare" (unifyc {:synsem {:sem {:pred :giocare}}} transitive)
   "guidare" (unifyc {:synsem {:sem {:pred :guidare}}} transitive)
   "imparare" (unifyc {:synsem {:sem {:pred :imparare}}} transitive)
   "incontrare" (unifyc {:synsem {:sem {:pred :incontrare}}} transitive)
   "insegnare" (unifyc {:synsem {:sem {:pred :insegnare}}} transitive)
   "lavorare" (unifyc {:synsem {:sem {:pred :lavorare}}} transitive)
   "mandare" (unifyc {:synsem {:sem {:pred :mandare}}} transitive)
   "portare" (unifyc {:synsem {:sem {:pred :portare}}} transitive)
   "prendere" (unifyc transitive
                     {:synsem {:sem {:pred :prendere}}
                      :italiano {:passato "preso"}})
   "ricevere" (unifyc {:synsem {:sem {:pred :ricevere}}} transitive)
   "ricordare" (unifyc {:synsem {:sem {:pred :ricordare}}} transitive)
   "rispondere" (unifyc intransitive
                       {:synsem {:essere false
                                 :sem {:pred :rispondere}}
                        :italiano {:passato "risposto"}})

   "ritornare" (unifyc intransitive
                      {:synsem {:sem {:pred :ritornare}
                                :essere true}})

   "scaricare" (unifyc {:synsem {:sem {:pred :scaricare}}} transitive)
   "scrivere" (unifyc {:synsem {:sem {:pred :scrivere}}} transitive
                     {:italiano {:passato "scritto"}})
   "stampare" (unifyc {:synsem {:sem {:pred :stampare}}} transitive)
   "studiare" (unifyc {:synsem {:sem {:pred :studiare}}} transitive)
   "suonare" (unifyc {:synsem {:sem {:pred :suonare}}} transitive)
;   "telefonare" (unifyc intransitive
;                       {:synsem {:essere false
;                                 :sem :telephonare}})
   "chiedere" (unifyc transitive
                     {:synsem {:sem {:pred :chiedere}}
                      :italiano {:passato "chiesto"}})
   "tornare" (unifyc intransitive
                    {:synsem {:sem {:pred :tornare}
                              :essere true}})
   "usare" (unifyc {:synsem {:sem {:pred :usare}}} transitive)
   "vedere" (unifyc transitive
                   {:synsem {:sem {:pred :vedere}}
                    :italiano {:passato "visto"
                               :futuro-stem "vedr"}})

   "vendere" (unifyc {:synsem {:sem {:pred :vendere}}} transitive)
   "venire" (unifyc intransitive
                   {:synsem {:essere true
                             :sem {:pred :venire}}
                    :italiano {:passato "venuto"
                               :futuro-stem "verr"}})

   "camicia"
    (unifyc agreement-noun
           common-noun
           countable-noun
           feminine-noun
           {:synsem {:sem {:pred :camicia
                           :artifact true
                           :speakable false
                           ;; (although an exception would be tshirts with writing on them):
                           :legible false
                           :consumable false
                           :clothing true}}})

   "cane"
   (unifyc agreement-noun
          common-noun
          countable-noun
          masculine-noun
          {:synsem {:sem {:animal true
                          :human false
                          :pet true
                          :pred :cane}}})


   "casa"
    (unifyc agreement-noun
           common-noun
           countable-noun
           feminine-noun
           {:synsem {:sem {:pred :casa
                           :activity false ;; should not need this probably: should be inferrable from  :place==true or perhaps :artifact==true.
                           :buyable true
                           :artifact true
                           :place true}}})

    "cattivo"
     (unifyc adjective
            {:synsem {:cat :adjective
                      :sem {:pred :cattivo
                            :comparative false
                            :human true;; TODO:should not need this because child => human.
                            :child true}}
             :italiano {:cat :adjective}})

      ;; working on: "mi sono comprato un nuovo cellulare"
     "cellulare"
      (unifyc agreement-noun
             masculine-noun
             common-noun
             countable-noun
      {:synsem {:cat :noun
                :sem {:pred :cellulare
                      :artifact true
                      :consumable false
                      :writable false
                      :place false
                      :speakable false}}})
      
      "chiunque"
      {:synsem {:cat :fail ; :noun ;; disabling until more constraints are put on usage of it (TODO).
                :pronoun true
                :case :nom
                :agr {:person :3rd
                      :number :sing}
                :sem {:human true
                      :pred :chiunque
                      :elective-existential true}
                :subcat '()}}

      "ci"
      {:synsem {:cat :noun
                :pronoun true
                :case pronoun-acc
                :agr {:person :1st
                      :number :plur}
                :sem {:human true 
                      :pred :noi}
                :subcat '()}
       :italiano {:initial true
                  :cat :noun
                  :case pronoun-acc}}

      "cipolla"
      (unifyc agreement-noun
             common-noun
             feminine-noun
             {:synsem {:sem {:pred :cipolla
                             :edible true
                             :animate false
                             :artifact false}}})

      "cercare"
      (trans-intrans {:synsem {:essere false
                               :sem {:activity true
                                     :discrete false
                                     :pred :cercare}}}
                     {:subj {:animate true}})

      "città"
      (unifyc agreement-noun
             common-noun
             countable-noun
             feminine-noun
             {:synsem {:sem {:pred :città
                             :buyable false  ;; can't buy a city (unless you're a billionaire like Mike Bloomberg)
                             :artifact true ;;
                             :city true}
                       :subcat {:1 {:cat :det
                                    :def :def}}}})
      
      "compito"
      (unifyc agreement-noun
             common-noun
             countable-noun
             masculine-noun
             {:synsem {:sem {:pred :compito
                             :legible true
                             :speakable false
                             :buyable false
                             :artifact true}}})

      "comprare"
      (trans-intrans {:synsem {:essere false
                               :sem {:pred :comprare}}}
                     {:subj {:human true}
                      :obj {:buyable true}})
      
      "contento"
      [(let [complement-complement-sem (ref {:human true})
             complement-sem (ref {:pred :di
                                  :mod complement-complement-sem})
             subject-sem (ref {:place true})]
         (unifyc adjective
                comparative
                {:synsem {:sem {:pred :contento
                                :arg1 subject-sem
                                :arg2 complement-complement-sem}
                        :subcat {:1 {:cat :noun
                                     :sem subject-sem}
                                 :2 {:cat :prep
                                     :sem complement-sem}}}})

         ;; non-comparative
         (unifyc adjective
                {:synsem {:cat :adjective
                          :sem {:pred :contento
                                :comparative false
                                :human true}}}))]

      "corto"
      [(let [complement-complement-sem (ref {:human true}) ;; only humans can be short.
             complement-sem (ref {:pred :di
                                  :mod complement-complement-sem})
             subject-sem (ref {:human true})] ;; only humans can be short.
        (unifyc adjective
               comparative
               {:synsem {:sem {:pred :corto
                               :arg1 subject-sem
                               :arg2 complement-complement-sem}
                         :subcat {:1 {:cat :noun
                                      :sem subject-sem}
                                  :2 {:cat :prep
                                      :sem complement-sem}}}}))

       ;; non-comparative
       (unifyc adjective
              {:synsem {:cat :adjective
                        :sem {:pred :corto
                              :comparative false
                              :human true}}
               :italiano {:cat :adjective}})]

      ;; TODO: account for "dare" being ditransitive.
      "dare"
      (trans-intrans
       {:italiano {:present {:2sing "dai"
                             :3plur "danno"}
                   :futuro-stem "dar"}
        :synsem {:cat :verb
                 :essere false
                 :sem {:pred :dare}}}
       {:subj {:human true}
        :iobj {:animate true}
        :obj {:buyable true}})

      "dei"
      {:synsem {:cat :det
                :def :partitivo
                :number :plur
                :gender :masc}}

      "delle"
      {:synsem {:cat :det
                :def :partitivo
                :number :plur
                :gender :fem}}

               
      "deludere"
      (trans-intrans {:italiano {:passato "deluso"}
                      :synsem {:essere false
                               :sem {:deliberate false
                                     :discrete true
                                     :activity false
                                     :pred :deludere}}}
                     {:subj {:human true}
                      :obj {:human true}})

      "difficile"
      ;; non-comparative
      ;; TODO: add comparative
      (unifyc adjective
             {:synsem {:cat :adjective
                       :sem {:pred :difficile
                             :comparative false
                             ;; "difficile" has a very specific list of things it can modify:
                             :drinkable false
                             :human false
                             :animate false
                             :buyable false
                             :legible true
;                             :activity true ;; TODO: cannot declare this as an activity because of some semantic implicature..
                             :artifact true
                             :physical-object true
                             :edible false}}
              :italiano {:cat :adjective}})
      
      "di"
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
       :italiano {:initial true}}

      "di la"
      {:synsem {:cat :det
                :def :partitivo
                :number :sing
                :mass true
                :gender :fem}}

      "di le"
      {:synsem {:cat :det
                :def :partitivo
                :number :plur
                :gender :fem}}

      "di il"
      {:synsem {:cat :det
                :def :partitivo
                :number :sing
                :mass true
                :gender :masc}}

      "domani"
      (unifyc sentential-adverb
             {:synsem {:cat :sent-modifier
                       :sem {:pred :domani}
                       :subcat {:1 {:infl :futuro
                                    :sem {:tense :futuro}
                                    :subcat '()}}}
              :italiano "domani"})

      "donna"
      (unifyc agreement-noun
             common-noun
             countable-noun
             feminine-noun
             {:synsem {:sem {:human true
                             :pred :donna
                             :child false}}})

      "dopodomani"
      (unifyc sentential-adverb
             {:synsem {:cat :sent-modifier
                       :sem {:pred :dopodomani}
                       :subcat {:1 {:infl :futuro
                                    :sem {:tense :future}
                                    :subcat '()}}}})

      "dormire"
      (unifyc
       intransitive
       {:synsem {:cat :verb
                 :essere false
                 :sem {:subj {:animate true}
                       :discrete false
                       :pred :dormire}}})


      "essere"
      (let [essere-common 
            (let [infl (ref :top)
                  agr (ref :top)]
              {:synsem {:agr agr
                        :cat :verb
                        :essere true
                        :infl infl
                        :subcat {:1 {:agr agr}}}
               :italiano {:agr agr
                          :futuro-stem "sar"
                          :essere true
                          :infinitive "essere"
                          :infl infl
                          :present {:1sing "sono"
                                    :2sing "sei"
                                    :3sing "è"
                                    :1plur "siamo"
                                    :2plur "siete"
                                    :3plur "sono"}
                          :passato "stato"
                          :imperfetto {:1sing "ero"
                                       :2sing "eri"
                                       :3sing "era"
                                       :1plur "eravamo"
                                       :2plur "eravate"
                                       :3plur "erano"}
                          :futuro {:1sing "sarò"
                                   :2sing "sarai"
                                   :3sing "sarà"
                                   :1plur "saremo"
                                   :2plur "sarete"
                                   :3plur "saranno"}}})]
        [;; essere: adjective
         ;; TODO: unify essere-adjective and essere-intensifier into one lexical entry.
         (let [gender (ref :top)
               number (ref :top)]
           (unifyc
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
           (unifyc
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
           (unifyc
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

         (unifyc essere-common
                verb-aux
                verb-subjective
                {:italiano {:notes "essere-aux"}})])

      "gatto"
      (unifyc agreement-noun
             common-noun
             countable-noun
             masculine-noun
             {:synsem {:sem {:animal true
                             :human false
                             :pred :gatto
                             :pet true}}})
      
      "i"
      (unifyc determiner
             {:synsem {:cat :det
                       :def :def
                       :gender :masc
                       :number :plur}})

      "il"
      (unifyc determiner
             {:synsem {:cat :det
                       :def :def
                       :gender :masc
                       :number :sing}})

      "io"
      {:synsem {:cat :noun
                :pronoun true
                :case :nom
                :agr {:person :1st
                      :number :sing}
                :sem {:human true
                      :pred :io}
                :subcat '()}}

      "la"
      ;; TODO: refactor commonalities
      ;; 1. pronoun: human
      [{:synsem {:cat :noun
                 :pronoun true
                 :case pronoun-acc
                 :agr {:gender :fem
                       :person :3rd
                       :number :sing}
                 :sem {:human true
                       :pred :lei}
                 :subcat '()}
        :italiano {:initial true   ;; TODO: verify that 'la' above and this below are being unified correctly.
                   :cat :noun
                   :case pronoun-acc}}

       ;; 2. pronoun: non-human
       {:synsem {:cat :noun
                 :pronoun true
                 :case pronoun-acc
                 :agr {:gender :fem
                       :person :3rd
                       :number :sing}
                 :sem {:human false
                       :place false ;; "they go to it (loro vanna a la)" sounds strange
                       :pred :lei}
                 :subcat '()}
        :italiano {:initial true
                   :cat :noun
                   :case pronoun-acc}}

       ;; 3. article
       {:synsem {:cat :det
                 :def :def
                 :gender :fem
                 :number :sing}}]
      
      "la loro"
      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing
                :sem {:number :plur
                      :person :3rd}}}

      "la mia"
      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing
                :sem {:number :sing
                      :person :1st}}}

      "la nostra"
      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing
                :sem {:number :plur
                      :person :1st}}}

   ;; TODO for below: add pronominal "la sua" (translated in English as "his" and "hers", depending on gender of the owner of the referent).
   ;; e.g. "I gatti sono i suoi. (The cats are hers) (if talking about a female owner) or (The cats are his) (if talking about a male owner).
   "la sua"
   {:synsem {:cat :det
             :def :possessive
             :gender :fem
             :number :sing
             :sem {:number :sing
                   :person :3rd}}}
   "la tua"
   [{:synsem {:cat :det
              :def :possessive
              :gender :fem
              :number :sing
              :sem {:number :sing
                    :person :2nd}}}

    {:synsem {:cat :det
              :def :possessive
              :gender :fem
              :number :sing
                :sem {:number :plur
                      :person :2nd}}}]

   "le"
   {:synsem {:cat :det
             :def :def
             :gender :fem
             :number :plur}}

   "lei"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :fem
                   :number :sing}
             :sem {:pred :lei} ;; note: we don't specify human=true (english "it").
             :subcat '()}}

   "leggere"
   (trans-intrans {:italiano {:passato "letto"}
                   :synsem {:essere false
                            :sem {:pred :leggere
                                  :discrete false}}}
                  {:subj {:human true}})

   "libro"
   (unifyc agreement-noun
          common-noun
          countable-noun
          masculine-noun
          {:synsem {:sem {:pred :libro
                          :legible true
                          :speakable false
                          :mass false
                          :buyable true
                          :consumable false
                          :artifact true}}})

   "lo"
   [{:synsem {:cat :noun
              :pronoun true
              :case pronoun-acc
              :agr {:gender :masc
                    :person :3rd
                    :number :sing}
              :sem {:human true
                    :pred :lo}
              :subcat '()}}

    {:synsem {:cat :noun
              :pronoun true
              :case pronoun-acc
              :agr {:gender :masc
                    :person :3rd
                    :number :sing}
              :sem {:human false
                    :place false
                    :pred :lo}
              :subcat '()}
       :italiano {:initial true  ;; TODO: verify that 'lo' above and this below are being unified correctly.
                  :pronoun true
                  :cat :noun
                  :case pronoun-acc}}]

   "loro"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :number :plur}
             :sem {:human true
                   :pred :loro}
             :subcat '()}}

   "lui"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :masc
                   :number :sing}
             :sem {:pred :lui} ;; note: we don't specify human=true (english "it").
             :subcat '()}}

   "madre"
   (unifyc agreement-noun
          common-noun
          countable-noun
          feminine-noun
          {:synsem {:sem {:human true
                          :pred :madre
                          :child false}}})

   "mangiare"
   (trans-intrans
    {:synsem {:essere false
              :sem {:pred :mangiare
                    :subj {:animate true}}}}
    {:obj {:edible true}})

   ;; non-comparative
   ;; TODO: add comparative
   "nero"
   (unifyc adjective
          {:synsem {:cat :adjective
                    :sem {:pred :nero
                          :comparative false
                          :physical-object true
                          :human false}}})

   "noi"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :number :plur}
             :sem {:human true
                   :pred :noi}
             :subcat '()}}

   "pane"
   ;; inherently singular.
   (unifyc agreement-noun
          common-noun
          masculine-noun
          {:synsem {:sem {:pred :pane
                          :edible true
                          :artifact true}
                    :subcat {:1 {:cat :det
                                 :number :sing
                                 :def :def}}}})


   "parlare"
   (let [common
         {:synsem {:essere false
                   :sem {:pred :parlare
                         :subj {:human true}}}}]
     (unifyc common transitive
            {:synsem {:obj {:speakable true}}})
     (unifyc common intransitive intransitive-unspecified-obj))

   "qualche"
   {:synsem {:cat :det
             :def :indef
             :mass false
             :number :sing}}

   ;; non-comparative
   ;; TODO: add comparative
   "rosso"
   (unifyc adjective
          {:synsem {:cat :adjective
                    :sem {:pred :rosso
                          :comparative false
                          :physical-object true
                          :human false}}})

   "tu"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :number :sing}
             :sem {:human true
                   :pred :tu}
             :subcat '()}}

   "un"
   [{:synsem {:cat :det
              :def :indef
              :mass false
              :gender :masc
              :number :sing}}]
   "una"
   [{:synsem {:cat :det
              :def :indef
              :mass false
              :gender :fem
              :number :sing}}]

   "vino"
   (unifyc drinkable-noun
          agreement-noun
          masculine-noun
          {:synsem {:sem {:pred :vino
                          :artifact true}}})

   "voi"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :number :plur}
             :sem {:human true
                   :pred :voi}
             :subcat '()}}

})
