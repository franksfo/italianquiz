(ns italianverbs.lexicon
  (:use [italianverbs.lexiconfn]
        [clojure.test])
  (:require
   [clojure.tools.logging :as log]
   [italianverbs.unify :as fs]
   [clojure.set :as set]))

;; WARNING: clear blows away entire lexicon in backing store (mongodb).
(clear!)

;; useful abbreviations (aliases for some commonly-used maps):
;; TODO: combine with abbreviations inside the (def nouns).
(def human {:human true})
(def animal {:animate true :human false})

(defn sem-impl [input]
  "expand input feature structures with semantic (really cultural) implicatures, e.g., if human, then not buyable or edible"
  (cond
   (= input :top) input
   true
   (let [animate (if (= (fs/get-in input '(:animate))
                        true)
                   {:artifact false
                    :mass false
                    :furniture false
                    :physical-object true
                    :part-of-human-body false
                    :drinkable false
                    :speakable false
                    :place false}{})
         artifact (if (= (fs/get-in input '(:artifact))
                         true)
                    {:animate false
                     :physical-object true}{})

         buyable (if (= (fs/get-in input '(:buyable))
                        true)
                   {:human false
                    :part-of-human-body false})

         city (if (= (fs/get-in input '(:city))
                     true)
                {:place true
                 :legible false})

         clothing (if (= (fs/get-in input '(:clothing))
                         true)
                    {:animate false
                     :place false
                     :physical-object true}{})


         consumable-false (if (= (fs/get-in input '(:consumable)) false)
                            {:drinkable false
                             :edible false} {})

         drinkable
         ;; drinkables are always mass nouns.
         (if (= (fs/get-in input '(:drinkable)) true)
           {:mass true
            :legible false}{})

         drinkable-xor-edible-1
         ;; things are either drinkable or edible, but not both (except for weird foods
         ;; like pudding or soup). (part 1: edible)
         (if (and (= (fs/get-in input '(:edible)) true)
                  (= (fs/get-in input '(:drinkable) :notfound) :notfound))
           {:drinkable false}{})

         drinkable-xor-edible-2
         ;; things are either drinkable or edible, but not both (except for weird foods
         ;; like pudding or soup). (part 2: drinkable)
         (if (and (= (fs/get-in input '(:drinkable)) true)
                  (= (fs/get-in input '(:edible) :notfound) :notfound))
           {:edible false})

         ;; qualities of foods and drinks.
         edible (if (or (= (fs/get-in input '(:edible)) true)
                        (= (fs/get-in input '(:drinkable)) true))
                  {:buyable true
                   :physical-object true
                   :consumable true
                   :human false
                   :pet false
                   :place false
                   :speakable false
                   :legible false
                   :furniture false
                   :part-of-human-body false}{})

         furniture (if (= (fs/get-in input '(:furniture))
                          true)
                     {:artifact true
                      :animate false
                      :buyable true
                      :drinkable false
                      :legible false
                      :edible false
                      :place false
                      :speakable false})

         human (if (= (fs/get-in input '(:human))
                      true)
                 {:buyable false
                  :physical-object true
                  :edible false
                  :animate true
                  :part-of-human-body false
                  :drinkable false
                  :speakable false
                  :place false}{})
         inanimate (if (= (fs/get-in input '(:animate))
                           false)
                     {:human false
                      :part-of-human-body false}{})

         ;; legible(x) => artifact(x),drinkable(x,false),edible(x,false),human(x,false)
         legible
         (if (= (fs/get-in input '(:legible)) true)
           {:artifact true
            :drinkable false
            :human false
            :furniture false
            :part-of-human-body false
            :edible false})

         non-places (if (or
                         (= (fs/get-in input '(:legible)) true)
                         (= (fs/get-in input '(:part-of-human-body)) true)
                         (= (fs/get-in input '(:pred)) :fiore)
                         (= (fs/get-in input '(:pred)) :scala))
                   {:place false})

         ;; artifact(x,false) => legible(x,false)
         not-legible-if-not-artifact
         (if (= (fs/get-in input '(:artifact)) false)
           {:legible false})

         part-of-human-body
         (if (= (fs/get-in input '(:part-of-human-body)) true)
           {:speakable false
            :buyable false
            :animate false
            :edible false
            :drinkable false
            :legible false
            :artifact false})

         ;; we don't eat pets (unless things get so desperate that they aren't pets anymore)
         pets (if (= (fs/get-in input '(:pet))
                     true)
                {:edible false
                 :buyable true
                 :physical-object true
                 })

         place (if (= (fs/get-in input '(:place))
                      true)
                 {:animate false
                  :speakable false
                  :physical-object true
                  :drinkable false
                  :edible false
                  :legible false}{})

         ]
     (let [merged
           (if (= input :fail) :fail
               (fs/merge input animate artifact buyable city clothing consumable-false drinkable
                         drinkable-xor-edible-1 drinkable-xor-edible-2
                         edible furniture human inanimate
                         legible non-places not-legible-if-not-artifact part-of-human-body pets place
                         ))]
       (log/debug (str "sem-impl so far: " merged))
       (if (not (= merged input))
         (sem-impl merged) ;; we've added some new information: more implications possible from that.
         merged))))) ;; no more implications: return

(def modal
  "modal verbs take a VP[inf] as their 2nd arg. the subject of the modal verb is the same as the subject of the VP[inf]"
  (let [subj-sem (ref :top)
        vp-inf-sem (ref {:subj subj-sem})
        subj-subcat (ref {:cat :noun
                          :sem subj-sem})]
     {:synsem {:sem {:subj subj-sem
                     :obj vp-inf-sem}
               :subcat {:1 subj-subcat
                        :2 {:sem vp-inf-sem
                            :cat :verb
                            :infl :infinitive
                            :subcat {:1 subj-subcat
                                     :2 '()}}}}
      :english {:modal true}}))

(def nouns
  (let [gender (ref :top)

        ;; common nouns are underspecified for number: number selection (:sing or :plur) is deferred until later.
        ;; (except for mass nouns which are only singular)
        number (ref :top)

        ;; common nouns are neither nominative or accusative. setting their case to :top allows them to (fs/match) with
        ;; verbs' case specifications like {:case {:not :acc}} or {:case {:not :nom}}.
        case (ref :top)

        person (ref :top)

        agreement
        (let [number (ref :top)
              gender (ref :top)
              person (ref :top)
              agr (ref {:number number
                        :gender gender
                        :case :top
                        :person person})
              cat (ref :top)]
          {:synsem {:cat cat
                    :subcat {:1 {:number number
                                 :person person
                                 :gender gender}}
                    :agr agr}
           :italian {:cat cat
                     :agr agr}
           :english {:cat cat
                     :agr agr}})

        common-noun
        (unify
         {:synsem {:cat :noun
                   :agr {:person :3rd}
                   :subcat {:1 {:cat :det}}}})

        masculine {:synsem {:agr {:gender :masc}}}
        feminine {:synsem {:agr {:gender :fem}}}

        mass-noun
        (let [mass (ref true)]
          {:synsem {:subcat {:1 {:cat :det
                                 :mass mass
                                 :number :sing}}
                    :sem {:mass mass}}})

        countable-noun
        (let [mass (ref false)]
          {:synsem {:subcat {:1 {:cat :det
                                 :mass mass}}
                    :sem {:mass mass}}})

        drinkable
        (unify mass-noun
               common-noun
               {:synsem {:sem {:number :sing
                               :drinkable true}}})

        ]
    (list


     (unify agreement
            drinkable
            feminine
            {:italian {:italian "acqua"}
             :english {:english "water"}
             :synsem {:sem {:artifact false
                            :animate false
                            :pred :acqua}}})

     (unify agreement
            common-noun
            countable-noun
            masculine
            {:synsem {:sem {:pred :amico
                            :human true}}
             :italian {:italian "amico"}
             :english {:english "friend"}})

     (unify agreement
            drinkable
            feminine
            {:italian {:italian "birra"}
             :english {:english "beer"}
             :synsem {:sem {:pred :birra
                            :artifact true}}})

    (unify agreement
           common-noun
           countable-noun
           masculine
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


    (unify agreement
           common-noun
           countable-noun
           masculine
           {:synsem {:sem {:pred :compito
                           :legible true
                           :speakable false
                           :buyable false
                           :artifact true
                           :activity true}}
            :italian {:italian "compito"}
            :english {:english "homework assignment"}})

    (unify agreement
           common-noun
           countable-noun
           masculine
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

    (unify agreement
           common-noun
           countable-noun
           feminine
           {:synsem {:sem {:pred :città
                           :buyable false  ;; can't buy a city (unless you're a billionaire like Mike Bloomberg)
                           :artifact true ;;
                           :city true}}
            :italian {:italian "città"}
            :english {:english "city"}}
           {:synsem {:subcat {:1 {:cat :det
                                  :def :def}}}})


    ;; inherently plural
    (unify agreement
           common-noun
           countable-noun
           masculine
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

    (unify agreement
           common-noun
           countable-noun
           feminine
           {:synsem {:sem {:pred :camicia
                           :artifact true
                           :speakable false
                           ;; (although an exception would be tshirts with writing on them):
                           :legible false
                           :consumable false
                           :clothing true}}}
           {:italian {:italian "camicia"}
            :english {:english "shirt"}})

    (unify agreement
           common-noun
           countable-noun
           masculine
           {:synsem {:sem (unify animal {:pred :cane :pet true})}
            :italian {:italian "cane"}
            :english {:english "dog"}})

    (unify agreement
           common-noun
           feminine
           {:synsem {:sem {:pred :cipolla
                           :edible true
                           :animate false
                           :artifact false}}
            :italian {:italian "cipolla"}
            :english {:english "onion"}})



    (unify agreement
           common-noun
           countable-noun
           masculine
           {:synsem {:sem human}}
           {:synsem {:sem {:pred :dottore}}
            :italian {:italian "dottore"}
            :english {:english "doctor"}})

    (unify agreement
           common-noun
           countable-noun
           feminine
           {:synsem {:sem human}}
           {:synsem {:sem {:pred :donna}}
            :italian {:italian "donna"}
            :english {:irregular {:plur "women"}
                      :english "woman"}})

    (unify agreement
           common-noun
           countable-noun
           masculine
           {:synsem {:sem {:pred :fiore
                           :animate false
                           :artifact false
                           :buyable true
                           :consumable false
                           :speakable false}}
            :italian {:italian "fiore"}
            :english {:english "flower"}}
           {:synsem {:subcat {:1 {:cat :det}}}})

    (unify agreement
           common-noun
           countable-noun
           masculine
           {:synsem {:sem {:legible true
                           :artifact true
                           :buyable true
                           :speakable false
                           :pred :giornale}}}
           {:italian {:italian "giornale"}
            :english {:english "newspaper"}})

    (unify agreement
           common-noun
           countable-noun
           masculine
           {:synsem {:sem (unify animal {:pred :gatto :pet true})}
            :italian {:italian "gatto"}
            :english {:english "cat"}})

    (unify agreement
           common-noun
           countable-noun
           masculine
           {:synsem {:sem (unify animal {:pred :granchi :pet false})}
            :italian {:italian "granchio"
                      :irregular {:plur "granchi"}}
            :english {:english "crab"}})

    (unify agreement
           common-noun
           countable-noun
           masculine
           {:synsem {:sem {:pred :libro
                           :legible true
                           :speakable false
                           :mass false
                           :buyable true
                           :consumable false
                           :artifact true}}
            :italian {:italian "libro"}
            :english {:english "book"}})

    (unify agreement
           common-noun
           countable-noun
           feminine
           {:synsem {:sem human}}
           {:synsem {:sem {:pred :madre}}
            :italian {:italian "madre"}
            :english {:english "mother"}})

     ;; inherently plural.
    (unify agreement
           common-noun
           feminine
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

    (unify agreement
           common-noun
           countable-noun
           feminine
           {:synsem {:sem {:legible true
                           :buyable false
                           :speakable true
                           :pred :parola}}}
           {:italian {:italian "parola"}
            :english {:english "word"}})

     ;; inherently singular.
    (unify agreement
           common-noun
           masculine
           {:synsem {:sem (sem-impl {:pred :pane
                                     :edible true
                                     :artifact true})}
            :italian {:italian "pane"}
            :english {:english "bread"}}
           {:synsem {:subcat {:1 {:cat :det
                                  :number :sing
                                  :def :def}}}})

     ;; inherently singular.
    (unify agreement
           common-noun
           feminine
           {:synsem {:sem (sem-impl {:pred :pasta
                                     :edible true
                                     :artifact true})}
            :italian {:italian "pasta"}
            :english {:english "pasta"}}
           {:synsem {:subcat {:1 {:cat :det
                                  :number :sing
                                  :def :def}}}}
           )

    (unify agreement
           common-noun
           countable-noun
           feminine
           {:synsem {:sem human}}
           {:synsem {:sem {:pred :professoressa}}}
           {:italian {:italian "professoressa"}
            :english {:english "professor"
                      :note " (&#x2640;)"}}) ;; unicode female symbol

    (unify agreement
           common-noun
           countable-noun
           masculine
           {:synsem {:sem human}}
           {:synsem {:sem {:pred :professore}}}
           {:italian {:italian "professore"}
            :english {:english "professor"
                      :note " (&#x2642;)"}}) ;; unicode male symbol

     ;; "pizza" can be either mass or countable.
    (unify agreement
           common-noun
           feminine
           {:synsem {:sem {:pred :pizza
                           :edible true
                           :artifact true}}
            :italian {:italian "pizza"}
            :english {:english "pizza"}})

     (unify agreement
            common-noun
            countable-noun
            masculine
            {:synsem {:sem human}}
            {:synsem {:sem {:pred :ragazzo}}
             :italian {:italian "ragazzo"}
             :english {:english "guy"}})

     (unify agreement
            common-noun
            countable-noun
            feminine
            {:synsem {:sem human}}
            {:synsem {:sem {:pred :ragazza}}
             :italian {:italian "ragazza"}
             :english {:english "girl"}})

     (unify agreement
            common-noun
            feminine
            countable-noun
            {:synsem {:sem {:artifact true
                            :consumable false
                            :legible false
                            :speakable false
                           :pred :scala}}
            :italian {:italian "scala"}
             :english {:english "ladder"}})

     (unify agreement
            common-noun
            feminine
            countable-noun
            {:synsem {:sem {:artifact true
                            :consumable true
                            :legible true
                            :speakable true
                            :pred :stravaganza}}
             :italian {:italian "stravaganza"}
             :english {:english "extravagant thing"}})

     (unify agreement
            common-noun
            feminine
            countable-noun
            {:synsem {:sem {:furniture true
                            :pred :sedia}}
             :italian {:italian "sedia"}
             :english {:english "chair"}})

     (unify agreement
            common-noun
            countable-noun
            masculine
            {:synsem {:sem human}}
            {:synsem {:sem {:pred :studente}}}
            {:italian {:italian "studente"}
             :english {:english "student"}})

     (unify agreement
            common-noun
            masculine
            countable-noun
           {:synsem {:sem {:furniture true
                           :pred :tavolo}}
             :italian {:italian "tavolo"}
             :english {:english "table"}})

     (unify agreement
            common-noun
            countable-noun
            masculine
            {:synsem {:sem human}}
            {:synsem {:sem {:pred :uomo}}
             :italian {:irregular {:plur "uomini"}
                       :italian "uomo"}
             :english {:irregular {:plur "men"}
                       :english "man"}})

     (unify drinkable
            agreement
            masculine
            {:italian {:italian "vino"}
             :english {:english "wine"}
            :synsem {:sem {:pred :vino
                            :artifact true}}})
     )
    )
  )

(def proper-nouns
  (let [proper-noun
        {:synsem {:cat :noun
                  :pronoun false
                  :propernoun true
                  :agr {:person :3rd}
                  :subcat '()}}]
    (list

     (unify proper-noun
            {:synsem {:sem {:pred :giorgio
                            :human true}
                      :agr {:number :sing
                            :person :3rd
                            :gender :masc}}
             :italian "Giorgio"
             :english "Giorgio"})

     (unify proper-noun
            {:synsem {:sem {:pred :milano
                            :buyable false
                            :artifact true
                            :city true}
                      :agr {:number :sing
                            :person :3rd
                            :gender :masc}}
             :italian {:italian "Milano"}
             :english {:english "Milan"}})

     (unify proper-noun
            {:synsem {:sem {:pred :napoli
                            :buyable false
                            :artifact true
                            :city true}
                      :agr {:number :sing
                            :person :3rd
                            :gender :masc}}
             :italian {:italian "Napoli"}
             :english {:english "Naples"}})

     (unify proper-noun
            {:synsem {:sem {:pred :paola
                            :human true}
                      :agr {:number :sing
                            :person :3rd
                            :gender :fem}}
             :italian "Paola"
             :english "Paola"})

     (unify proper-noun
            {:synsem {:sem {:pred :roma
                            :buyable false
                            :artifact true ;; but wasn't built in a day..
                            :city true}
                      :agr {:number :sing
                            :person :3rd
                            :gender :masc}}
                :italian {:italian "Roma"}
             :english {:english "Rome"}}))))

(def determiners
  (list

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
             :gender :masc}
    :italian "di i"
    :english "some"}

   {:synsem {:cat :det
             :def :partitivo
             :number :plur
             :gender :fem}
    :italian "di le"
    :english "some"}

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
             :number :plur}
    :italian "i tuoi"
    :english "your"}

   {:synsem {:cat :det
             :def :possessive
             :gender :masc
             :number :plur}
    :italian "i vostri"
    :english "your (pl) "}

   {:synsem {:cat :det
             :def :possessive
             :gender :masc
             :number :sing}
    :italian "il vostro"
    :english "your (pl) "}

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
             :def :def
             :gender :fem
             :number :plur}
    :italian "le"
    :english "the"}

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

   {:synsem {:cat :det
             :def :partitivo
             :number :sing
             :mass true
             :gender :masc}
    :italian "di il"
    :english "some"}

   {:synsem {:cat :det
             :def :partitivo
             :mass false
             :number :sing}
    :italian "qualche"
    :english "some"}

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


   ))


;; A generalization of intransitive and transitive:
;; they both have a subject, thus "subjective".
(def subjective
  (let [subj-sem (ref :top)
        subject-agreement (ref {:case {:not :acc}})
        infl (ref :top)
        essere-type (ref :top)]
    {:italian {:agr subject-agreement :infl infl :essere essere-type}
     :english {:agr subject-agreement :infl infl}
     :synsem {:essere essere-type
              :infl infl
              :cat :verb
              :sem {:subj subj-sem}
              :subcat {:1 {:sem subj-sem
                           :cat :noun
                           :agr subject-agreement}}}}))

;; intransitive: has subject but no object.
(def intransitive
  (unify subjective
         {:synsem {:subcat {:2 '()}}}))

;; transitive: has both subject and object.
(def transitive
  (unify subjective
         (let [obj-sem (ref :top)
               infl (ref :top)]
           {:english {:infl infl}
            :italian {:infl infl}
            :synsem {:sem {:obj obj-sem}
                     :infl infl
                     :subcat {:2 {:sem obj-sem
                                  :subcat '()
                                  :cat :noun
                                  :agr {:case :acc}}}}})))

(def transitive-but-with-adjective-instead-of-noun
  (unify subjective
         (let [obj-sem (ref :top)
               infl (ref :top)]
           {:english {:infl infl}
            :italian {:infl infl}
            :synsem {:sem {:obj obj-sem}
                     :infl infl
                     :subcat {:2 {:sem obj-sem
                                  :subcat '()
                                  :cat :adjective}}}})))

(def transitive-but-with-intensifier-instead-of-noun
  (unify subjective
         (let [obj-sem (ref :top)
               infl (ref :top)]
           {:english {:infl infl}
            :italian {:infl infl}
            :synsem {:sem {:obj obj-sem}
                     :infl infl
                     :subcat {:2 {:sem obj-sem
                                  :cat :intensifier}}}})))
(def amare
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
                   :obj {:animate true}}}}))

(def andare
   {:italian {:infinitive "andare"
              :essere true
              :irregular {:present {:1sing "vado"
                                    :2sing "vai"
                                    :3sing "va"
                                    :1plur "andiamo"
                                    :2plur "andate"
                                    :3plur "vanno"}
                          :futuro {:1sing "andrò"
                                   :2sing "andrai"
                                   :3sing "andrà"
                                   :1plur "andremo"
                                   :2plur "andrete"
                                   :3plur "andranno"}}}
    :english {:infinitive "to go"
              :irregular {:past "went"
                          :past-particle "gone"}}
    :synsem {:essere true
             :sem {:subj {:animate true}
                   :activity false ;; because "I was going when (something happened) .." sounds weird.
                   :pred :andare
                   :discrete false}}})

(def andare-intrans
  (unify
   intransitive
   andare))

(def andare-taking-pp
  (unify
   subjective
   andare
   (let [place-sem (ref {:place true
                         :pred :a})]
     {:synsem {:sem {:location place-sem}
               :subcat {:2 {:sem place-sem
                            :subcat '()
                            :cat :prep}}}})
   {:note "andare-pp"}))

(def avere-common
  {:synsem {:essere false
            :cat :verb}
   :italian {:infinitive "avere"
             :irregular {:passato "avuto"
                         :present {:1sing "ho"
                                   :2sing "hai"
                                   :3sing "ha"
                                   :1plur "abbiamo"
                                   :2plur "avete"
                                   :3plur "hanno"}}}
   :english {:infinitive "to have"
             :irregular {:past "had"
                         :present {:1sing "have"
                                   :2sing "have"
                                   :3sing "has"
                                   :1plur "have"
                                   :2plur "have"
                                   :3plur "have"}}}})
(def avere
  (unify
   transitive
   avere-common
   {:synsem {:sem {:pred :avere
                   :activity false
                   :discrete false
                   :subj {:human true}
                   :obj {:buyable true}}}}))


;; whether a verb has essere or avere as its
;; auxiliary to form its passato-prossimo form:
;; Must be encoded in both the :italian (for morphological agreement)
;; and the :synsem (for subcategorization by the appropriate aux verb).
(def aux-type
  (let [essere-binary-categorization (ref :top)
        aux (ref true)
        sem (ref {:tense :past})
        subject (ref :top)]
    {:italian {:aux aux
               :essere essere-binary-categorization}
     :synsem {:aux aux
              :sem sem
              :essere essere-binary-categorization
              :subcat {:1 subject
                       :2 {:cat :verb
                           :subcat {:1 subject
                                    :2 '()}
                           :sem sem
                           :infl :past}}}}))

(def avere-aux
  (unify
   aux-type
   subjective
   avere-common
   {:synsem {:infl :present
             :subcat {:2 {:essere false}}}
    :english {:hidden true}}))

(def bevere
  (unify
   transitive
   {:italian {:infinitive "bere"
              :irregular {:passato "bevuto"}}
    :english {:infinitive "to drink"
              :irregular {:past "drank"}}
    :synsem {:essere false
             :sem {:pred :bevere
                   :subj {:animate true}
                   :obj {:drinkable true}}}}))

(def comprare
  (unify
   transitive
   {:italian {:infinitive "comprare"}
    :english {:infinitive "to buy"
              :irregular {:past "bought"}}
    :synsem {:essere false
             :sem {:pred :comprare
                   :subj {:human true}
                   :obj {:buyable true}}}}))

(def deludere
  (unify
   transitive
   {:italian {:infinitive "deludere"
              :irregular {:passato "deluso"}}
    :english {:infinitive "to disappoint"
              :irregular {:past "disappointed"}}
    :synsem {:essere false
             :sem {:subj {:human true}
                   :obj {:human true}
                   :discrete true
                   :activity false
                   :pred :deludere}}}))

(def dormire
  (unify
   intransitive
   {:italian {:infinitive "dormire"}
    :english {:infinitive "to sleep"
              :irregular {:past "slept"}}
    :synsem {:essere false
             :sem {:subj {:animate true}
                   :discrete false
                   :pred :dormire}}}))

(def dovere
  (unify
   subjective
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
                   }}}))


(def essere-common
  (unify
   {:synsem {:essere true}
    :italian {:infinitive "essere"
              :essere true
              :irregular {:present {:1sing "sono"
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
                                   :3plur "saranno"}}}
    :english {:infinitive "to be"
              :irregular {:present {:1sing "am"
                                    :2sing "are"
                                    :3sing "is"
                                    :1plur "are"
                                    :2plur "are"
                                    :3plur "are"}
                          :past {:participle "been"
                                 :1sing "was"
                                 :2sing "were"
                                 :3sing "was"
                                 :1plur "were"
                                 :2plur "were"
                                 :3plur "were"}}}}))

(def essere-aux
  (unify
   aux-type
    subjective
    essere-common
    {:notes "essere-aux"}
    {:synsem {:infl :present
              :subcat {:2 {:essere true}}}
     :english {:infinitive "to be" ;; just for documentation purposes: never reaches surface string due to :hidden=true.
               :hidden true}})) ;; gets removed by morphological rules.

(def aux-verbs
  (list
   essere-aux
   avere-aux))


(def essere-copula
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
                     :obj {:human human}}}})))

;; TODO: combine essere-adjective and essere-intensifier
(def essere-adjective
  (let [subject (ref {:cat :noun})
        comp-sem (ref
                  {:activity false
                   :discrete false})]
    (unify
     essere-common
     {:notes "essere-adjective"}
     {:synsem {:cat :verb
               :subcat {:1 subject
                        :2 {:cat :adjective
                            :sem comp-sem
                            :subcat {:1 subject
                                     :2 '()}}}
               :sem comp-sem}})))

;; this is for e.g "essere più alto di quelle donne belle (to be taller than those beautiful women)"
(def essere-intensifier
  (let [subject (ref {:cat :noun})
        comp-sem (ref
                  {:activity false
                   :discrete false})]
    (unify
     essere-common
     {:notes "essere-intensifer"}
     {:synsem {:cat :verb
               :subcat {:1 subject
                        :2 {:cat :intensifier
                            :sem comp-sem
                            :subcat {:1 subject
                                     :2 '()}}}
               :sem comp-sem}})))

;; TODO: fare-common (factor out common stuff from fare-do and fare-make)
(def fare-do
  (unify
   transitive
   {:italian {:infinitive "fare"
              :irregular {:passato "fatto"
                          :present {:1sing "facio"
                                    :2sing "fai"
                                    :3sing "fa"
                                    :1plur "facciamo"
                                    :2plur "fate"
                                    :3plur "fanno"}
                          :imperfetto {:1sing "facevo"
                                       :2sing "facevi"
                                       :3sing "faceva"
                                       :1plur "facevamo"
                                       :2plur "facevate"
                                       :3plur "facevano"}
                          :futuro {:1sing "farò"
                                   :2sing "farai"
                                   :3sing "farà"
                                   :1plur "faremo"
                                   :2plur "farete"
                                   :3plur "faranno"}}}
    :english {:infinitive "to do"
              :irregular {:past-participle "done"
                          :present {:1sing "do"
                                    :2sing "do"
                                    :3sing "does"
                                    :1plur "do"
                                    :2plur "do"
                                    :3plur "do"}}}
    :synsem {:cat :verb
             :infl :infinitive
             :sem {:pred :fare
                   :subj {:human true}
                   :obj {:activity true}}}}))

(def fare-make
  (unify
   transitive
   ;; TODO: as with "essere", make irregular conjugations
   ;; shared between fare-do and fare-make.
   {:italian {:infinitive "fare"
              :irregular {:passato "fatto"
                          :present {:1sing "facio"
                                    :2sing "fai"
                                    :3sing "fa"
                                    :1plur "facciamo"
                                    :2plur "fate"
                                    :3plur "fanno"}
                          :imperfetto {:1sing "facevo"
                                       :2sing "facevi"
                                       :3sing "faceva"
                                       :1plur "facevamo"
                                       :2plur "facevate"
                                       :3plur "facevano"}
                          :futuro {:1sing "farò"
                                   :2sing "farai"
                                   :3sing "farà"
                                   :1plur "faremo"
                                   :2plur "farete"
                                   :3plur "faranno"}}}
    :english {:infinitive "to make"
              :irregular {:past "made"}}
    :synsem {:cat :verb
             :essere false
             :sem {:pred :fare
                   :discrete false
                   :subj {:human true}
                   :obj {:artifact true}}}}))

(def mangiare
  (unify
   transitive
   {:italian {:infinitive "mangiare"}
    :english {:infinitive "to eat"
              :irregular {:past "ate"}}
    :synsem {:essere false
             :sem {:pred :mangiare
                   :subj {:animate true}
                   :obj {:edible true}}}}))

(def leggere
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
                   :obj {:legible true}}}}))

(def parlare
  (unify
   transitive
   {:italian {:infinitive "parlare"}
    :english {:infinitive "to speak"
              :irregular {:past "spoke"}}
    :synsem {:essere false
             :sem {:pred :parlare
                   :subj {:human true}
                   :obj {:speakable true}}}}))

(def pensare
  (unify
   intransitive
   {:italian {:infinitive "pensare"}
    :english {:infinitive "to think"
              :irregular {:past "thought"}}
    :synsem {:essere false
             :sem {:pred :pensare
                   :discrete false
                   :subj {:human true}}}}))

(def potere
  (let [pred-of-complement (ref :top)]
    (unify
     subjective
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
                     :subj {:animate true}}}})))

(def recordare
  (unify
   transitive
   {:italian {:infinitive "ricordare"}
    :english {:infinitive "to remember"}
    :synsem {:essere false
             :sem {:subj {:human true}
                   :obj {:legible true}
                   :pred :recordare}}}))

(def ridere
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
                   }}}))

(def scrivere
  (unify
   transitive
   {:italian {:infinitive "scrivere"}
    :english {:infinitive "to write"
              :irregular {:past "wrote"}}
    :synsem {:essere false
             :sem {:pred :scrivere
                   :subj {:human true}
                   :obj {:legible true}}}}))

(def sognare
  (unify
   intransitive
   {:italian {:infinitive "sognare"}
    :english {:infinitive "to dream"
              :irregular {:past "dreamt"}}
    :synsem {:essere false
             :sem {:subj {:animate true}
                   :discrete false
                   :pred :sognare}}}))

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

(def vedere
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
                   :subj {:animate true}}}}))

(def vivere
  (unify
   intransitive
   {:italian {:infinitive "vivere"}
    :english {:infinitive "to live"
              :irregular {:past "lived"}}
    :synsem {:essere true
             :sem {:pred :vivere
                   ;; TODO: activity=false for now, but other senses of 'vivere' could be activities, e.g.
                   ;; adding PP (e.g. "vivevano in Milano (was living in Milano)")
                   :activity false
                   :discrete false
                   :subj {:animate true}}}})) ;; TODO: change to living-thing: (e.g. plants are living but not animate)

(def volere
  (unify
   subjective
   modal
   {:italian {:infinitive "volere"
              :irregular {:present {:1sing "voglio"
                                    :2sing "vuoi"
                                    :3sing "vuole"
                                    :1plur "vogliamo"
                                    :2plur "volete"
                                    :3plur "vogliono"}}}
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
                   :activity false
                   :discrete false
                   :subj {:animate true}}}}))

(def intransitive-verbs
  (list
   andare-intrans
   deludere
   dormire
   pensare
   ridere
   sognare
   vivere))

(def transitive-verbs
  (list
   amare
   avere
   bevere
   comprare
   essere-adjective
   essere-copula
   essere-intensifier
   fare-make
   leggere
   mangiare
   parlare
   scrivere
;   telefonare
   vedere
   ))

(def verbs-taking-pp
  (list
   andare-taking-pp))

(def modal-verbs
  (list
   dovere
   potere
   volere))

(def verbs
  (concat
   (list essere-aux
         avere-aux)
   intransitive-verbs
   transitive-verbs
   verbs-taking-pp
   modal-verbs))

(def nominative-pronouns
  (list {:synsem {:cat :fail ; :noun ;; disabling until more constraints are put on usage of it.
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

        {:synsem {:cat :noun
                  :pronoun true
                  :agr {:case :nom
                        :person :3rd
                        :number :sing}
                  :sem (unify human {:pred :ognuno})
                  :subcat '()}
         :english "everyone"
         :italian "ognuno"}


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
                   :note " (&#x2642;)"} ;; unicode female symbol
         :italian "tu"}

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
                        :gender :fem
                        :number :sing}
                  :sem (unify human {:pred :lei})
                  :subcat '()}
         :english "she"
         :italian "lei"}
        {:synsem {:cat :noun
                  :pronoun true
                  :agr {:case :nom
                        :person :1st
                        :number :plur}
                  :sem (unify human {:pred :noi})
                  :subcat '()}
         :english "we"
         :italian "noi"}
        {:synsem {:cat :noun
                  :pronoun true
                  :agr {:case :nom
                        :person :2nd
                        :number :plur}
                  :sem (unify human {:pred :voi})
                  :subcat '()}
         :italian "voi"
         :english "you all"}
        {:synsem {:cat :noun
                  :pronoun true
                  :agr {:case :nom
                        :person :3rd
                        :number :plur}
                  :sem (unify human {:pred :loro})
                  :subcat '()}
         :italian "loro"
         :english "they"}))

(def accusative-pronouns
  (let [acc (ref :acc)
        disj (ref :disj)
        noun (ref :noun)]
    (list {:synsem {:cat noun
                    :pronoun true
                    :agr {:case acc
                          :person :1st
                          :number :sing}
                    :sem (unify human {:pred :io})
                    :subcat '()}
           :english "me"
           :italian {:italian "mi"
                     :pronoun true
                     :cat noun
                     :case acc}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case acc
                          :person :2nd
                          :number :sing}
                    :sem (unify human {:pred :tu})
                    :subcat '()}
           :english "you"
           :italian {:italian "ti"
                     :cat noun
                     :case acc}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case acc
                          :gender :masc
                          :person :3rd
                          :number :sing}
                    :sem (unify human {:pred :lo})
                    :subcat '()}
           :english "him"
           :italian {:italian "lo"
                     :pronoun true
                     :cat noun
                     :case acc}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case acc
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
                     :pronoun true
                     :cat noun
                     :case acc}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case acc
                          :gender :fem
                          :person :3rd
                          :number :sing}
                    :sem (unify human {:pred :lei})
                    :subcat '()}
           :english "her"
           :italian {:italian "la"
                     :cat noun
                     :case acc}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case acc
                          :gender :fem
                          :person :3rd
                          :number :sing}
                    :sem {:human false
                          :place false ;; "they go to it (loro vanna a la)" sounds strange
                          :pred :lei}
                    :subcat '()}
           :english {:english "it"
                     :note " (&#x2640;)"} ;; unicode female symbol
           :italian {:italian "la"
                     :cat noun
                     :case acc}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case acc
                          :person :1st
                          :number :plur}
                    :sem (unify human {:pred :noi})
                    :subcat '()}
           :english "us"
           :italian {:italian "ci"
                     :cat noun
                     :case acc}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case acc
                          :person :2nd
                          :number :plur}
                    :sem (unify human {:pred :voi})
                    :subcat '()}
           :english "you all"
           :italian {:italian "vi"
                     :cat noun
                     :case acc}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case acc
                          :gender :masc
                          :person :3rd
                          :number :plur}
                    :sem (unify human {:pred :lui})
                    :subcat '()}
           :english {:english "them"
                     :note " (&#x2642;) "} ;; unicode male
           :italian {:italian "li"
                     :cat noun
                     :case acc}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case acc
                          :gender :fem
                          :person :3rd
                          :number :plur}
                    :sem (unify human {:pred :lei})
                    :subcat '()}
           :english {:english "them"
                     :note " (&#x2640;) "}
           :italian {:italian "le"
                     :cat noun
                     :case acc}}

          )))

(def disjunctive-pronouns
  (let [disj (ref :disj)
        noun (ref :noun)]

    (list {:synsem {:cat noun
                    :pronoun true
                    :agr {:case disj
                          :person :1st
                          :number :sing}
                    :sem (unify human {:pred :io})
                    :subcat '()}
           :english "me"
           :italian {:italian "me"
                     :pronoun true
                     :cat noun
                     :case disj}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case disj
                          :person :2nd
                          :polite false
                          :number :sing}
                    :sem (unify human {:pred :tu})
                    :subcat '()}
           :english "you"
           :italian {:italian "te"
                     :cat noun
                     :case disj}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case disj
                          :gender :masc
                          :person :3rd
                          :number :sing}
                    :sem (unify human {:pred :lui})
                    :subcat '()}
           :english "him"
           :italian {:italian "lui"
                     :cat noun
                     :case disj}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case disj
                          :gender :fem
                          :person :2nd
                          :polite true
                          :number :sing}
                    :sem (unify human {:pred :lei})
                    :subcat '()}
           :english "her"
           :italian {:italian "lei"
                     :cat noun
                     :case disj}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case disj
                          :gender :fem
                          :person :3rd
                          :number :sing}
                    :sem (unify human {:pred :lei})
                    :subcat '()}
           :english "her"
           :italian {:italian "lei"
                     :cat noun
                     :case disj}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case disj
                          :person :3rd
                          :number :sing}
                    :sem (unify {:human false
                                 :place false
                                 :pred :esso})
                    :subcat '()}
           :english "it"
           :italian {:italian "esso"
                     :cat noun
                     :case disj}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case disj
                          :person :1st
                          :number :plur}
                    :sem (unify human {:pred :noi})
                    :subcat '()}
           :english "us"
           :italian {:italian "noi"
                     :pronoun true
                     :cat noun
                     :case disj}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case disj
                          :person :2nd
                          :number :plur}
                    :sem (unify human {:pred :voi})
                    :subcat '()}
           :english "you all"
           :italian {:italian "voi"
                     :cat noun
                     :case disj}}

          ;; note: no gender: "loro" in either case of masc or fem.
          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case disj
                          :person :3rd
                          :number :plur}
                    :sem (unify human {:pred :lui})
                    :subcat '()}
           :english "them"
           :italian {:italian "loro"
                     :cat noun
                     :case disj}}

          ;; non human masculine and feminine forms
          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case disj
                          :person :3rd
                          :gender :masc
                          :number :plur}
                    :sem {:human false
                          :place false
                          :pred :essi}
                    :subcat '()}
           :english "them"
           :italian {:italian "essi"
                     :cat noun
                     :case disj}}

          {:synsem {:cat noun
                    :pronoun true
                    :agr {:case disj
                          :person :3rd
                          :gender :fem
                          :number :plur}
                    :sem {:human false
                          :place false
                          :pred :essi}
                    :subcat '()}
           :english "them"
           :italian {:italian "esse"
                     :cat noun
                     :case disj}}

          )))

(def prepositions
  (list {:synsem {:cat :prep
                  :sem {:pred :a
                        :comparative false}
                  :subcat {:1 {:cat :noun
                               :sem {:place true}}}}
         :italian "a"
         :english "to"}

        ;; using di (1,2) to debug:
        ;; * Questo professore è meno difficile di lo (This professor (♂) is less difficult than him).
        ;; (should be lui, not lo).
        {:synsem {:cat :prep
                  :sem {:pred :di
                        :comparative true}
                  :subcat {:1 {:cat :noun
                               :def {:not :partitivo} ;; avoid alliteration like "di delle ragazze (of some women)"
                               :agr {:case :disj} ;; pronouns must be disjunctive (me/te/lui/lei...etc)
                               ;; non-pronouns will unify with this constraint.

                               ;; TODO: remove this constraint: for debugging only.
                               :sem {:human true}}}}
         :italian "di"
         :english "than"}

;        {:synsem {:cat :prep
;                  :sem {:pred :in}
;                  :subcat {:1 {:cat :noun
;                               :sem {:city true}}}}
;         ;; this overrides the prep-phrase's extends, which are too general
;         ;; for this lexical entry "a"/"in".
;         :extend {:prep-phrase {:a {:head :prepositions
;                                    :comp :proper-nouns}}}
;         :italian "a"
;         :english "in"}
        ))

(def intensifiers
  (list
   (let [human (ref :top)
         animate (ref :top)]
     {:synsem {:sem {:human human
                     :animate animate}
               :cat :intensifier
               :subcat {:1 {:cat :adjective
                            :sem {:comparative true
                                  :human human
                                  :animate animate}}}}



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
      })))

  ;; TODO: cut down duplication in here (i.e. :italian :cat, :english :cat, etc)
  ;; (this is being accomplished below: see TODO below about "copy all the below adjectives.."
(def adjectives
  (let [adjective (ref :adjective)
        gender (ref :top)
        number (ref :top)
        adj {:synsem {:cat adjective
                      :agr {:gender gender
                            :number number}
                      }
             :italian {:cat adjective
                       :agr {:number number
                             :gender gender}}
             :english {:cat adjective}}]
    (map (fn [entry]
           (unify adj entry))
         (list
          ;; non-comparative:
          {:synsem {:cat :adjective
                    :sem {:pred :alto
                          :comparative false
                           :mod {:human true}}}
           :italian {:italian "alto"}
           :english {:english "tall"}}

          ;; comparative:
          (let [complement-complement-sem (ref {:human true}) ;; only humans can be tall.
                complement-sem (ref {:pred :di
                                     :mod complement-complement-sem})
                subject-sem (ref {:human true})] ;; only humans can be tall.
            {:synsem {:sem {:pred :alto
                            :comparative true
                            :arg1 subject-sem
                            :arg2 complement-complement-sem}
                      :subcat {:1 {:cat :noun
                                   :sem subject-sem}
                               :2 {:cat :prep
                                   :sem complement-sem}}}
             :italian {:italian "alto"}
             :english {:english "tall"}})

          ;; non-comparative
          ;; TODO: add comparative
          {:synsem {:sem {:pred :bello
                          :comparative false
                          :mod :top}} ;; for now, no restrictions on what can be beautiful.
           :italian {:italian "bello"}
           :english {:english "beautiful"}}

          ;; non-comparative
          ;; TODO: add comparative
          {:synsem {:cat :adjective
                    :sem {:pred :brutto
                          :comparative false
                          :mod :top}} ;; for now, no restrictions on what can be ugly.
           :italian {:italian "brutto"
                     :cat :adjective}
           :english {:english "ugly"
                     :cat :adjective}}

          ;; non-comparative
          ;; TODO: add comparative
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
                     :cat :adjective}}

           (let [complement-complement-sem (ref {:human true}) ;; only humans can be short.
                 complement-sem (ref {:pred :di
                                      :mod complement-complement-sem})
                 subject-sem (ref {:human true})] ;; only humans can be short.
             {:synsem {:sem {:pred :corto
                             :comparative true
                             :arg1 subject-sem
                             :arg2 complement-complement-sem}
                       :subcat {:1 {:cat :noun
                                    :sem subject-sem}
                                :2 {:cat :prep
                                    :sem complement-sem}}}
              :italian {:italian "corto"}
              :english {:english "short"}})

           ;; non-comparative
           {:synsem {:cat :adjective
                     :sem {:pred :corto
                           :comparative false
                           :mod {:human true}}}
            :italian {:italian "corto"
                      :cat :adjective}
            :english {:english "short"
                      :cat :adjective}}

           ;; non-comparative
           ;; TODO: add comparative
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
                      :cat :adjective}}

           ;; non-comparative
           ;; TODO: add comparative
           {:synsem {:sem {:pred :gentile
                           :comparative false
                           :mod {:human true}}} ;; sono gli umani possono essere gentile.
            :italian {:italian "gentile"}
            :english {:english "kind"}}

           ;; non-comparative
           ;; TODO: add comparative
           {:synsem {:cat :adjective
                     :sem {:pred :nero
                           :comparative false
                           :mod {:physical-object true
                                 :human false}}}
            :italian {:italian "nero"
                      :cat :adjective}
            :english {:english "black"
                      :cat :adjective}}

           ;; non-comparative
           ;; TODO: add comparative
           {:synsem {:cat :adjective
                     :sem {:pred :piccolo
                           :comparative false
                           :mod {:physical-object true
                                 :mass false}}}
            :italian {:italian "piccolo"
                      :cat :adjective}
            :english {:english "small"
                      :cat :adjective}}

           ;; non-comparative
           (unify
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
             {:synsem {:sem {:pred :ricco
                             :comparative true
                             :arg1 subject-sem
                             :arg2 complement-complement-sem}
                       :subcat {:1 {:cat :noun
                                    :sem subject-sem}
                                :2 {:cat :prep
                                    :sem complement-sem}}}
              :italian {:italian "ricco"}
              :english {:english "rich"}})

           ;; non-comparative
           {:synsem {:cat :adjective
                     :sem {:pred :robusto
                           :comparative false
                           :activity false
                           :mod {:animate true}}}
            :italian {:italian "robusto"
                      :cat :adjective}
            :english {:english "large-built"
                      :cat :adjective}}

           {:synsem {:cat :adjective
                     :sem {:pred :rosso
                           :comparative false
                           :mod {:physical-object true
                                 :human false}}}
            :italian {:italian "rosso"
                        :cat :adjective}
            :english {:english "red"
                      :cat :adjective}}

           {:synsem {:cat :adjective
                     :sem {:pred :rumorosa
                           :comparative false
                           :mod {:animate true}}}
            :italian {:italian "rumoroso"
                      :cat :adjective}
            :english {:english "noisy"
                      :cat :adjective}}

           ;; comparative:
           (let [complement-complement-sem (ref {:human true}) ;; only animals can be noisy.
                 complement-sem (ref {:pred :di
                                      :mod complement-complement-sem})
                 subject-sem (ref {:animate true})] ;; only animals can be noisy.
             {:synsem {:sem {:pred :semplice
                             :comparative true
                             :arg1 subject-sem
                             :arg2 complement-complement-sem}
                       :subcat {:1 {:cat :noun
                                    :sem subject-sem}
                                :2 {:cat :prep
                                    :sem complement-sem}}}
              :italian {:italian "rumoroso"}
              :english {:english "noisy"}})


           ;; comparative:
           (let [complement-complement-sem (ref {:human true}) ;; only humans can be naive.
                 complement-sem (ref {:pred :di
                                      :mod complement-complement-sem})
                 subject-sem (ref {:human true})] ;; only humans can be naive.
             {:synsem {:sem {:pred :semplice
                             :comparative true
                             :arg1 subject-sem
                             :arg2 complement-complement-sem}
                       :subcat {:1 {:cat :noun
                                    :sem subject-sem}
                                :2 {:cat :prep
                                    :sem complement-sem}}}
              :italian {:italian "semplice"}
              :english {:english "naive"}})

           ;; non-comparative:
           {:synsem {:cat :adjective
                     :sem {:pred :semplice
                           :comparative false
                           :mod {:human true}}}
            :italian {:italian "semplice"
                      :cat :adjective}
            :english {:english "naive"
                      :cat :adjective}}))))

(def lookup-in
  "find all members of the collection that matches with query successfully."
  (fn [query collection]
    (loop [coll collection matches nil]
      (let [first-val (first coll)]
        (if (nil? first-val)
          matches
          (let [result (fs/match (fs/copy query) (fs/copy first-val))]
            (if (not (fs/fail? result))
              (recur (rest coll)
                     (cons first-val matches))
              (recur (rest coll)
                     matches))))))))

(declare lexicon)

(defn lookup [query]
  (lookup-in query lexicon))

(defn it [italian]
  (let [result
        (set/union (set (lookup {:italian italian}))
                   (set (lookup {:italian {:infinitive italian}}))
                   (set (lookup {:italian {:infinitive {:infinitive italian}}}))
                   (set (lookup {:italian {:italian italian}}))
                   (set (lookup {:italian {:irregular {:passato italian}}})))]
    (if (= (.size result) 1)
      (first result) ;; simply return first member rather than singleton-set:
      ;; makes it easier to work with by end-users using with generate/* functions.
      result)))

(defn en [english]
  (lookup {:english english}))

(def quando
  {:english "when"
   :italian "quando"
   :synsem {:cat :quantifier
            :sem {:pred :quando}
            :subcat {:1 {:cat :verb
                         :infl :imperfetto
                         :sem {:activity true}}
                     :2 {:cat :verb
                         :infl :present
                         :sem {:aspect :passato
                               :discrete true
                               :tense :past}}}}})

(def lexicon
  (let [noun
        (let [gender (ref :top)
              ;; common nouns are underspecified for number: number selection (:sing or :plur) is deferred until later.
              ;; (except for mass nouns which are only singular)
              number (ref :top)

              ;; common nouns are neither nominative or accusative. setting their case to :top allows them to (fs/match) with
              ;; verbs' case specifications like {:case {:not :acc}} or {:case {:not :nom}}.
              case (ref :top)

              person (ref :top)

              agreement
              (let [number (ref :top)
                    gender (ref :top)
                    person (ref :top)
                    agr (ref {:number number
                              :gender gender
                              :case :top
                              :person person})
                      cat (ref :top)]
                {:synsem {:cat cat
                          :subcat {:1 {:number number
                                       :person person
                                       :gender gender}}
                          :agr agr}
                 :italian {:cat cat
                           :agr agr}
                 :english {:cat cat
                           :agr agr}})
              common
              (unify
               {:synsem {:cat :noun
                         :agr {:person :3rd}
                         :subcat {:1 {:cat :det}}}})

              masculine {:synsem {:agr {:gender :masc}}}
              feminine {:synsem {:agr {:gender :fem}}}

              mass
              (let [mass (ref true)]
                {:synsem {:subcat {:1 {:cat :det
                                       :mass mass
                                       :number :sing}}
                          :sem {:mass mass}}})

              countable
              (let [mass (ref false)]
                {:synsem {:subcat {:1 {:cat :det
                                       :mass mass}}
                          :sem {:mass mass}}})

              drinkable
              (unify mass
                     common
                     {:synsem {:sem {:number :sing
                                     :drinkable true}}})]
          {:agreement agreement
           :common common
           :countable countable
           :feminine feminine
           :masculine masculine})

        verb {:transitive transitive}
        ]

    (concat
     (list
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

      ;; melanzana
      (unify (:agreement noun)
             (:common noun)
             (:countable noun)
             (:feminine noun)
             {:synsem {:sem {:pred :cipolla
                             :edible true
                             :animate false
                             :artifact false}}
              :italian {:italian "melanzana"}
              :english {:english "eggplant"}})

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
                       :discrete true
                       :subj {:human true}
                       :obj {:buyable true}}}})

      ;; stradale
      (unify (:agreement noun)
             (:common noun)
             (:countable noun)
             (:masculine noun)
             {:synsem {:sem {:pred :stradale
                             :buyable false ;; a road's too big to own.
                             :artifact true
                             :city false
                             :place true}}
              :italian {:italian "stradale"}
              :english {:english "road"}})
           )
     adjectives intensifiers determiners
     nouns proper-nouns prepositions
     nominative-pronouns accusative-pronouns disjunctive-pronouns
     verbs

     (list quando))))

                                        ;(def tinylex (list (it "Napoli") (it "lui") (it "pensare")))
                                        ;(def tinylex (list (it "Napoli"))); (it "lui"))); (it "pensare")))
                                        ;(def tinylex (list (it "Napoli") (it "pensare") (it "Roma") (it "sognare") (it "dormire") (it "tavolo") (it "gatto") (it "lui") (it "lei")))
                                        ;(def tinylex (list (it "Napoli") (it "Roma") (it "io")))
(def tinylex (list (it "gatto") (it "uomo")))


;(def nouns (list (first (it "professoressa"))))
;(def adjectives (list (first (it "piccolo"))))

(map (fn [lexeme]
       (let [italian (:italian lexeme)
             english (:english lexeme)]
         (add italian english
              lexeme)))
     lexicon)

(def adjs (filter (fn [lex] (= (fs/get-in lex '(:synsem :cat)) :adjective)) lexicon))
(def nouns (filter (fn [lex] (= (fs/get-in lex '(:synsem :cat)) :noun)) lexicon))
(def common-nouns (filter (fn [lex] (and (= (fs/get-in lex '(:synsem :cat)) :noun)
                                         (= (fs/get-in lex '(:synsem :subcat :1 :cat)) :det)))
                            lexicon))
(def dets (filter (fn [lex] (= (fs/get-in lex '(:synsem :cat)) :determiner)) lexicon))

(def animate-nouns
  (filter (fn [lex]
            (let [sem (fs/get-in lex '(:synsem :sem))
                  sem-impl (sem-impl sem)]
              (= (fs/get-in sem-impl '(:animate)) true)))
          nouns))

(def human-nouns
  (filter (fn [lex]
            (let [sem (fs/get-in lex '(:synsem :sem))
                  sem-impl (sem-impl sem)]
              (= (fs/get-in sem-impl '(:human)) true)))
          nouns))

