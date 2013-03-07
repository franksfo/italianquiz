(ns italianverbs.lexicon
  (:use [italianverbs.lexiconfn]
        [clojure.test])
  (:require
   [italianverbs.fs :as fs]
   [clojure.set :as set]))

;; WARNING: clear blows away entire lexicon in backing store (mongodb).
(clear!)

;; useful abbreviations (aliases for some commonly-used maps):
;; TODO: combine with abbreviations inside the (def nouns).
(def human {:human true})
(def animal {:animate true :human false})
(def food {:synsem {:sem {:edible true}}})
(def infinitive
  {:synsem {:cat :verb
            :infl :infinitive}})

(defn sem-impl [input]
  "expand input feature structures with semantic (really cultural) implicatures, e.g., if human, then not buyable or edible"
  (cond
   (= input :top) input
   true
   (let [animate (if (= (fs/get-in input '(:animate))
                        true)
                   {:artifact false
                    :mass false
                    :physical-object true
                    :drinkable false
                    :speakable false
                    :place false}{})
         artifact (if (= (fs/get-in input '(:artifact))
                         true)
                    {:animate false
                     :physical-object true}{})

         consumable-false (if (= (fs/get-in input '(:consumable)) false)
                            {:drinkable false
                             :edible false} {})

         drinkable
         ;; drinkables are always mass nouns.
         (if (= (fs/get-in input '(:drinkable)) true)
           {:mass true}{})
         
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
                   :speakable false
                   :legible false}{})
         human (if (= (fs/get-in input '(:human))
                      true)
                 {:buyable false
                  :physical-object true
                  :edible false
                  :animate true
                  :drinkable false}{})
         inanimate (if (= (fs/get-in input '(:animate))
                           false)
                      {:human false}{})

         ;; legible(x) => artifact(x),drinkable(x,false),edible(x,false),human(x,false)
         legible
         (if (= (fs/get-in input '(:legible)) true)
           {:artifact true
            :drinkable false
            :human false
            :edible false})

         ;; artifact(x,false) => legible(x,false)
         not-legible-if-not-artifact
         (if (= (fs/get-in input '(:artifact)) false)
           {:legible false})

         ;; we don't eat pets (unless things get so desperate that they aren't pets anymore)
         pets (if (= (fs/get-in input '(:pet))
                     true)
                {:edible false
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
     (let [merged (fs/merge animate artifact consumable-false drinkable
                            drinkable-xor-edible-1 drinkable-xor-edible-2
                            edible human inanimate
                            legible not-legible-if-not-artifact pets place
                            input
                            )]
       (if (not (= merged input))
         (sem-impl merged) ;; we've added some new information: more implications possible from that.
         merged))))) ;; no more implications: return

(def noun-conjugator
  (let [italian-root (ref :top)
        english-root (ref :top)
        synsem (ref :top)
        agr (ref :top)]
    (unify
     {:root {:synsem synsem}
      :synsem synsem}
     {:root
      {:italian italian-root
       :english english-root
       :synsem {:agr agr}}
      :italian {:root italian-root
                :cat :noun
                :agr agr}
      :english {:root english-root
                :agr agr}
      :synsem {:agr agr}})))

;; noun-conjugator (above) is deprecated:
;; <replace with>:
(def noun-conjugator-new
  (let [italian-root (ref :top)
        english-root (ref :top)
        synsem (ref :top)
        agr (ref :top)]
    (unify
     {:root {:synsem synsem}
      :synsem synsem}
     {:root
      {:italian italian-root
       :english english-root
       :synsem {:agr agr}}
      :italian {:italian italian-root
                :agr agr
                :cat :noun}
      :english {:english english-root
                :agr agr}
      :synsem {:agr agr}})))

(def nouns
  (let [gender (ref :top)

        ;; common nouns are underspecified for number: number selection (:sing or :plur) is deferred until later.
        ;; (except for mass nouns which are only singular)
        number (ref :top)

        ;; common nouns are neither nominative or accusative. setting their case to :top allows them to (fs/match) with
        ;; verbs' case specifications like {:case {:not :acc}} or {:case {:not :nom}}.
        case (ref :top)

        person (ref :top)
        agreement {:synsem {:agr {:person person
                                  :number number
                                  :case case
                                  :gender gender}
                            :subcat {:1 {:gender gender
                                         :number number}}}}
        common-noun
        (unify
;         {:italian {:cat :noun}}
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

        ;; TODO: this abbreviation (drinkable) is starting to get into the same
        ;; realm as (sem-impl). Combine the two.
        drinkable
        (unify noun-conjugator
               mass-noun
               {:root (unify agreement
                             common-noun
                             {:synsem {:sem {:number :sing
                                             :drinkable true}}})})
        ]
    (list


     (unify drinkable
            feminine
            {:root {:italian "acqua"
                    :english "water"
                    :synsem {:sem {:artifact false
                                   :animate false
                                   :pred :acqua}}}})

     (unify drinkable
            feminine
            {:root {:italian "birra"
                    :english "beer"
                    :synsem {:sem {:pred :birra
                                   :artifact true}}}})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          countable-noun
                          masculine
                          {:synsem {:sem {:pred :amico
                                          :human true}}
                           :italian "amico"
                           :english "friend"})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          countable-noun
                          masculine
                          {:synsem {:sem {:pred :compito
                                          :legible true
                                          :speakable false
                                          :buyable false
                                          :artifact true
                                          :activity true}}
                           :italian "compito"
                           :english "homework assignment"})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          countable-noun
                          masculine
                          {:synsem {:sem {:pred :mare
                                          :buyable false ;; a seaside's too big to own.
                                          :artifact false
                                          :place true}}
                           :italian "mare"
                           :english "seaside"}
                          {:synsem {:subcat {:1 {:cat :det
                                                 :number :sing
                                                 :def :def}}}})})

     
     ;; inherently singular.
     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          masculine
                          {:synsem {:sem (sem-impl {:pred :pane
                                                    :edible true
                                                    :artifact true})}
                           :italian "pane"
                           :english "bread"}
                          {:synsem {:subcat {:1 {:cat :det
                                                 :number :sing
                                                 :def :def}}}})})

     ;; inherently singular.
     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          feminine
                          food
                          {:synsem {:sem {:pred :pasta
                                          :artifact true}}
                           :italian "pasta"
                           :english "pasta"}
                          {:synsem {:subcat {:1 {:cat :det
                                                 :number :sing
                                                 :def :def}}}})})
     (unify noun-conjugator-new
            {:root (unify agreement
                          common-noun
                          countable-noun
                          feminine
                          {:synsem {:sem {:pred :camicia
                                          :artifact true
                                          :speakable false
                                          :legible false ;; (exception: tshirts with writing on them)
                                          :consumable false
                                          :clothing true}}}
                          {:italian "camicia"
                           :english "shirt"})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          countable-noun
                          masculine
                          {:synsem {:sem (unify animal {:pred :cane :pet true})}
                           :italian "cane"
                           :english "dog"})})


     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          countable-noun
                          masculine
                          {:synsem {:sem human}}
                          {:synsem {:sem {:pred :dottore}}
                           :italian "dottore"
                           :english "doctor"})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          countable-noun
                          feminine
                          {:synsem {:sem human}}
                          {:synsem {:sem {:pred :donna}}
                           :italian "donna"
                           :english {:irregular {:plur "women"}
                                     :english "woman"}})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          countable-noun
                          masculine
                          {:synsem {:sem {:pred :fiore
                                          :animate false
                                          :artifact false
                                          :buyable true
                                          :consumable false
                                          :speakable false}}
                           :italian "fiore"
                           :english "flower"}
                          {:synsem {:subcat {:1 {:cat :det}}}})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          countable-noun
                          masculine
                          {:synsem {:sem (unify animal {:pred :gatto :pet true})}
                           :italian "gatto"
                           :english "cat"})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          countable-noun
                          masculine
                          {:synsem {:sem {:pred :libro
                                          :legible true
                                          :speakable false
                                          :mass false
                                          :consumable false
                                          :artifact true}}
                           :italian "libro"
                           :english "book"})})

     ;; inherently plural.
     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          feminine
                          mass-noun
                          {:synsem {:sem {:pred :notizie
                                          :buyable false
                                          :legible true}}
                           ;; "notizia" would work also: would be pluralized by (morphology/conjugate-it) to "notizie".
                           :italian "notizie"
                           :english "new"} ;; "news" (will be pluralized by (morphology/conjugate-en) to "news".
                          {:synsem {:subcat {:1 {:cat :det
                                                 :number :plur
                                                 :def :def}}}})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          countable-noun
                          feminine
                          {:synsem {:sem {:legible true
                                          :speakable true
                                          :pred :parola}}}
                          {:italian "parola"
                           :english "word"})})

     (unify noun-conjugator-new
            {:root (unify agreement
                          common-noun
                          countable-noun
                          feminine
                          {:synsem {:sem human}}
                          {:synsem {:sem {:pred :professoressa}}}
                          {:italian "professoressa"
                           :english {:english "professor"
                                     :note " (&#x2640;) "}})}) ;; unicode female symbol

     (unify noun-conjugator-new
            {:root (unify agreement
                          common-noun
                          countable-noun
                          masculine
                          {:synsem {:sem human}}
                          {:synsem {:sem {:pred :professore}}}
                          {:italian "professore"
                           :english {:english "professor"
                                     :note " (&#x2642;) "}})}) ;; unicode male symbol

     
     ;; "pizza" can be either mass or countable.
     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          feminine
                          food
                          {:synsem {:sem {:pred :pizza
                                          :artifact true}}
                           :italian "pizza"
                           :english "pizza"})})
     
     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          feminine
                          countable-noun
                          {:synsem {:sem {:artifact true
                                          :consumable false
                                          :legible false
                                          :speakable false
                                          :pred :scala}}
                           :italian "scala"
                           :english "ladder"})})

     (unify noun-conjugator-new
            {:root (unify agreement
                          common-noun
                          countable-noun
                          masculine
                          {:synsem {:sem human}}
                          {:synsem {:sem {:pred :ragazzo}}
                           :italian "ragazzo"
                           :english "guy"})})



     (unify noun-conjugator-new
            {:root (unify agreement
                          common-noun
                          countable-noun
                          feminine
                          {:synsem {:sem human}}
                          {:synsem {:sem {:pred :ragazza}}}
                          {:italian "ragazza"
                           :english "girl"})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          countable-noun
                          masculine
                          {:synsem {:sem human}}
                          {:synsem {:sem {:pred :studente}}}
                          {:italian "studente"
                           :english "student"})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          countable-noun
                          masculine
                          {:synsem {:sem human}}
                          {:synsem {:sem {:pred :uomo}}
                           :italian {:irregular {:plur "uomini"}
                                     :italian "uomo"}
                           :english {:irregular {:plur "men"}
                                     :english "man"}})})

     (unify drinkable
            masculine
            {:root {:italian "vino"
                    :english "wine"
                    :synsem {:sem {:pred :vino
                                   :artifact true}}}})
)))

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
             :def :def
             :gender :masc
             :number :sing}
    :italian "il"
    :english "the"}

   {:synsem {:cat :det
             :def :def
             :gender :fem
             :number :sing}
    :italian "la"
    :english "the"}

   {:synsem {:cat :det
             :def :def
             :gender :fem
             :number :plur}
    :italian "le"
    :english "the"}

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
  (let [subj-sem (ref :top)]
    {:synsem {:sem {:subj subj-sem}
              :subcat {:1 {:sem subj-sem
                           :cat :noun
                           :agr {:cat {:not :acc}}}}}}))

;; intransitive: has subject but no object.
(def intransitive
  (unify subjective
         {:synsem
          {:subcat {:2 '()}}}))

;; intransitive: has both subject and object.
(def transitive
  (unify subjective
         (let [obj-sem (ref :top)]
           {:synsem {:sem {:obj obj-sem}
                     :subcat {:2 {:sem obj-sem
                                  :cat :noun
                                  :agr {:case {:not :nom}}}}}})))

;; TODO add subcat frames (<NP,PP>)
(def andare-intrans
  (unify
   intransitive
   infinitive
   {:italian {:infinitive "andare"
              :essere true
              :irregular {:present {:1sing "vado"
                                    :2sing "vai"
                                    :3sing "va"
                                    :1plur "andiamo"
                                    :2plur "andate"
                                    :3plur "vanno"}}}
    :english {:infinitive "to go"
              :irregular {:past "went"}}
    :synsem {:sem {:subj {:animate true}
                   :pred {:pred :andare
                          :essere true}}}}))

(def avere-common
  {:synsem {:cat :verb}
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
   infinitive
   avere-common
   {:synsem {:infl :infinitive
             :sem {:pred :avere
                   :subj {:human true}
                   :obj {:buyable true}}}}))

(def avere-aux
  (let [v-past-pred (ref :top)
        subject (ref :top)]
    (unify
     subjective
     infinitive
     avere-common
     {:synsem {:subcat {:1 subject
                        :2 {:cat :verb
                            :subcat {:1 subject}
                            :sem {:pred v-past-pred}
                            :infl :past}}
               :sem {:pred v-past-pred}}}
     {:synsem {:subcat {:2 {:sem {:pred {:essere false}}}}}})))

;; TODO: not sure if we need this: avere (to have) is not usually intransitive.
(def avere-aux-intrans
  (unify
   (fs/copy infinitive)
   (fs/copy avere-aux)
   {:synsem {:subcat {:2 {:subcat {:2 '()}}}}}))

(def avere-aux-trans
  (let [v-past-pred (ref :top)
        subject (ref :top)
        subject-sem (ref :top) ;; todo: subject-sem is not contained within subject: move to avere-aux (above)
        object-sem (ref :top)]
    (unify
     (fs/copy infinitive)
     (fs/copy avere-aux)
     {:synsem {:subcat {:2 {:subcat {:2 :top}}}}})))

(def bevere
  (unify
   transitive
   infinitive
   {:italian {:infinitive "bevere"
              :irregular {:passato "bevuto"}}
    :english {:infinitive "to drink"
              :irregular {:past "drank"}}
    :synsem {:sem {:pred :bevere

                   :subj (sem-impl {:animate true})
                   :obj (sem-impl {:drinkable true})}}}))


(def comprare
  (unify
   transitive
   infinitive
   {:italian "comprare"
    :english {:infinitive "to buy"
              :irregular {:past "bought"}}
    :synsem {:sem {:pred :comprare
                   :subj {:human true}
                   :obj {:buyable true}}}}))

(def dormire
  (unify
   intransitive
   infinitive
   {:italian "dormire"
    :english {:infinitive "to sleep"
              :irregular {:past "slept"}}
    :synsem {:sem {:subj {:animate true}
                   :pred {:pred :dormire
                          :essere false}}}}))

(def essere-common
  (unify
   infinitive
   {:italian {:infinitive "essere"
              :essere true
              :irregular {:present {:1sing "sono"
                                    :2sing "sei"
                                    :3sing "è"
                                    :1plur "siamo"
                                    :2plur "siete"
                                    :3plur "sono"}
                          :passato "stato"}}
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

(def essere-trans
  (unify
   transitive
   infinitive
   essere-common
   {:synsem {:sem {:pred {:pred :essere
                          :essere true}}}}))

(def essere-aux
  (let [v-past-pred (ref :top)
        subject (ref :top)]
    (unify
     subjective
     infinitive
     essere-common
     {:synsem {:subcat {:1 subject
                        :2 {:cat :verb
                            :subcat {:1 subject}
                            :sem {:pred v-past-pred}
                            :infl :past}}
               :sem {:pred v-past-pred}}}
     {:synsem {:subcat {:2 {:sem {:pred {:essere true}}}}}})))

(def essere-aux-intrans
  (unify
   (fs/copy infinitive)
   (fs/copy essere-aux)
   {:synsem {:subcat {:2 {:subcat {:2 '()}}}}}))

(def essere-aux-trans
  (unify
   (fs/copy infinitive)
   (fs/copy transitive)
   (fs/copy essere-aux)))

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
                                    :3plur "fanno"}}}
    :english {:infinitive "to do"
              :irregular {:present {:1sing "do"
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
   {:italian {:infinitive "fare"
              :irregular {:passato "fatto"
                          :present {:1sing "facio"
                                    :2sing "fai"
                                    :3sing "fa"
                                    :1plur "facciamo"
                                    :2plur "fate"
                                    :3plur "fanno"}}}
    :english {:infinitive "to make"
              :irregular {:past "made"}}
    :synsem {:cat :verb
             :infl :infinitive
             :sem {:pred :fare
                   :subj (sem-impl {:human true})
                   :obj (sem-impl {:artifact true})}}}))

(def mangiare
  (unify
   transitive
   infinitive
   {:italian "mangiare"
    :english "to eat"
    :synsem {:sem {:pred {:pred :mangiare
                          :essere false}
                   :subj (sem-impl {:animate true})
                   :obj (sem-impl {:edible true})}}}))

(def leggere
  (unify
   transitive
   infinitive
   {:italian {:infinitive "leggere"
              :irregular {:passato "letto"}}
    :english {:infinitive "to read" ;; spelled "read" but pronounced like "reed".
              :irregular {:past "read"}} ;; spelled "read" but pronounced like "red".
    :synsem {:sem {:pred :leggere
                   :subj {:human true}
                   :obj {:legible true}}}}))


(def parlare
  (unify
   transitive
   infinitive
   {:italian {:infinitive "parlare"}
    :english {:infinitive "to speak" 
              :irregular {:past "spoken"}}
    :synsem {:sem {:pred :parlare
                   :subj {:human true}
                   :obj {:speakable true}}}}))

(def pensare
  (unify
   intransitive
   infinitive
   {:italian "pensare"
    :english {:infinitive "to think"
              :irregular {:past "thought"}}
    :synsem {:sem {:pred :pensare
                   :subj {:human true}}}}))

(def scrivere
  (unify
   transitive
   infinitive
   {:italian "scrivere"
    :english "to write"
    :synsem {:sem {:pred :scrivere
                   :subj {:human true}
                   :obj {:legible true}}}}))

(def sognare
  (unify
   intransitive
   infinitive
   {:italian "sognare"
    :english {:infinitive "to dream"
              :irregular {:past "dreamt"}}
    :synsem {:sem {:subj {:animate true}
                   :pred {:pred :sognare
                          :essere false}}}}))

(def vedere
  (unify
   transitive
   infinitive
   {:italian {:infinitive "vedere"
              :irregular {:passato "visto"}}
    :english {:infinitive "to see"
              :irregular {:past "seen"}}
    :synsem {:sem {:pred :vedere
                   :subj {:animate true}}}}))

(def finite-verb
  (let [subj-sem (ref :top)
        root-sem (ref {:subj subj-sem})
        subj-agr (ref :top)
        subj (ref {:sem subj-sem
                   :agr subj-agr})
        subcat (ref {:1 subj})
        cat (ref :verb)
        english-infinitive (ref :top)
        italian-infinitive (ref :top)]
     {:root
      {:italian italian-infinitive
       :english english-infinitive
       :synsem {:cat cat
                :sem root-sem
                :subcat subcat}}
      :synsem {:sem root-sem
               :cat cat
               :subcat subcat}
      :italian {:agr subj-agr
                :infinitive italian-infinitive}
      :english {:agr subj-agr
                :infinitive english-infinitive}}))

(def present-tense-verb
  (unify finite-verb
         {:synsem {:infl :present}}))

(def future-tense-verb
  (unify finite-verb
         {:synsem {:infl :futuro}
          :italian {:infl :futuro}
          :english {:infl :futuro}}))

(def present-tense-aux-past-verb
  (unify finite-verb
         {:synsem {:infl :present
                   :sem {:time :past}}}))

;; TODO: all verbs should have :infl information (not just past verbs)
(def past-tense-verb
  (let [past (ref :past)]
    (unify finite-verb
           {:synsem {:infl past}
            :english {:infl past}
            :italian {:infl past}})))

(def trans-present-tense-verb
  (unify present-tense-verb
         (let [obj-sem (ref :top)
               obj (ref {:sem obj-sem})]
           {:root
            {:synsem {:subcat {:2 obj}}}
            :synsem {:sem {:obj obj-sem}}})))

(def trans-future-tense-verb
  (unify future-tense-verb
         (let [obj-sem (ref :top)
               obj (ref {:sem obj-sem})]
           {:root
            {:synsem {:subcat {:2 obj}}}
            :synsem {:sem {:obj obj-sem}}})))

(def trans-past-tense-verb
  (unify past-tense-verb
         (let [obj-sem (ref :top)
               obj (ref {:sem obj-sem})]
           {:root
            {:synsem {:subcat {:2 obj}}}
            :synsem {:sem {:obj obj-sem}}})))

(def intrans-present-tense-verb
  (unify present-tense-verb
         {:root {:synsem
                 {:subcat {:2 '()}}}}))

(def intrans-future-tense-verb
  (unify future-tense-verb
         {:root {:synsem
                 {:subcat {:2 '()}}}}))

(def intrans-past-tense-verb
  (unify past-tense-verb
         {:root
          {:synsem {:subcat {:2 '()}}}}))

(def present-aux-verbs
  (list
   (unify {:root (fs/copy avere-aux-trans)}
          present-tense-aux-past-verb)
   (unify {:root (fs/copy essere-aux-intrans)}
          present-tense-aux-past-verb)
   (unify {:root (fs/copy essere-aux-trans)}
          present-tense-aux-past-verb)))

(def avere-present-aux-trans
  (first present-aux-verbs))

(def past-intransitive-verbs
  (list
   (unify {:root andare-intrans}
          intrans-past-tense-verb)
   (unify {:root dormire}
          intrans-past-tense-verb)
   (unify {:root sognare}
          intrans-past-tense-verb)))

(def past-transitive-verbs
  (list
   (unify {:root avere}
          trans-past-tense-verb)
   (unify {:root bevere}
          trans-past-tense-verb)
   (unify {:root comprare}
          trans-past-tense-verb)
   (unify {:root essere-trans}
          trans-past-tense-verb)
   (unify {:root fare-make}
          trans-past-tense-verb)
   (unify {:root leggere}
          trans-past-tense-verb)
   (unify {:root mangiare}
          trans-past-tense-verb)
   (unify {:root parlare}
          trans-past-tense-verb)
   (unify {:root scrivere}
          trans-past-tense-verb)
   (unify {:root vedere}
          trans-past-tense-verb)))

(def past-verbs (concat past-intransitive-verbs past-transitive-verbs))

(def present-transitive-verbs
  (list
   (unify {:root avere}
          trans-present-tense-verb)
   (unify {:root bevere}
          trans-present-tense-verb)
   (unify {:root comprare}
          trans-present-tense-verb)

   ;; need some activities (nouns with {:activity true}) to enable this:
   ;; (unify {:root fare-do}
   ;;           trans-present-tense-verb)

   (unify {:root fare-make}
          trans-present-tense-verb)
   (unify {:root leggere}
          trans-present-tense-verb)
   (unify {:root mangiare}
          trans-present-tense-verb)
   (unify {:root parlare}
          trans-present-tense-verb)
   (unify {:root scrivere}
          trans-present-tense-verb)
   (unify {:root vedere}
          trans-present-tense-verb)
   ))

(def future-transitive-verbs
  (list
  (unify {:root avere}
          trans-future-tense-verb)
   (unify {:root bevere}
          trans-future-tense-verb)
   (unify {:root comprare}
          trans-future-tense-verb)

   ;; need some activities (nouns with {:activity true}) to enable this:
   ;; (unify {:root fare-do}
   ;;           trans-future-tense-verb)

   (unify {:root fare-make}
          trans-future-tense-verb)
   (unify {:root leggere}
          trans-future-tense-verb)
   (unify {:root mangiare}
          trans-future-tense-verb)
   (unify {:root parlare}
          trans-future-tense-verb)
   (unify {:root scrivere}
          trans-future-tense-verb)
   (unify {:root vedere}
          trans-future-tense-verb)
   ))

(def present-intransitive-verbs
  (list
   (unify {:root andare-intrans}
          intrans-present-tense-verb)
   (unify {:root dormire}
          intrans-present-tense-verb)
   (unify {:root pensare}
          intrans-present-tense-verb)
   (unify {:root sognare}
          intrans-present-tense-verb)))

(def future-intransitive-verbs
  (list
   (unify {:root dormire}
          intrans-future-tense-verb)
   (unify {:root pensare}
          intrans-future-tense-verb)
   (unify {:root sognare}
          intrans-future-tense-verb)))


(def present-verbs
  (concat
   present-aux-verbs
   present-transitive-verbs
   present-intransitive-verbs))

(def future-verbs
  (concat
   future-transitive-verbs
   future-intransitive-verbs))

(def verbs
  (concat
   present-aux-verbs
   present-verbs
   past-verbs
   future-verbs
   ;; infinitives:
   (list
    andare-intrans
    avere
    bevere
    comprare
    dormire
;    fare-do
    fare-make
    leggere
    mangiare
    parlare
    pensare
    scrivere
    sognare
    vedere
    )))

(def pronouns
  (list {:synsem {:cat :noun
                  :agr {:case :nom
                        :person :1st
                        :number :sing}
                  :sem (unify human {:pred :io})
                  :subcat '()}
         :english "i"
         :italian "io"}
        {:synsem {:cat :noun
                  :agr {:case :nom
                        :person :2nd
                        :number :sing}
                  :sem (unify human {:pred :tu})
                  :subcat '()}
         :english "you"
         :italian "tu"}
        {:synsem {:cat :noun
                  :agr {:case :nom
                        :person :3rd
                        :gender :masc
                        :number :sing}
                  :sem (unify human {:pred :lui})
                  :subcat '()}
         :english "he"
         :italian "lui"}
        {:synsem {:cat :noun
                  :agr {:case :nom
                        :person :3rd
                        :gender :fem
                        :number :sing}
                  :sem (unify human {:pred :lei})
                  :subcat '()}
         :english "she"
         :italian "lei"}
        {:synsem {:cat :noun
                  :agr {:case :nom
                        :person :1st
                        :number :plur}
                  :sem (unify human {:pred :noi})
                  :subcat '()}
         :english "we"
         :italian "noi"}
        {:synsem {:cat :noun
                  :agr {:case :nom
                        :person :2nd
                        :number :plur}
                  :sem (unify human {:pred :voi})
                  :subcat '()}
         :italian "voi"
         :english "you all"}
        {:synsem {:cat :noun
                  :agr {:case :nom
                        :person :3rd
                        :number :plur}
                  :sem (unify human {:pred :loro})
                  :subcat '()}
         :italian "loro"
         :english "they"}))

(def prepositions
  (list {:synsem {:cat :prep
                  :subcat {:1 {:cat :noun
                               :sem {:place true}}}}
         :italian "a"
         :english "at"}))

;; TODO: cut down duplication in here (i.e. :italian :cat, :english :cat, etc).
(def adjectives
  (list {:synsem {:cat :adjective
                  :sem {:pred :alto
                        :mod {:human true}}}
         :italian {:italian "alto"
                   :cat :adjective}
         :english {:english "tall"
                   :cat :adjective}}
        {:synsem {:cat :adjective
                  :sem {:pred :bianco
                        :mod {:physical-object true
                              :human false}}}
        :italian {:italian "bianco"
                  :irregular {:masc {:plur "bianchi"}
                              :fem {:plur "bianche"}}
                   :cat :adjective}
         :english {:english "white"
                   :cat :adjective}}

        {:synsem {:cat :adjective
                  :sem {:pred :difficile
                        :mod {
                              :drinkable false
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

        {:synsem {:cat :adjective
                  :sem {:pred :nero
                        :mod {:physical-object true
                              :human false}}}
         :italian {:italian "nero"
                   :cat :adjective}
         :english {:english "black"
                   :cat :adjective}}

        {:synsem {:cat :adjective
                  :sem {:pred :piccolo
                        :mod {:physical-object true
                              :mass false}}}
         :italian {:italian "piccolo"
                   :cat :adjective}
         :english {:english "small"
                   :cat :adjective}}

        {:synsem {:cat :adjective
                  :sem {:pred :rosso
                        :mod {:physical-object true
                              :human false}}}
         :italian {:italian "rosso"
                   :cat :adjective}
         :english {:english "red"
                   :cat :adjective}}))


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
  (set/union (set (lookup {:italian italian}))
             (set (lookup {:italian {:infinitive italian}}))
             (set (lookup {:italian {:infinitive {:infinitive italian}}}))
             (set (lookup {:root {:italian italian}}))
             (set (lookup {:italian {:italian italian}}))
             (set (lookup {:root {:italian {:italian italian}}}))
             (set (lookup {:italian {:irregular {:passato italian}}}))))

(defn en [english]
  (lookup {:english english}))

(def lexicon (concat adjectives determiners nouns prepositions pronouns verbs))

;(def nouns (list (first (it "professoressa"))))
;(def adjectives (list (first (it "piccolo"))))

(map (fn [lexeme]
       (let [italian (:italian lexeme)
             english (:english lexeme)]
         (add italian english
              lexeme)))
     lexicon)

