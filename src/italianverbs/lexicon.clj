(ns italianverbs.lexicon
  (:use [italianverbs.lexiconfn]
        [clojure.test])
  (:require
   [italianverbs.fs :as fs]
   [clojure.set :as set]))

;; WARNING: clear blows away entire lexicon in backing store (mongodb).
(clear!)

(def human {:human true
            :animate true
            :buyable false})
(def animal {:animate true})

(def food {:synsem {:sem {:edible true
                          :buyable true}}})

(def infinitive-verb
  {:synsem {:cat :verb
            :infl :infinitive}})

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
                :agr agr}
      :english {:root english-root
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
        {:synsem {:cat :noun
                  :agr {:person :3rd}
                  :subcat {:1 {:cat :det}}}}

        masculine {:synsem {:agr {:gender :masc}}}
        feminine {:synsem {:agr {:gender :fem}}}

        mass-noun
        {:synsem {:subcat {:1 {:cat :det
                               :number :sing
                               :def :def}}}}

        drinkable
        (unify noun-conjugator
               mass-noun
               {:root (unify agreement
                             common-noun
                             {:synsem {:sem {:number :sing
                                             :drinkable true
                                             :buyable true}}})})
        ]
    (list


     (unify drinkable
            feminine
            {:root {:italian "acqua"
                    :english "water"
                    :synsem {:sem {:pred :acqua}}}})

     (unify drinkable
            feminine
            {:root {:italian "birra"
                    :english "beer"
                    :synsem {:sem {:pred :birra
                                   :artifact true}}}})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          masculine
                          {:synsem {:sem {:pred :amico
                                          :human :true}}
                           :italian "amico"
                           :english "friend"})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          masculine
                          {:synsem {:sem {:pred :compito
                                          :legible true
                                          :buyable false
                                          :artifact true
                                          :activity true}}
                           :italian "compito"
                           :english "homework assignment"})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          masculine
                          {:synsem {:sem {:pred :mare
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
                          {:synsem {:sem {:pred :pane
                                          :edible true
                                          :buyable true
                                          :artifact true}}
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

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          masculine
                          {:synsem {:sem {:pred :fiore
                                          :animate false
                                          :buyable true
                                          :artifact false}}
                           :italian "fiore"
                           :english "flower"}
                          {:synsem {:subcat {:1 {:cat :det}}}})})



     
     ;; inherently plural.
     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          feminine
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
                          {:synsem {:sem {:artifact true
                                          :pred :scala}}
                           :italian "scala"
                           :english "ladder"})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          masculine
                          {:synsem {:sem human}}
                          {:synsem {:sem {:pred :ragazzo}}
                           :italian "ragazzo"
                           :english "guy"})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          masculine
                          {:synsem {:sem human}}
                          {:synsem {:sem {:pred :dottore}}
                           :italian "dottore"
                           :english "doctor"})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          feminine
                          {:synsem {:sem human}}
                          {:synsem {:sem {:pred :professoressa}}}
                          {:italian "professoressa"
                           :english "professor"})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          feminine
                          {:synsem {:sem human}}
                          {:synsem {:sem {:pred :ragazza}}}
                          {:italian "ragazza"
                           :english "girl"})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          masculine
                          {:synsem {:sem human}}
                          {:synsem {:sem {:pred :studente}}}
                          {:italian "studente"
                           :english "student"})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          masculine
                          {:synsem {:sem {:pred :libro
                                          :legible true
                                          :artifact true}}
                           :italian "libro"
                           :english "book"})})
     
     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          masculine
                          {:synsem {:sem (unify animal {:pred :cane})}
                           :italian "cane"
                           :english "dog"})})

     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          masculine
                          {:synsem {:sem (unify animal {:pred :gatto})}
                           :italian "gatto"
                           :english "cat"})})

     (unify drinkable
            masculine
            {:root {:italian "vino"
                    :english "wine"
                    :synsem {:sem {:pred :vino
                                   :artifact true}}}})
     ;; articles
     
     {:synsem {:cat :det
               :def :def
               :gender :masc
               :number :sing}
      :italian "il"
      :english "the"}
     {:synsem {:cat :det
               :def :indef
               :gender :masc
               :number :sing}
      :italian "un"
      :english "a"}

     {:synsem {:cat :det
               :def :def
               :gender :fem
               :number :sing}
      :italian "la"
      :english "the"}

     {:synsem {:cat :det
               :def :indef
               :gender :fem
               :number :sing}
      :italian "una"
      :english "a"}

     {:synsem {:cat :det
               :def :def
               :gender :masc
               :number :plur}
      :italian "i"
      :english "the"}

     {:synsem {:cat :det
               :def :def
               :gender :fem
               :number :plur}
      :italian "le"
      :english "the"}

     {:synsem {:cat :det
               :def :partitivo
               :number :sing
               :gender :masc}
      :italian "di il"
      :english "some"}

     {:synsem {:cat :det
               :def :partitivo
               :number :sing
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
    

     
     )))

;; A generalization of intransitive and transitive:
;; they both have a subject, thus "subjective".
(def subjective
  (let [subj-sem (ref :top)
        subj (ref {:sem subj-sem
                   :cat :noun
                   :agr {:case {:not :acc}}})]
    {:synsem {:sem {:subj subj-sem}
              :subcat {:1 subj}}}))

;; intransitive: has subject but no object.
(def intransitive
  (unify subjective
         {:synsem
          {:subcat {:2 :nil!}}}))

;; intransitive: has both subject and object.
(def transitive
  (unify subjective
         (let [obj-sem (ref :top)
               obj (ref {:sem obj-sem
                         :cat :noun
                         :agr {:case {:not :nom}}})]
           {:synsem {:sem {:obj obj-sem}
                     :subcat {:2 obj}}})))

(def bevere
  (unify
   transitive
   infinitive-verb
   {:italian "bevere"
    :english "to drink"
    :synsem {:sem {:pred :bevere
                   :subj {:animate true}
                   :obj {:drinkable true}}}}))

(def fare1
  (unify
   transitive
   {:italian {:infinitive "fare"
              :irregular {:present {:1sing "facio"
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
             :morph :irreg
             :infl :infinitive
             :sem {:pred :fare
                   :subj {:human true}
                   :obj {:activity true}}}}))

(def fare2
  (unify
   transitive
   {:italian {:infinitive "fare"
              :irregular {:present {:1sing "facio"
                                    :2sing "fai"
                                    :3sing "fa"
                                    :1plur "facciamo"
                                    :2plur "fate"
                                    :3plur "fanno"}}}
    :english "to make"
    :synsem {:cat :verb
             :morph :irreg
             :infl :infinitive
             :sem {:pred :fare
                   :subj {:human true}
                   :obj {:artifact true}}}}))


(def avere1
  (unify
   transitive
   {:italian {:infinitive "avere"
              :irregular {:present {:1sing "ho"
                                    :2sing "hai"
                                    :3sing "ha"
                                    :1plur "abbiamo"
                                    :2plur "avete"
                                    :3plur "hanno"}}}
    :english {:infinitive "to have"
              :irregular {:present {:1sing "have"
                                    :2sing "have"
                                    :3sing "has"
                                    :1plur "have"
                                    :2plur "have"
                                    :3plur "have"}}}
    :synsem {:cat :verb
             :morph :irreg
             :infl :infinitive
             :sem {:pred :avere
                   :subj {:human true}
                   :obj {:buyable true}}}}))

(def avere-aux
  (let [v-past-pred (ref :top)]
    (unify
     subjective
     {:synsem {:subcat {:2 {:cat :verb
                            :sem {:pred v-past-pred}
                            :infl :past}}}}
     {:italian {:infinitive "avere"
                :irregular {:present {:1sing "ho"
                                      :2sing "hai"
                                      :3sing "ha"
                                      :1plur "abbiamo"
                                      :2plur "avete"
                                      :3plur "hanno"}}}
      :english {:infinitive "to have"
                :irregular {:present {:1sing "have"
                                      :2sing "have"
                                      :3sing "has"
                                      :1plur "have"
                                      :2plur "have"
                                      :3plur "have"}}}
      :synsem {:cat :verb
               :morph :irreg
               :infl :infinitive
               :sem {:pred v-past-pred}}})))

(def comprare
  (unify
   transitive
   infinitive-verb
   {:italian "comprare"
    :english "to buy"
    :synsem {:sem {:pred :comprare
                   :subj {:human true}
                   :obj {:buyable true}}}}))

(def dormire
  (unify
   intransitive
   infinitive-verb
   {:italian "dormire"
    :english "to sleep"
    :synsem {:sem {:subj {:animate true}
                   :pred :dormire}}}))

(def mangiare
  (unify
   transitive
   infinitive-verb
   {:italian "mangiare"
    :english "to eat"
    :synsem {:sem {:pred :mangiare
                   :subj {:animate true}
                   :obj {:edible true}}}}))

(def leggere
  (unify
   transitive
   infinitive-verb
   {:italian {:infinitive "leggere"
              :irregular {:passato "letto"}}
    :english {:infinitive "to read" ;; spelled "read" but pronounced like "reed".
              :irregular {:past "read"}} ;; spelled "read" but pronounced like "red".
    :synsem {:sem {:pred :leggere
                   :subj {:human true}
                   :obj {:legible true}}}}))

(def vedere
  (unify
   transitive
   infinitive-verb
   {:italian "vedere"
    :english "to see"
    :synsem {:sem {:pred :vedere
                   :subj {:animate true}}}}))

(def scrivere
  (unify
   transitive
   infinitive-verb
   {:italian "scrivere"
    :english "to write"
    :synsem {:sem {:pred :scrivere
                   :subj {:human true}
                   :obj {:legible true}}}}))

(def sognare
  (unify
   intransitive
   infinitive-verb
   {:italian "sognare"
    :english "to dream"
    :synsem {:sem {:subj {:animate true}
                   :pred :sognare}}}))


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
                 {:subcat {:2 :nil!}}}}))

(def vp-1-lexicon
  (concat
   (list
    avere1
    (unify {:root avere1}
           trans-present-tense-verb)

    avere-aux
    (unify {:root avere-aux}
           present-tense-verb)

    bevere
    (unify {:root bevere}
           trans-present-tense-verb)
    comprare
    (unify {:root comprare}
           trans-present-tense-verb)
    dormire
    (unify {:root dormire}
           intrans-present-tense-verb)
    fare1
    (unify {:root fare1}
           trans-present-tense-verb)
    fare2
    (unify {:root fare2}
           trans-present-tense-verb)
    leggere
    (unify {:root leggere}
           trans-present-tense-verb)
    (unify {:root leggere}
           trans-past-tense-verb)

    mangiare
    (unify {:root mangiare}
           trans-present-tense-verb)
    
    scrivere
    (unify {:root scrivere}
           trans-present-tense-verb)

    sognare
    (unify {:root sognare}
           intrans-present-tense-verb)

    vedere
    (unify {:root vedere}
           trans-present-tense-verb))))


(def pronouns
  (list {:synsem {:cat :noun
                  :agr {:case :nom
                        :person :1st
                        :number :sing}
                  :sem (unify human {:pred :io})
                  :subcat :nil!}
         :english "i"
         :italian "io"}
        {:synsem {:cat :noun
                  :agr {:case :nom
                        :person :2nd
                        :number :sing}
                  :sem (unify human {:pred :tu})
                  :subcat :nil!}
         :english "you"
         :italian "tu"}
        {:synsem {:cat :noun
                  :agr {:case :nom
                        :person :3rd
                        :gender :masc
                        :number :sing}
                  :sem (unify human {:pred :lui})
                  :subcat :nil!}
         :english "he"
         :italian "lui"}
        {:synsem {:cat :noun
                  :agr {:case :nom
                        :person :3rd
                        :gender :fem
                        :number :sing}
                  :sem (unify human {:pred :lei})
                  :subcat :nil!}
         :english "she"
         :italian "lei"}
        {:synsem {:cat :noun
                  :agr {:case :nom
                        :person :1st
                        :number :plur}
                  :sem (unify human {:pred :noi})
                  :subcat :nil!}
         :english "we"
         :italian "noi"}
        {:synsem {:cat :noun
                  :agr {:case :nom
                        :person :2nd
                        :number :plur}
                  :sem (unify human {:pred :voi})
                  :subcat :nil!}
         :italian "voi"
         :english "you all"}
        {:synsem {:cat :noun
                  :agr {:case :nom
                        :person :3rd
                        :number :plur}
                  :sem (unify human {:pred :loro})
                  :subcat :nil!}
         :italian "loro"
         :english "they"}))

(def prepositions
  (list {:synsem {:cat :prep
                  :subcat {:1 {:cat :noun
                               :sem {:place true}}}}
         :italian "a"
         :english "at"}))

(def lexicon (concat vp-1-lexicon nouns pronouns prepositions))

(map (fn [lexeme]
       (let [italian (:italian lexeme)
             english (:english lexeme)]
         (add italian english
              lexeme)))
     lexicon)

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

(defn lookup [query]
  (lookup-in query lexicon))

;; note: seems to have duplicates
;; (e.g. finite form of italian has both
;;  {:italian {:infinitive "leggere"}} and
;;  {:root {:italian "leggere"}})
;; apparently I'm not understanding how to use
;; set/union correctly to avoid the duplication.
(defn it [italian]
  (set/union
   (lookup {:italian italian})
   (lookup {:italian {:infinitive italian}})
   (lookup {:root {:italian italian}})))

(defn en [english]
  (lookup {:english english}))
