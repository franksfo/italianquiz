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

(def food {:edible true
           :buyable true})

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

(def np-1-lexicon
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

        ]
    (list

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

     ;; inherently singular.
     (unify noun-conjugator
            {:root (unify agreement
                          common-noun
                          masculine
                          {:synsem {:sem {:pred :pane
                                          :edible true
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

     )))

;; "x-itive": a generalization of intransitive and transitive (they both have a subject)
(def x-itive
  (let [subj-sem (ref :top)
        subj (ref {:sem subj-sem
                   :cat :noun
                   :agr {:case {:not :acc}}})]
    {:synsem {:sem {:subj subj-sem}
              :subcat {:1 subj}}}))

;; intransitive: has subject but no object.
(def intransitive
  (unify x-itive
         {:synsem
          {:subcat {:2 :nil!}}}))

;; intransitive: has both subject and object.
(def transitive
  (unify x-itive
         (let [obj-sem (ref :top)
               obj (ref {:sem obj-sem
                         :cat :noun
                         :agr {:case {:not :nom}}})]
           {:synsem {:sem {:obj obj-sem}
                     :subcat {:2 obj}}})))

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

(def comprare
  (unify
   transitive
   infinitive-verb
   {:italian "comprare"
    :english "to buy"
    :synsem {:sem {:pred :comprare
                   :subj {:human true}
                   :obj {:artifact true}}}}))

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
   {:italian "leggere"
    :english "to read"
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

(def finitizer
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
               :subcat subcat
               :infl :present}
      :italian {:agr subj-agr
                :infinitive italian-infinitive}
      :english {:agr subj-agr
                :infinitive english-infinitive}}))

(def trans-finitizer
  (unify finitizer
         (let [obj-sem (ref :top)
               obj (ref {:sem obj-sem})]
           {:root
            {:synsem {:subcat {:2 obj}}}
            :synsem {:sem {:obj obj-sem}}})))

(def intrans-finitizer
  (unify finitizer
         {:root {:synsem
                 {:subcat {:2 :nil!}}}}))

(def vp-1-lexicon
  (concat
   (list
    comprare
    (unify {:root comprare}
           trans-finitizer)
    dormire
    (unify {:root dormire}
           intrans-finitizer)
    fare1
    (unify {:root fare1}
           trans-finitizer)
    fare2
    (unify {:root fare2}
           trans-finitizer)
    leggere
    (unify {:root leggere}
           trans-finitizer)
    mangiare
    (unify {:root mangiare}
           trans-finitizer)
    
    scrivere
    (unify {:root scrivere}
           trans-finitizer)

    sognare
    (unify {:root sognare}
           intrans-finitizer)

    vedere
    (unify {:root vedere}
           trans-finitizer))))


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
    
(def lexicon (concat vp-1-lexicon np-1-lexicon pronouns))

(map (fn [lexeme]
       (let [italian (:italian lexeme)
             english (:english lexeme)]
         (add italian english
              lexeme)))
     lexicon)

"TODO: rewrite more efficiently (rather than recurring)"
(defn lookup-in [query collection]
  "find all members of the collection that matches with query successfully."
  (if (= (.size collection) 0)
    nil
    (let [result (fs/match query (fs/copy (first collection)))]
      (if (not (fs/fail? result))
        (cons (first collection) (lookup-in query (rest collection)))
        (lookup-in query (rest collection))))))

(defn lookup [query]
  (lookup-in query lexicon))

(defn it [italian]
  (set/union
   (lookup {:italian italian})
   (lookup {:italian {:infinitive italian}})
   (lookup {:root {:italian italian}})))

(defn en [english]
  (lookup {:english english}))
