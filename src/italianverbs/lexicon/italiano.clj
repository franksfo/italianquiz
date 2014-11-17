(ns italianverbs.lexicon.italiano
  (:refer-clojure :exclude [get-in merge resolve]))

(require '[clojure.set :refer (union)])
(require '[clojure.tools.logging :as log])

(require '[italianverbs.lexiconfn :refer (comparative exception-generator listify 
                                                      non-comparative-adjective subcat0
                                                      map-function-on-map-vals phonize 
                                                      pronoun-acc
                                                      pronoun-noun unify)])
(require '[italianverbs.lexicon :refer (transform)])
(require '[italianverbs.pos :refer :all])
(require '[italianverbs.unify :refer :all :exclude [unify]])
(require '[italianverbs.unify :as unify])

(def lexicon-source
  {

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
   (unify transitive
           {:synsem {:essere false
                     :sem {:pred :abbracciare
                           :activity false
                           :discrete false
                           :subj {:human true}
                           :obj {:animate true}}}})



   "acqua"

   (unify (:agreement noun)
          (:drinkable noun)
          (:feminine noun)
          {:synsem {:sem {:artifact false
                          :animate false
                          :pred :acqua}}}
          )

   "affolato"

   [;; comparative
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
                                    :sem than-this}}}}))
    ;; non-comparative
    (unify adjective
           {:synsem {:cat :adjective
                     :sem {:pred :affolato
                           :comparative false
                           :place true}}})
                           

    ] ;; only places can be crowded.

   "aiutare" ;; disabled for now: (:disable :fail)

   {:synsem {:essere false
             :disable :fail ;; disabled for now.
             :sem {:pred :aiutare
                   :activity true
                   :subj {:human true}
                   :obj {:human true}}}}


   "alto"

   [;; non-comparative:
    (let [subject-sem (ref {:human true}) ;; only humans can be tall.
          subject-agr (ref :top)] 
      (unify adjective
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
      (unify adjective
             comparative
             {:synsem {:sem {:pred :alto
                             :arg1 subject-sem
                             :arg2 complement-complement-sem}
                       :subcat {:1 {:cat :noun
                                    :sem subject-sem}
                                :2 {:cat :prep
                                    :sem complement-sem}}}}))]

   "amico"
   (unify agreement-noun
          common-noun
          countable-noun
          masculine-noun
          {:synsem {:sem {:pred :amico
                          :human true
                          :child false}}})
   "amare"
    (unify transitive
           {:synsem {:essere false
                     :sem {:pred :amare
                           :activity false
                           :discrete false
                           :subj {:human true}
                           :obj {:animate true}}}})

   "andare"
   (map (fn [each]
          (unify
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
          (unify
           intransitive
           {:synsem {:sem {:location '()}}})

          ;; "andare" that takes a prepositional phrase
          (unify
           verb-subjective
           (let [place-sem (ref {:place true
                                 :pred :a})]
             {:synsem {:sem {:location place-sem}
                       :subcat {:2 {:sem place-sem
                                    :subcat '()
                                    :cat :prep}}}
              :note "andare-pp"}))))

   "Antonio"

   {:synsem {:agr {:number :sing
                   :person :3rd
                   :gender :masc}
             :sem {:pred :antonio
                   :human true}
             :propernoun true}}

   "Antonia"

   {:synsem {:sem {:pred :antonia
                   :human true}
             :agr {:number :sing
                   :person :3rd
                   :gender :fem}
             :propernoun true}}

   "avere"
   (let [avere-common {:synsem {:essere false
                                :cat :verb}
                       :italiano {:italiano "avere"
                                  :drop-e true
                                  :passato "avuto"
                                  :present {:1sing "ho"
                                            :2sing "hai"
                                            :3sing "ha"
                                            :1plur "abbiamo"
                                            :2plur "avete"
                                            :3plur "hanno"}}}]
     [(unify ;; 1. "avere": to possess something buyable
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
                               :essere false}}}}))

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
                                       :2 '()}}}}})])


   "bello"

   [;; non-comparative
    (unify adjective
           {:synsem {:sem {:pred :bello
                           :comparative false
                           }}}) ;; for now, no restrictions on what can be beautiful.
    
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
              :italiano {:italiano "bello"}
              :english {:english "beautiful"}}))]

   "bene"
   {:synsem {:cat :adverb
             :sem {:pred :bene}}
    :italiano {:italiano "bene"}
    :english {:english "well"}}

   "bere"
   (let [bere-common
         {:italiano {:italiano "bere"
                     :passato "bevuto"
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
                               :3plur "bevano"}}}]
     [
      (unify
       bere-common
       (:transitive verb)
       {:synsem {:essere false
                 :sem {:pred :bere
                       :subj {:animate true}
                       :obj {:drinkable true}}}})
      
      (unify
       bere-common
       intransitive
       {:synsem {:essere false
                 :sem {:pred :bere
                       :subj {:animate true}}}})
      ;; even though there is no grammatical object here (since it is intransitive, maybe there could
      ;; be a semantic-only object (at some extra-grammatical level of representation) that could be {:drinkable true}.
      ])


   "cane"
   (unify agreement-noun
          common-noun
          countable-noun
          masculine-noun
          {:synsem {:sem (unify animal {:pred :cane
                                        :pet true})}})

   "dormire"
   {:synsem {:cat :verb
             :essere false
             :sem {:subj {:animate true}
                   :discrete false
                   :pred :dormire}}}


   "gatto"
   (unify agreement-noun
          common-noun
          countable-noun
          masculine-noun
          {:synsem {:sem (unify animal {:pred :gatto
                                        :pet true})}})
   "il"
   (unify determiner
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
             :sem (unify human {:pred :io})
             :subcat '()}}

   "la"
   [{:synsem {:cat pronoun-noun
             :pronoun true
             :case pronoun-acc
             :agr {:gender :fem
                   :person :3rd
                   :number :sing}
             :sem (unify human {:pred :lei})
             :subcat '()}
     :italiano {:initial true   ;; TODO: verify that 'la' above and this below are being unified correctly.
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
     :italiano {:initial true
                :cat pronoun-noun
                :case pronoun-acc}}


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

   ;; TODO: add pronominal "la sua (hers)" and "la sua (his)"
   "la sua"
   [{:synsem {:cat :det
              :def :possessive
              :gender :fem
              :number :sing
             :sem {:gender :fem
                   :number :sing
                   :person :3rd}}}
    {:synsem {:cat :det
              :def :possessive
              :gender :fem
              :number :sing
              :sem {:gender :masc
                    :number :sing
                    :person :3rd}}}]
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

   "lei"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :fem
                   :number :sing}
             :sem (unify human {:pred :lei})
             :subcat '()}}

   "libro"
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
                          :artifact true}}})

   "lo"
   [{:synsem {:cat pronoun-noun
              :pronoun true
              :case pronoun-acc
              :agr {:gender :masc
                    :person :3rd
                    :number :sing}
              :sem (unify human {:pred :lo})
              :subcat '()}}

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
       :italiano {:initial true  ;; TODO: verify that 'lo' above and this below are being unified correctly.
                  :pronoun true
                  :cat pronoun-noun
                  :case pronoun-acc}}]

   "loro"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :number :plur}
                :sem (unify human {:pred :loro})
             :subcat '()}}

   "lui"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :masc
                   :number :sing}
             :sem (unify human {:pred :lui})
             :subcat '()}}

   "madre"
   (unify agreement-noun
          common-noun
          countable-noun
          feminine-noun
          {:synsem {:sem human}}
          {:synsem {:sem {:pred :madre
                          :child false}}})

   ;; non-comparative
   ;; TODO: add comparative
   "nero"
   (unify adjective
          {:synsem {:cat :adjective
                    :sem {:pred :nero
                          :comparative false
                          :physical-object true
                          :human false}}})

   "tu"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :number :sing}
             :sem (unify human {:pred :tu})
             :subcat '()}}

   "un"
   [{:synsem {:cat :det
              :def :indef
              :mass false
              :gender :masc
              :number :sing}}]
   "vino"
   (unify drinkable-noun
          agreement-noun
          masculine-noun
          {:synsem {:sem {:pred :vino
                          :artifact true}}})

})

;; TODO: move this to lexiconfn. however (transform) is in lexicon (not lexiconfn), so
;; it might be better to consolidate lexiconfn and lexicon into a single namespace.
(defn transform-each-lexical-val [italian-lexical-string lexical-val]
  (let [lexical-val
        (phonize lexical-val italian-lexical-string)]
    (cond
     (map? lexical-val)
     (transform lexical-val)
     true
     (map (fn [each]
            (transform each))
          lexical-val))))

;; take source lexicon (declared above) and compile it.
;; 1. canonicalize all lexical entries
;; (i.e. vectorize the values of the map).

(def lexicon-stage-1
  (listify lexicon-source))

;; 2. apply grammatical-category and semantic rules to each element in the lexicon
(def lexicon-stage-2
  (map-function-on-map-vals 
   lexicon-stage-1
   transform-each-lexical-val))

;; 3. generate exceptions
;; problem: merge is overwriting values: use a collator that accumulates values.
(def exceptions (listify (reduce #(merge-with concat %1 %2)
                                 (map #(listify %)
                                      (exception-generator lexicon-stage-2)))))

;; 4. generate final form of lexicon by adding the
;; base lexicon to the exceptions generated from it.
(def lexicon
  (merge-with concat lexicon-stage-2 exceptions))
