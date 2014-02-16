(ns italianverbs.pos
  (:require
   [italianverbs.unify :refer (unifyc)]))

(def noun
  (let [gender (ref :top)
        ;; common nouns are underspecified for number: number selection (:sing or :plur) is deferred until later.
        ;; (except for mass nouns which are only singular)
        number (ref :top)
        ;; common nouns are neither nominative or accusative. setting their case to :top allows them to (match) with
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
        {:synsem {:cat :noun
                  :agr {:person :3rd}
                  :subcat {:1 {:cat :det}
                           :2 '()}}}

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
        (unifyc mass
                common
                {:synsem {:sem {:number :sing
                                :drinkable true}}})]
    {:agreement agreement
     :common common
     :countable countable
     :drinkable drinkable
     :feminine feminine
     :masculine masculine}))

(def agreement-noun (:agreement noun))
(def common-noun (:common noun))
(def countable-noun (:countable noun))
(def drinkable-noun (:drinkable noun))
(def feminine-noun (:feminine noun))
(def masculine-noun (:masculine noun))

(def proper-noun
  {:synsem {:cat :noun
            :pronoun false
            :propernoun true
            :agr {:person :3rd}
            :subcat '()}})

(def adjective
  (let [adjective (ref :adjective)
        gender (ref :top)
        number (ref :top)]
    {:synsem {:cat adjective
              :agr {:gender gender
                    :number number}
              }
     :italian {:cat adjective
               :agr {:number number
                     :gender gender}}
     :english {:cat adjective}}))

;; useful abbreviations (aliases for some commonly-used maps):
(def human {:human true})
(def animal {:animate true :human false})

;; A generalization of intransitive and transitive:
;; they both have a subject, thus "subjective".
(def verb-subjective
  (let [subj-sem (ref :top)
        subject-agreement (ref {:case :nom})
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
  (unifyc verb-subjective
          {:synsem {:subcat {:2 '()}}}))

;; transitive: has both subject and object.
(def transitive
  (unifyc verb-subjective
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

(def verb {:transitive transitive})

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

;; whether a verb has essere or avere as its
;; auxiliary to form its passato-prossimo form:
;; Must be encoded in both the :italian (for morphological agreement)
;; and the :synsem (for subcategorization by the appropriate aux verb).
(def verb-aux-type
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
                           :subcat {:1 subject}
                           :sem sem
                           :infl :past}}}}))

(def verb-aux-type-2
  (let [essere-binary-categorization (ref :top)
        aux (ref true)
        sem (ref {:tense :past})
        subject (ref :top)
        object (ref :top)]
    {:italian {:aux aux
               :essere essere-binary-categorization}
     :synsem {:aux aux
              :sem sem
              :essere essere-binary-categorization
              :subcat {:1 subject
                       :2 {:cat :verb
                           :subcat {:1 subject
                                    :2 object}
                           :sem sem
                           :infl :past}}}}))

;; TODO: not using this: either use or lose.
(def transitive-but-with-prepositional-phrase-instead-of-noun
  (unifyc verb-subjective
          (let [obj-sem (ref :top)
                infl (ref :top)]
            {:english {:infl infl}
            :italian {:infl infl}
             :synsem {:sem {:obj obj-sem}
                      :infl infl
                      :subcat {:2 {:sem obj-sem
                                   :subcat '()
                                   :cat :prep}
                               :3 '()}}})))

;; whether a verb has essere or avere as its
;; auxiliary to form its passato-prossimo form:
;; Must be encoded in both the :italian (for morphological agreement)
;; and the :synsem (for subcategorization by the appropriate aux verb).
(def verb-aux-type
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
                           :subcat {:1 subject}
                           :sem sem
                           :infl :past}}}}))

(def verb-aux-type-2
  (let [essere-binary-categorization (ref :top)
        aux (ref true)
        sem (ref {:tense :past})
        subject (ref :top)
        object (ref :top)]
    {:italian {:aux aux
               :essere essere-binary-categorization}
     :synsem {:aux aux
              :sem sem
              :essere essere-binary-categorization
              :subcat {:1 subject
                       :2 {:cat :verb
                           :subcat {:1 subject
                                    :2 object}
                           :sem sem
                           :infl :past}}}}))