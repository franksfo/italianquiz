(ns italianverbs.lexicon.italiano
  (:refer-clojure :exclude [get-in merge resolve]))

(require '[italianverbs.lexiconfn :refer (comparative non-comparative-adjective subcat0)])
(require '[italianverbs.lexicon :refer (transform)])
(require '[italianverbs.pos :refer :all])
(require '[italianverbs.unify :refer :all :exclude [unify]])
(require '[italianverbs.unify :as unify])

(defn unify [& args]
  (apply unifyc args))

(defn phonize [a-map a-string]
  (cond (or (vector? a-map) (seq? a-map))
        (map (fn [each-entry]
               (phonize each-entry a-string))
             a-map)
        true
        (merge a-map
               {:italiano a-string
                :phrasal false})))

(def lexicon
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

   "acqua"

   (unify (:agreement noun)
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
           subcat0
           {:synsem {:cat :adjective
                     :sem {:pred :affolato
                           :comparative false
                           :place true}}})] ;; only places can be crowded.

   "aiutare"

   {:synsem {:essere false
             :disable :fail
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

   "amare"

   [(unify agreement-noun
           common-noun
           countable-noun
           masculine-noun
           {:synsem {:sem {:pred :amico
                           :human true
                           :child false}}
            :italian {:italian "amico"}
            :english {:english "friend"}})

    (unify transitive
           {:synsem {:essere false
                     :sem {:pred :amare
                           :activity false
                           :discrete false
                           :subj {:human true}
                           :obj {:animate true}}}})]

   "andare"

    (map (fn [each]
           (unify
            each
            ;; common part of all andare lexemes:
            {:italian {:infinitive "andare"
                       :essere true
                       :drop-e true
                       :irregular {:present {:1sing "vado"
                                             :2sing "vai"
                                             :3sing "va"
                                             :1plur "andiamo"
                                             :2plur "andate"
                                             :3plur "vanno"}}}
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

   "bello"

   [;; non-comparative
    (unify adjective
           subcat0
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
              :italian {:italian "bello"}
              :english {:english "beautiful"}}))]

   "bene"
   {:synsem {:cat :adverb
             :sem {:pred :bene}}
    :italian {:italian "bene"}
    :english {:english "well"}}

   ;; bere
   "bere"

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
       :synsem {:essere false
                :sem {:pred :bere
                      :subj {:animate true}
                      :obj {:drinkable true}}}})

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


   "un"
   [{:synsem {:cat :det
              :def :indef
              :mass false
              :gender :masc
              :number :sing}}]
})

(defn transform-each-lexical-val [lexical-val]
  (cond
   (map? lexical-val)
   (if false lexical-val
       (transform lexical-val))
   true
   (if false
     lexical-val
     (map (fn [each]
            (transform each))
          lexical-val))))

;; http://stackoverflow.com/questions/1676891/mapping-a-function-on-the-values-of-a-map-in-clojure
;; http://stackoverflow.com/a/1677927
(defn map-function-on-map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(def lexicon
  (map-function-on-map-vals 
   (if false
     lexicon
     {"Antonio" (get lexicon "Antonio")
      "il" (get lexicon "il")
      "dormire" (get lexicon "dormire")}
     )

   transform-each-lexical-val))


