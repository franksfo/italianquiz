(ns italianverbs.lexicon.italiano
  (:refer-clojure :exclude [get-in merge resolve]))

(require '[clojure.set :refer (union)])
(require '[clojure.tools.logging :as log])

(require '[italianverbs.lexiconfn :refer (comparative non-comparative-adjective subcat0 unify)])
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
                          :child false}}
           :italiano {:italiano "amico"}
           :english {:english "friend"}})

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

   ;; bere
   "bere"
   (unify
    (:transitive verb)
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
                          :3plur "bevano"}}
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

(defn listify [m]
  (into {}
        (for [[k v] m]
          [k (cond (map? v)
                   (vec (list v))
                   (seq? v)
                   (vec v)
                   true
                   v)])))

;; take source lexicon (above) and compile it.
;; 1. canonicalize all lexical entries
;; (i.e. vectorize the values of the map).

(def lexicon-stage-1
  (listify lexicon-source))

;; http://stackoverflow.com/questions/1676891/mapping-a-function-on-the-values-of-a-map-in-clojure
;; http://stackoverflow.com/a/1677927
(defn map-function-on-map-vals [m f]
  (into {} (for [[k v] m] [k (f k v)])))

;; TODO: Move all functions out of this file: should be human editable only
(defn phonize [a-map a-string]
  (let [common {:phrasal false}]
    (cond (or (vector? a-map) (seq? a-map))
          (map (fn [each-entry]
                 (phonize each-entry a-string))
               a-map)

          (and (map? a-map)
               (not (= :no-italiano (get a-map :italiano))))
          (merge {:italiano {:italiano a-string}}
                 common
                 a-map)

        true
        (merge a-map
               {:italiano a-string}
               common))))

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

(declare map-function-on-map-vals)
(declare transform-each-lexical-val)

;; 2. apply grammatical-category and semantic rules to each element in the lexicon
(def lexicon-stage-2
  (map-function-on-map-vals 
   lexicon-stage-1
   transform-each-lexical-val))

;; TODO: need to regenerate :serialized for each exception.
(defn exception-generator [lexicon]
  (let [lexeme-kv (first lexicon)
        lexemes (second lexeme-kv)]
    (if lexeme-kv
      (let [
            result
            (if true
              nil ;; short-circuit

              (mapcat (fn [path-and-merge-fn]
                        (let [debug
                              (log/info (str "type of path-and-merge-fn: " (type path-and-merge-fn)))
                              
                              path (:path path-and-merge-fn)
                              merge-fn (:merge-fn path-and-merge-fn)]
                          (log/debug (str "path: " path))
                          (log/info "lexeme: " lexeme-kv " has values for path: " path)
                          (mapcat (fn [lexeme]
                                    (if (not (= :none (get-in lexeme path :none)))
                                      (list {(get-in lexeme path :none)
                                             (merge lexeme (apply merge-fn (list lexeme)))})))
                                  lexemes)))
                      [
                       {:path [:italiano :present :1sing]
                        :merge-fn
                        (fn [val]
                          {:italiano {:infl :present
                                      :italiano (get-in val [:italiano :present :1sing] :nothing)
                                      :agr {:number :sing
                                            :person :1st}}})}
                       {:path [:italiano :present :2sing]
                        :merge-fn
                        (fn [val]
                          {:italiano {:infl :present
                                      :italiano (get-in val [:italiano :present :2sing] :nothing)
                                      :agr {:number :sing
                                            :person :2nd}}})}
                       
                       {:path [:italiano :present :3sing]
                        :merge-fn
                        (fn [val]
                          {:italiano {:infl :present
                                      :italiano (get-in val [:italiano :present :3sing] :nothing)
                                      :agr {:number :sing
                                            :person :3rd}}})}]))]
            
            
        (if (not (empty? result))
          (concat result (exception-generator (rest lexicon)))
          (do
            (log/info (str "TYPE OF REST OF LEXICON IS: " (type (rest lexicon))))
            (exception-generator (rest lexicon))))))))

;; 3. generate exceptions
;; problem: merge is overwriting values: use a collator that accumulates values.
;(def exceptions (reduce #(merge-with concat %1 %2)
;                        (map #(listify %)
;                             (exception-generator lexicon-stage-2))))

(def foo (exception-generator lexicon-stage-2))

(def lexicon
  lexicon-stage-2)
;  (merge-with concat lexicon-stage-2 (listify exceptions)))

(def use-a-small-subset false)

;; promote from italianverbs.lexicon.italiano to italianverbs.lexicon.
(defn check-lexicon [lexicon]
  (let [check-one (fn [k v]
                    (let [result (fail? v)]
                      (if result 
                        (log/warn (str "fail found for: " k)))
                      (if result
                        (list k))))]
    (mapcat
     #(let [key %
            val (get lexicon %)]
        (if (seq? val) 
          (mapcat (fn [x] 
                    (check-one key x))
                  val)
          (check-one key val)))
     (keys lexicon))))

