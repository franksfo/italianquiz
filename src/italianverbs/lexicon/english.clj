(ns italianverbs.lexicon.english
  (:refer-clojure :exclude [get-in]))

(require '[clojure.tools.logging :as log])
(require '[italianverbs.lexiconfn :refer (compile-lex unify)])
(require '[italianverbs.morphology.english :refer (analyze english-specific-rules)])
(require '[italianverbs.pos :refer (adjective animal
                                    cat-of-pronoun common-noun
                                    comparative
                                    countable-noun determiner
                                    drinkable-noun
                                    non-comparative-adjective noun
                                    pronoun-acc pronoun-noun sentential-adverb
                                    verb verb-aux)])
(require '[italianverbs.pos.english :refer (agreement-noun intransitive intransitive-unspecified-obj transitive)])
(require '[italianverbs.unify :as unify])

(def lexicon-source
  {"a"
   {:synsem {:cat :det
             :def :indef
             :mass false
             :gender :masc
             :number :sing}}

   "cat"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem (unify animal {:pred :gatto
                                        :pet true})}})


   "embrace"
   (unify transitive
          {:synsem {:sem {:pred :abbracciare
                          :subj {:human true}}}})


   "i"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :number :sing}
             :sem {:human true
                   :pred :io}
             :subcat '()}}

   "hug"
   (unify transitive
          {:synsem {:sem {:pred :abbracciare
                          :subj {:human true}
                          :obj {:living true}}}})

   "read" ;; if this was a phonetic dictionary, there would be two entries for each pronounciation (i.e. both "reed" or "red" pronounciations)
   (let [common {:english {:past "read"
                           :note "(past)"} ;; spelled "read" but pronounced like "red".
                 :synsem {:sem {:pred :leggere
                                :discrete false
                                :subj {:human true}}}}]
                 
     [(unify
       common
       transitive
       {:synsem {:sem {:obj {:legible true}}}})

      (unify
       common
       intransitive-unspecified-obj)])


   "the"
   {:synsem {:cat :det
             :def :def
             :mass false
             :gender :masc
             :number :sing}}
   

})

;; TODO: just a stub for now:
(defn exception-generator [lexicon]
  (let [lexeme-kv (first lexicon)
        lexemes (second lexeme-kv)]
    (if lexeme-kv
      (list {})
      (list {}))))

(defn phonize [a-map a-string]
  (let [common {:phrasal false}]
    ;; TODO: remove support for either list-of-maps - too confusing. Instead, just require a list of maps.
    (cond (or (vector? a-map) (seq? a-map))
          (map (fn [each-entry]
                 (phonize each-entry a-string))
               a-map)

          (map? a-map)
          (unify {:english {:english a-string}}
                 common
                 a-map)

        true
        (unify a-map
               {:english a-string}
               common))))

(def lexicon (compile-lex lexicon-source exception-generator phonize english-specific-rules))

(defn lookup [token]
  "return the subset of lexemes that match this token from the lexicon."
  (analyze token (fn [k]
                   (get lexicon k))))
