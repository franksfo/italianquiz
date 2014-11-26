(ns italianverbs.lexicon.english
  (:refer-clojure :exclude [get-in]))

(require '[clojure.tools.logging :as log])
(require '[italianverbs.lexiconfn :refer (compile-lex unify)])
;(require '[italianverbs.morphology.english :refer (agreement analyze exception-generator phonize english-specific-rules)])
(require '[italianverbs.morphology.english :refer (analyze)])
(require '[italianverbs.pos :refer (adjective animal agreement-noun
                                    cat-of-pronoun common-noun
                                    comparative
                                    countable-noun determiner
                                    drinkable-noun feminine-noun 
                                    masculine-noun 
                                    non-comparative-adjective noun
                                    pronoun-acc pronoun-noun sentential-adverb
                                    verb verb-aux)])
(require '[italianverbs.pos.english :refer (intransitive intransitive-unspecified-obj transitive)])
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
          masculine-noun
          {:synsem {:sem (unify animal {:pred :gatto
                                        :pet true})}})


   "embrace"
   (unify transitive
          {:synsem {:sem {:pred :abbracciare
                          :subj {:human true}}}})

   "hug"
   (unify transitive
          {:synsem {:sem {:pred :abbracciare
                          :subj {:human true}
                          :obj {:living true}}}})

   "read"
   (let [common {:english {:english "to read" ;; spelled "read" but pronounced like "reed".
                           :past "read"
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

(def lexicon (compile-lex lexicon-source exception-generator phonize))

(defn lookup [token]
  "return the subset of lexemes that match this token from the lexicon."
  (analyze token (fn [k]
                   (get lexicon k))))
