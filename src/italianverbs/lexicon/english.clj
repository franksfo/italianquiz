(ns italianverbs.lexicon.english)

(require '[clojure.tools.logging :as log])
(require '[italianverbs.lexiconfn :refer (compile-lex unify)])
(require '[italianverbs.pos :refer :all])
(require '[italianverbs.unify :refer (copy)])

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
                                :discrte false
                                :subj {:human true}}}}]
                 
     [(unify
       common
       transitive
       {:synsem {:sem {:obj {:legible true}}}})

      (unify
       common
       intransitive-unspecified-obj)])
   

})

;; TODO: need to regenerate :serialized for each exception.
(defn exception-generator [lexicon]
  (let [lexeme-kv (first lexicon)
        lexemes (second lexeme-kv)]
    (if lexeme-kv
      (list {})
      (list {}))))

(def lexicon (compile-lex lexicon-source exception-generator))

