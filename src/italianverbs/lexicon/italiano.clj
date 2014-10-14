(ns italianverbs.lexicon.italiano
  (:refer-clojure :exclude [get-in merge resolve]))

(require '[italianverbs.pos :refer :all])
(require '[italianverbs.unify :refer :all])

(defn phonize [a-map a-string]
  (merge a-map
         {:italiano a-string
          :phrasal false}))

(def lexicon
  {"un"
   {:synsem {:cat :det
             :def :indef
             :mass false
             :gender :masc
             :number :sing}}

   "gatto"
   (unify agreement-noun
          common-noun
          countable-noun
          masculine-noun
          {:synsem {:sem (unify animal {:pred :gatto
                                        :pet true})}})})

