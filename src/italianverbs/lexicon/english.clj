(ns italianverbs.lexicon.english
  (:refer-clojure :exclude [get-in merge resolve]))

(require '[italianverbs.pos :refer :all])
(require '[italianverbs.unify :refer :all])

(defn phonize [a-map a-string]
  (cond (or (vector? a-map) (seq? a-map))
        (map (fn [each-entry]
               (phonize each-entry a-string))
             a-map)
        true
        (merge a-map
               {:english a-string
                :phrasal false})))

(def lexicon
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
                                        :pet true})}})})

