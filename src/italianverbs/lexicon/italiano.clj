(ns italianverbs.lexicon.italiano
  (:refer-clojure :exclude [get-in merge resolve]))

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


   "cane"
   (unify agreement-noun
          common-noun
          countable-noun
          masculine-noun
          {:synsem {:sem (unify animal {:pred :cane
                                        :pet true})}})


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

   

