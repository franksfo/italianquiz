(ns italianverbs.test.translate
  (:refer-clojure :exclude [get-in merge resolve find]))

(require '[italianverbs.grammar.english :as en])
(require '[italianverbs.grammar.italiano :as it])
(require '[italianverbs.morphology :refer [finalize fo fo-ps]])
(require '[italianverbs.unify :refer [get-in]])
(require '[clojure.test :refer :all])

(deftest translate-a-cat
  (is (= "a cat"
         (fo (en/generate {:synsem {:sem (get-in (first (it/parse "un gatto")) [:synsem :sem])}})))))

