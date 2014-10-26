(ns italianverbs.test.parse)

(require '[clojure.test :refer :all])
(require '[italianverbs.parse :refer :all :exclude [fo fo-ps]])
(require '[italianverbs.morphology :refer [fo]])
(require '[italianverbs.morphology.italiano :refer :all])

(deftest parse-test-1
  (is (= "un gatto" (fo (parse "un gatto" it-lexicon it-grammar)))))

(deftest parse-test-2
  (is (contains? (set (map fo (parse "Antonio dormire" it-lexicon it-grammar))) "Antonio dormirà")))


