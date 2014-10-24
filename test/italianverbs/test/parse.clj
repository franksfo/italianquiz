(ns italianverbs.test.parse)

(require '[clojure.test :refer :all])
(require '[italianverbs.parse :refer :all])
(require '[italianverbs.morphology :refer :all])

(deftest parse-test
  (is (not (empty? (fo-ps (parse "un gatto" it-lexicon it-grammar))))))
