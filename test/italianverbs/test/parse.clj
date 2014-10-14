(ns italianverbs.test.parse)

(require '[clojure.test :refer :all])
(require '[italianverbs.parse :refer :all])
(require '[italianverbs.morphology :as morph])

(deftest parse-test
  (is (not (empty? (morph/fo-ps (parse "un gatto" it-lexicon it-grammar))))))
