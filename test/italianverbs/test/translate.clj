(ns italianverbs.test.translate)
(require '[italianverbs.translate :refer :all])
(require '[clojure.test :refer :all])

(deftest translate-a-cat
  (is (= "a cat" (translate "un gatto"))))

(deftest translate-she-reads
  (is (= "she reads" (translate "lei legge"))))

