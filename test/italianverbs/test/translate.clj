(ns italianverbs.test.translate)
(require '[italianverbs.translate :refer :all])
(require '[clojure.test :refer :all])

(deftest translate-a-cat
  (is (= "a cat" (translate "un gatto"))))

