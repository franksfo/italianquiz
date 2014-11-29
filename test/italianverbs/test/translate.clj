(ns italianverbs.test.translate)
(require '[clojure.set :refer :all])
(require '[clojure.test :refer :all])
(require '[italianverbs.morphology :refer (fo)])
(require '[italianverbs.english :as en])
(require '[italianverbs.italiano :as it])
(require '[italianverbs.translate :refer :all])

(deftest translate-a-cat
  (let [un-gatto (translate-all "un gatto")
        formatted (if (seq? un-gatto)
                    (set (map fo un-gatto))
                    (set (list (fo un-gatto))))]
    (is (not (empty? (select #(= "a cat" %)
                             formatted))))))

(deftest translate-she-reads
  (is (= "she reads" (translate "lei legge"))))

(deftest test-roundtrip-italian
  (let [retval (fo (it/generate (get-meaning (parse "io dormo"))))]
    (or
     (and
      (seq? retval)
      (= (.size retval))
      (is (= "io dormo" (first retval))))
     (and
      (string? retval)
      (is (= "io dormo" retval))))))

(deftest test-roundtrip-english
  (let [retval (fo (en/generate (get-meaning (parse "she sleeps"))))]
    (or
     (and
      (seq? retval)
      (= (.size retval))
      (is (= "she sleeps" (first retval))))
     (and
      (string? retval)
      (is (= "she sleeps" retval))))))

