(ns italianverbs.test.translate)
(require '[clojure.tools.logging :as log])
(require '[clojure.set :refer :all])
(require '[clojure.test :refer :all])
(require '[italianverbs.morphology :refer (fo)])
(require '[italianverbs.english :as en])
(require '[italianverbs.italiano :as it])
(require '[italianverbs.translate :refer :all])

(deftest translate-a-cat
  (let [un-gatto (translate-all "un gatto")]
    (is (not (empty? (select #(= "a cat" %)
                             (set un-gatto)))))))

(deftest translate-she-reads
  (is (= "she reads" (translate "lei legge"))))

(deftest test-roundtrip-italian
  (let [retval (it/generate (get-meaning (parse "io dormo")))
        retval (cond (seq? retval)
                     (map fo retval)
                     true (fo retval))]
    (is
     (and
      (seq? retval)
      (not (empty? retval))
      (= "io dormo" (first retval)))

      (and
       (string? retval)
       (= "io dormo" retval)))))

(deftest test-roundtrip-english
  (let [retval (en/generate (get-meaning (parse "she sleeps")))
        retval (cond (seq? retval)
                     (map fo retval)
                     true (fo retval))]
    (is
     (or
      (and
       (seq? retval)
       (not (empty? retval))
       (= "she sleeps" (first retval)))
      (and
       (string? retval)
       (= "she sleeps" retval))))))



