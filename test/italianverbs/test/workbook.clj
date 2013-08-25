(ns italianverbs.test.workbook
  (:refer-clojure :exclude [get-in merge resolve find])
  (:use [clojure.test]
        [italianverbs.generate]
        [italianverbs.grammar]
        [italianverbs.lexicon]
        [italianverbs.lexiconfn]
        [italianverbs.morphology]
        [italianverbs.workbook]
        [italianverbs.unify :exclude [unify]]
        ))

(deftest test-gen13
  ;; test negative values: should be no infinite recursion.
  (is (nil? (gen13 -1 seed-phrases lexicon))))

(deftest test-gen13-2
  (nth (take 1 (gen13 0 seed-phrases lexicon) ) 0))

;(fo (take 10 (gen13 0 (gen13 0 seed-phrases lexicon) lexicon)))

(deftest test-gen13-3
  (let [result (double-apply 0 seed-phrases (list (it "il") (it "compito")))]
    (is (not (nil? result)))
    (is (= "Il compito (The homework assignment)." (first (fo (first result)))))))

