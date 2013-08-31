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

(deftest test-gen13-4
  (let [result (unify {:synsem {:infl :present}} (first (double-apply 0 seed-phrases (list (it "io") (it "dormire")))))]
    (is (= "Io dormo (I sleep)." (first (fo result))))))

(deftest test-gen13-5
  (let [result (sentence-impl
                (first (double-apply 0 seed-phrases (list (it "io") (it "dormire")))))]
    (is (or (= "Io dormo (I sleep)." (first (fo result)))
            (= "Io dormirò (I will sleep)." (first (fo result)))))))

;; gen13 test for workbook:
;;(fo (sentence-impl (take 30 (double-apply 0 seed-phrases (take 30 lexicon)))))
;;(fo (sentence-impl (take 1 (double-apply 0 seed-phrases (shuffle lexicon)))))
;;(fo (take 10 (repeatedly #(sentence-impl (take 1 (double-apply 0 seed-phrases (shuffle lexicon)))))))

(deftest test-gen14-1
  (let [result (first (take 1
                            (gen14 seed-phrases
                                   (list (it "io") (it "dormire"))
                                   (list (it "io") (it "dormire"))
                                   sent-impl 0)))]
    (is (or (= "Io dormo (I sleep)." (first (fo result)))
            (= "Io dormirò (I will sleep)." (first (fo result)))))))


(defn functions-as-seqs-test []
  (gen14 seed-phrases (shuffle lexicon)
         (gen14 seed-phrases
                (shuffle lexicon)
                (shuffle lexicon)
                sent-impl 0)
         sent-impl 0))

(defn functions-as-seqs-test2 []
  (gen14 seed-phrases (shuffle tinylex)
         (gen14 seed-phrases
                (shuffle tinylex)
                (shuffle tinylex)
                sent-impl 0)
         sent-impl 0))


(defn functions-as-seqs-test3 []
  (gen14 seed-phrases
         (shuffle tinylex)
         (gen14 seed-phrases
                (shuffle lexicon)
                (shuffle tinylex)
                sent-impl 0)
         sent-impl 0))

(defn functions-as-seqs-test4 []
  (gen14 seed-phrases
         (shuffle verbs)
         (gen14 seed-phrases
                (shuffle nouns)
                (shuffle dets)
                sent-impl 0)
         sent-impl 0))

(defn functions-as-seqs-test5 []
  (gen14 seed-phrases
             (shuffle verbs)
             (fn [] (gen14 (list cc0)
                           (fn [] (gen14 (list hc1)
                                         (fn [] (shuffle nouns))
                                         (fn [] (shuffle adjs))
                                         sent-impl 0))
                           (fn [] (shuffle dets))
                           sent-impl 0))
             sent-impl 0))



