(ns italianverbs.test.workbook
  (:refer-clojure :exclude [get-in merge resolve find])
  (:use [clojure.test]
        [italianverbs.generate]
        [italianverbs.grammar]
        [italianverbs.lexicon]
        [italianverbs.lexiconfn]
        [italianverbs.morphology]
        [italianverbs.ug]
        [italianverbs.workbook]
        [italianverbs.unify :exclude [unify]]
        ))

(def seed-phrases (list 'cc10))



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
         (shuffle lexicon)
         (gen14 seed-phrases
                (shuffle lexicon)
                (shuffle lexicon)
                sent-impl 0)
         sent-impl 0))


