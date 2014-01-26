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
