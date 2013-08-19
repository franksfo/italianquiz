(ns italianverbs.tutorial
  (:refer-clojure :exclude [get-in merge find])
  (:use
   [italianverbs.lexicon]
   [italianverbs.unify :exclude [unify]]
   [italianverbs.generate]
   [italianverbs.grammar]
   [italianverbs.html]
   [italianverbs.morphology]))

