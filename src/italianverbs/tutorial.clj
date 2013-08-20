(ns italianverbs.tutorial
  (:refer-clojure :exclude [find get-in merge resolve ])
  (:use
   [italianverbs.lexicon]
   [italianverbs.unify :exclude [get-in merge resolve unify]]
   [italianverbs.generate]
   [italianverbs.grammar]
   [italianverbs.html]
   [italianverbs.morphology]))

