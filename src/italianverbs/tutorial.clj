(ns italianverbs.tutorial
  (:refer-clojure :exclude [find get-in merge resolve ])
  (:use
   [italianverbs.lexicon]
   [italianverbs.unify :exclude [get-in merge resolve unify]]
   [italianverbs.generate]
   [italianverbs.grammar.english :as english]
   [italianverbs.grammar.italiano :as italiano]
   [italianverbs.html]
   [italianverbs.morphology]))

