(ns italianverbs.benchmark
  (:require
   [italianverbs.grammar :refer (grammar)]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.over :refer :all])

  (time (fo (take 1 (over grammar "io" (over grammar "essere" (over grammar "andare" (over grammar "a" (over grammar "il" "mercato")))))))))
