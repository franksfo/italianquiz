(ns italianverbs.test.borges.reader
  (:refer-clojure :exclude [get get-in resolve merge])
  (:require
   [clojure.core :as core]
   [italianverbs.borges.reader :refer :all]
   [italianverbs.borges.writer]
   [italianverbs.engine]
   [italianverbs.italiano :as it]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.unify :refer (get get-in)]))

;; (populate 1 {:synsem {:infl :futuro :sem {:pred :chiedere :subj {:pred :lei}}}})
;; (populate 1 {:synsem {:infl :present :sem {:pred :chiedere :subj {:pred :lei}}}})
;;(do (truncate)  (populate 1 {:synsem {:sem {:pred :chiedere :subj {:pred :lei}}}}))

(def spec {:synsem {:essere true}})

(def results (generate-using-db {:synsem {:essere true}} "it"))


