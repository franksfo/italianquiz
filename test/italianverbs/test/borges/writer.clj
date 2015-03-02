(ns italianverbs.test.borges.writer
  (:refer-clojure :exclude [get get-in resolve merge])
  (:require
   [clojure.core :as core]
   [clojure.test :refer :all]
   [italianverbs.borges.writer]
   [italianverbs.engine :refer :all :as engine]
   [italianverbs.english :as en]
   [italianverbs.espanol :as es]
   [italianverbs.italiano :as it]
   [italianverbs.morphology :refer :all]
   [italianverbs.morphology.espanol :as esm]
   [italianverbs.unify :refer [get get-in strip-refs]]
   [italianverbs.borges.writer :refer :all]
   ))

;; (populate 1 {:synsem {:infl :futuro :sem {:pred :chiedere :subj {:pred :lei}}}})
;; (populate 1 {:synsem {:infl :present :sem {:pred :chiedere :subj {:pred :lei}}}})
;;(do (truncate)  (populate 1 {:synsem {:sem {:pred :chiedere :subj {:pred :lei}}}}))

(def spec {:synsem {:essere true}})

(def enrich-function (:enrich @it/small))

(def try (enrich-function {:synsem {:essere true}}))

(def matching-head-lexemes (it/matching-head-lexemes spec))

(def spanish-sentence
  (fo (engine/generate {:synsem {:infl :present :sem {:aspect :progressive}}} @es/small)))

(deftest spanish-working
  (and (is (not (nil? spanish-sentence)))
       (is (not (= "" spanish-sentence)))))


(populate 1 en/small es/small {:synsem {:infl :present :sem {:aspect :progressive}}})
