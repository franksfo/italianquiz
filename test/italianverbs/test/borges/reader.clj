(ns italianverbs.test.borges.reader
  (:refer-clojure :exclude [get get-in resolve merge])
  (:require
   [clojure.core :as core]
   [clojure.data.json :as json]
   [italianverbs.borges.reader :refer :all]
   [italianverbs.borges.writer]
   [italianverbs.engine]
   [italianverbs.italiano :as it]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.populate :refer (populate)]
   [italianverbs.unify :refer [deserialize get get-in strip-refs]]
   [korma.core :as db]
   ))

;; (populate 1 {:synsem {:infl :futuro :sem {:pred :chiedere :subj {:pred :lei}}}})
;; (populate 1 {:synsem {:infl :present :sem {:pred :chiedere :subj {:pred :lei}}}})
;;(do (truncate)  (populate 1 {:synsem {:sem {:pred :chiedere :subj {:pred :lei}}}}))

(def spec {:synsem {:essere true}})

(populate 1 spec)

; (db/exec-raw [(str "SELECT source.serialized::text AS source
;                                        FROM expression AS source
;                                       WHERE source.language='en' LIMIT 1")] :results)


(def json-spec (json/write-str (strip-refs spec)))
;(def results (generate-using-db spec "en" "it"))

;(def translation-pair (generate {:synsem {:cat :verb
;                                          :subcat ()
;                                          :infl :imperfetto}}
;                                "it"
;                                "en"))


(def foo (generate {:synsem {:essere true}} "en" "it"))
