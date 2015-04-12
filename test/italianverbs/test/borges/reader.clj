(ns italianverbs.test.borges.reader
  (:refer-clojure :exclude [get get-in resolve merge])
  (:require
   [clojure.core :as core]
   [clojure.data.json :as json]
   [italianverbs.borges.reader :refer :all]
   [italianverbs.borges.writer :refer :all]
   [italianverbs.engine]
   [italianverbs.english :as en]
   [italianverbs.italiano :as it]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.unify :refer [deserialize get get-in strip-refs]]
   [korma.core :as db]
   ))

;; (populate 1 {:synsem {:infl :futuro :sem {:pred :chiedere :subj {:pred :lei}}}})
;; (populate 1 {:synsem {:infl :present :sem {:pred :chiedere :subj {:pred :lei}}}})
;;(do (truncate)  (populate 1 {:synsem {:sem {:pred :chiedere :subj {:pred :lei}}}}))

(def spec {:synsem {:essere true}})

(populate 1 en/small it/small spec)

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


(def foo (generate-question-and-correct-set {:synsem {:essere true}} "en" "US" "it" "IT"))

(def foo2 (take 5 (repeatedly #(generate-question-and-correct-set {:synsem {:essere true}} "en" "it"))))

(def foo3 (take 5 (repeatedly #(generate-question-and-correct-set {:synsem {:sem {:pred :dormire}}} "en" "it"))))


;; thanks to http://schinckel.net/2014/05/25/querying-json-in-postgres/ for his good info.

;; SELECT surface FROM italiano WHERE synsem->'sem' @> '{"pred":"andare"}';


;; SELECT count(*) FROM (SELECT DISTINCT english.surface AS en, italiano.surface AS it FROM italiano INNER JOIN english ON italiano.structure->synsem->'sem' = english.structure->synsem->'sem' ORDER BY english.surface) AS foo;
    
;; SELECT * FROM (SELECT synsem->'sem'->'pred'::text AS pred,surface FROM english) AS en WHERE en.pred='"andare"';
;; SELECT it.surface  FROM (SELECT synsem->'sem'->'pred' AS pred,surface,synsem FROM italiano) AS it WHERE it.synsem->'sem' @> '{"pred":"andare"}';


;;SELECT italiano.surface,english.surface FROM italiano INNER JOIN english ON italiano.synsem->'sem' = english.synsem->'sem';

;; number of distinct english <-> italiano translation pairs
;; SELECT count(*) FROM (SELECT DISTINCT english.surface AS en, italiano.surface AS it FROM italiano INNER JOIN english ON italiano.synsem->'sem' = english.synsem->'sem' ORDER BY english.surface) AS foo;

;; another view of showing all en <-> it translation pairs
;; SELECT DISTINCT en.surface AS en, it.surface AS it, it.structure->'synsem'->'sem'->'pred' AS pred FROM expression AS en INNER JOIN expression AS it ON (en.structure->'synsem'->'sem') @> (it.structure->'synsem'->'sem') AND en.language='en' AND en.language='en' WHERE it.language='it' ORDER BY pred LIMIT 3000;
