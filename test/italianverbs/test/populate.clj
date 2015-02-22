(ns italianverbs.test.populate
  (:refer-clojure :exclude [get-in])
  (:require
   [clojure.data.json :as json]
   [clojure.test :refer :all]
   [italianverbs.engine :refer [generate]]
   [italianverbs.english :as en]
   [italianverbs.italiano :as it]
   [italianverbs.morphology.english :as enm]
   [italianverbs.morphology.italiano :as itm]
   [italianverbs.morphology :refer [fo]]
   [italianverbs.populate :refer :all]
   [italianverbs.unify :as unify :refer (fail? get-in strip-refs unify)]
   [korma.core :as k]))


;; TODO: add tests

;; find Italian expressions with no English counterpart:
(def italian-singles 
  "SELECT DISTINCT it.surface AS it, 
                   it.structure->'synsem'->'sem'->'pred' AS pred 

              FROM expression AS en 

        RIGHT JOIN expression AS it 
                ON (en.structure->'synsem'->'sem') @> (it.structure->'synsem'->'sem') 
               AND en.language='en' 

             WHERE it.language='it' 
               AND en.surface IS NULL 
          ORDER BY pred;")

(def italian-singles-preds
  "SELECT DISTINCT it.structure->'synsem'->'sem'->'pred' AS pred 

              FROM expression AS en 

        RIGHT JOIN expression AS it 
                ON (en.structure->'synsem'->'sem') @> (it.structure->'synsem'->'sem') 
               AND en.language='en' 

             WHERE it.language='it' 
               AND en.surface IS NULL 
          ORDER BY pred;")

;; You can diagnose problems with, e.g.:
;;
;; (def foo (generate {:synsem {:cat :verb :sem {:pred :dormire}}} @en/small))
