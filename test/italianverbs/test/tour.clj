(ns italianverbs.test.tour
  (:require
   [clojure.test :refer :all]
   [italianverbs.engine :refer [generate]]
   [italianverbs.italiano :as it]
   [italianverbs.morphology :refer [fo]]
   [italianverbs.tour :refer :all]
   [italianverbs.unify :refer (unify unifyc)]
   [korma.core :as k]))

;; TODO: need a shim to return fixtures (simulated database results)
(deftest tour-generate
  (is (not (= "" "foo"))))
