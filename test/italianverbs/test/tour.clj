(ns italianverbs.test.tour
  (:require
   [clojure.test :refer :all]
   [italianverbs.italiano :as it]
   [italianverbs.tour :refer :all]
   [korma.core :as k]))

;; TODO: need a shim to return fixtures (simulated database results)
(deftest tour-generate
  (is (not (= "" "foo"))))
