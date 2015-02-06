(ns italianverbs.test.engine
  (:require
   [clojure.test :refer :all]
   [italianverbs.italiano :as it]
   [italianverbs.engine :refer :all :as engine]))

(deftest generate-with-essere-true
  (is (= true (engine/generate {:synsem {:essere true}} it/small :do-enrich true) [:synsem :essere])))







