(ns italianverbs.test.engine
  (:refer-clojure :exclude [get-in])
  (:require
   [clojure.test :refer :all]
   [italianverbs.italiano :as it]
   [italianverbs.engine :refer :all :as engine]
   [italianverbs.unify :refer :all]))

(deftest generate-with-essere-true
  (is (= true (get-in (engine/generate {:synsem {:essere true}} it/small :do-enrich true) [:synsem :essere]))))








