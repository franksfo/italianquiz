(ns italianverbs.test.engine
  (:refer-clojure :exclude [get get-in merge resolve])
  (:require
   [clojure.test :refer :all]
   [italianverbs.engine :as engine]
   [italianverbs.italiano :as it :refer :all]
   [italianverbs.morphology :refer :all]
   [italianverbs.unify :refer :all]))

(deftest enrich-should-return-map-or-list-of-maps
  (is (or (map? ((:enrich @it/small) {:synsem {:essere true}})))))

(deftest generate-with-essere-true
  (is (= true 
         (get-in 

          (engine/generate {:synsem {:essere true}} it/small :do-enrich true) 

          [:synsem :essere]))))








