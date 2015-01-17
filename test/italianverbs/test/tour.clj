(ns italianverbs.test.tour
  (:refer-clojure :exclude [get-in])
  (:require
   [clojure.test :refer :all]
   [italianverbs.engine :refer [generate]]
   [italianverbs.italiano :as it]
   [italianverbs.morphology :refer [fo]]
   [italianverbs.tour :refer :all]
   [italianverbs.unify :refer (fail? get-in strip-refs unify)]
   [korma.core :as k]))

;; TODO: need a shim to return fixtures (simulated database results)
(deftest tour-generate
  (is (not (= "" "foo"))))

(def killer-spec 
  {:comp {:phrasal false} 
   :head {:phrasal :top} 
   :synsem {:sem {:tense :past 
                  :aspect :perfect 
                  :pred :parlare 
                  :subj {:mass false 
                         :furniture false 
                         :pred :loro 
                         :place false 
                         :drinkable false 
                         :human true 
                         :animate true 
                         :speakable false 
                         :activity false 
                         :physical-object true 
                         :buyable false 
                         :legible false 
                         :artifact false 
                         :gender :masc 
                         :edible false 
                         :part-of-human-body false} 
                  :obj :unspec} 
            :subcat []
            :cat :verb}})

(deftest enrich-test
  (let [took-time (time (fo (generate (enrich killer-spec) it/small)))]))


