(ns italianverbs.test.borges.writer
  (:refer-clojure :exclude [get get-in resolve merge])
  (:require
   [clojure.core :as core]
   [clojure.test :refer :all]
   [italianverbs.borges.writer :refer :all]
   [italianverbs.engine :refer :all :as engine]
   [italianverbs.english :as en :refer [en]]
   [italianverbs.espanol :as es :refer [es]]
   [italianverbs.italiano :as it :refer [it]]
   [italianverbs.morphology :refer :all]
   [italianverbs.morphology.espanol :as esm]
   [italianverbs.unify :refer [get get-in strip-refs]]
   [italianverbs.borges.writer :refer :all]
   ))

(def spec {:synsem {:essere true}})

(def enrich-function (:enrich @it/small))

(def try (enrich-function {:synsem {:essere true}}))

(def matching-head-lexemes (it/matching-head-lexemes spec))

(def spanish-sentence
  (fo (engine/generate {:synsem {:infl :present 
                                 :sem {:aspect :progressive}}} @es/small)))

(deftest spanish-working
  (is (not (nil? spanish-sentence)))
  (is (not (= "" spanish-sentence))))

(deftest populate-test
  (let [do-populate
        (populate 1 en/small es/small {:synsem {:infl :present :sem {:aspect :progressive}}})]
    (is true))) ;; TODO: add test

(deftest spanish-subject-agreement
  (let [vosotros-comeis (let [example (engine/generate {:synsem {:sem {:subj {:gender :masc :pred :voi} :tense :present :pred :mangiare}}} @es/small)] 
                          {:sem (strip-refs (get-in example [:synsem :sem :subj :gender] :unspecified)) 
                           :surface (fo example)})]
    (is 
     (or 
      (= (:surface vosotros-comeis) "ustedes comen")
      (= (:surface vosotros-comeis) "vosotros comeis")))))


(deftest do-populate
  (let [do-populate (populate 1 en/small es/small {:synsem {:sem {:pred :speak}}})]
    (is (= 1 1)))) ;; stub TODO: fill out test

(defn prep [this-many & [:truncate :false]]
  (if (= truncate true)
    (truncate))
  (populate this-many en/small it/small :top)
  (populate this-many en/small es/small :top))

(deftest do-prep
  (prep 10)
  (is (= 1 1))) ;; stub TODO: fill out test
