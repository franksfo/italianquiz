(ns italianverbs.test.borges.writer
  (:refer-clojure :exclude [get get-in resolve merge])
  (:require
   [clojure.core :as core]
   [clojure.test :refer :all]
   [italianverbs.borges.writer :refer :all]
   [italianverbs.engine :refer :all :as engine]
   [italianverbs.english :as en]
   [italianverbs.espanol :as es]
   [italianverbs.italiano :as it]
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


(deftest do-fill
  (let [do-fill (fill 1 en/small es/small {:synsem {:sem {:pred :speak}}})]
    (is (= 1 1)))) ;; stub TOOD: fill out test
