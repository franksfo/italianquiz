(ns italianverbs.test.grammar
  (:require
   [italianverbs.generate :as gen]
   [italianverbs.morphology :as morph]
   [italianverbs.unify :as unify])
  (:use [clojure.test]
        [italianverbs.grammar]))

(deftest io-dormo
  (let [result (first (take 1
                            (gen/generate
                             (unify s-present {:synsem {:sem {:pred :dormire
                                                              :tense :present
                                                              :subj {:pred :io}}}}))))]
    (is (not (unify/fail? result)))
    (is (= (unify/get-in (morph/finalize result) '(:italian)) "io dormo"))
    (is (= (unify/get-in (morph/finalize result) '(:english)) "I sleep"))))



