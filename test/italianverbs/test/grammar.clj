(ns italianverbs.test.grammar
  (:require
   [italianverbs.generate :as gen]
   [italianverbs.unify :as unify])
  (:use [clojure.test]
        [italianverbs.grammar]))

(deftest io-dormo
  (let [result (first (take 1
                            (gen/generate
                             (unify s-present {:synsem {:sem {:pred :dormire
                                                              :subj {:pred :io}}}}))))]
    (is (not (unify/fail? result)))
    (is (= (unify/get-in (gen/finalize result) '(:italian)) "io dormo"))
    (is (= (unify/get-in (gen/finalize result) '(:english)) "I sleep"))))



