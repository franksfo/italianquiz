(ns italianverbs.test.grammar
  (:require
   [italianverbs.morphology :as morph]
   [italianverbs.unify :as unify])
  (:use [clojure.test]
        [italianverbs.generate]
        [italianverbs.grammar]))

(deftest io-dormo
  (let [result (first (take 1
                            (generate
                             (unify s-present {:synsem {:sem {:pred :dormire
                                                              :subj {:pred :io}}}}))))]
    (is (not (unify/fail? result)))
    (is (= (unify/get-in (morph/finalize result) '(:italian)) "io dormo"))
    (is (= (unify/get-in (morph/finalize result) '(:english)) "I sleep"))))

(deftest fare-bene
  (let [result (over vp-plus-adverb "fare" "bene")]
    (is (not (unify/fail? result)))))

(deftest a-vendere-la-casa
  (let [result (over prep-plus-verb-inf "a"
                         (over vp
                                   "vendere"
                                   (over np "la" "casa")))]
    (is (not (unify/fail? result)))))

(deftest fare-bene-a-vendere-la-casa
  (let [fare-bene (over vp-plus-adverb "fare" "bene")
        a-vendere-la-casa
        (over prep-plus-verb-inf "a"
                         (over vp
                                   "vendere"
                                   (over np "la" "casa")))]
    (let [result
          (over vp-past fare-bene a-vendere-la-casa)]
      (is (not (unify/fail? result))))))



(deftest adj-agreement-with-subject
  "adjectives must agree with subjects - tests this behavior with intermediate 'meno ricco' between the subject and the adjective."
  (let [lei-e-piu-ricca-di-giorgio
        (over s-present "lei"
              (over vp "essere"
                    (over intensifier-phrase "più"
                          ;; error: (over adj-phrase (second (it "ricco")))
                          (over adj-phrase "ricco"
                                (over prep-phrase "di" "Giorgio")))))]
    (is (= (morph/strip (morph/get-italian (get-in (first lei-e-piu-ricca-di-giorgio) '(:italian))))
           "lei è più ricca di Giorgio"))))
