(ns italianverbs.test.parse
  (:refer-clojure :exclude [get-in merge resolve find]))

(require '[clojure.test :refer :all])


;; not yet needed, but might be some day.
;(require '[italianverbs.generate :as gen])

(require '[italianverbs.grammar.english :as en-g])
(require '[italianverbs.grammar.italiano :as it-g])

;(require '[italianverbs.lexicon.english :as en-l])
(require '[italianverbs.lexicon.italiano :as it-l])
;; not yet needed, but might be some day.
;(require '[italianverbs.lexiconfn :as lexfn])
(require '[italianverbs.morphology :refer [fo]])
(require '[italianverbs.parse :refer :all])
(require '[italianverbs.unify :refer (get-in)])

(deftest parse-test-1
  (is (= "un gatto" (fo (parse "un gatto")))))
 
(deftest parse-test-2
  (is (contains? (set (fo (parse "Antonio dormire"))) "Antonio dormirà")))

(deftest parse-test-3
  (let [result (parse "il" (parse "gatto" "nero"))]
    (is (> (.size result) 0))
    (is (= (get-in (first result) [:synsem :sem :pred])
           :gatto))))





