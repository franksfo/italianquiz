(ns italianverbs.test.parse)

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

(deftest parse-test-1
  (is (= "un gatto" (fo (parse "un gatto" it-lexicon it-grammar)))))
 
(deftest parse-test-2
  (is (contains? (set (map fo (parse "Antonio dormire" it-lexicon it-grammar))) "Antonio dormirà")))


