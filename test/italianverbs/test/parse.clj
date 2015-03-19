(ns italianverbs.test.parse
  (:refer-clojure :exclude [get-in merge resolve find]))

(require '[clojure.test :refer :all])
(require '[clojure.string :as str])

;; not yet needed, but might be some day.
;(require '[italianverbs.generate :as gen])

(require '[italianverbs.english :as en])
(require '[italianverbs.italiano :as it])

(require '[italianverbs.morphology :refer [fo]])
(require '[italianverbs.parse :refer :all])
(require '[italianverbs.unify :refer (get-in)])

(deftest tokenization-1
  "there should be only 2 tokens, even though there's 3 tokens at first according to initial tokenization."
  (let [result (toks "la sua birra" it/lexicon it/lookup)]
    (is (= (.size result) 2))))

(deftest tokenization-1
  "there should be 3 tokens, for each of the tokens according to initial tokenization (there is no way to combine any initial tokens in to larger tokens as there was in the test immediately above."
  (let [result (toks "il gatto nero" it/lexicon it/lookup)]
    (is (= (.size result) 3))))
 
(deftest parse-test-1-en
  (is (= "a cat" (fo (first (en/parse "a cat"))))))

