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
  (is (= "un gatto" (fo (first (parse "un gatto"))))))
 
(deftest parse-test-2
  (is (= (fo (first (parse "Antonio dormirà")))
         "Antonio dormirà")))

(deftest parse-test-3
  (let [result (parse "il gatto nero")]
    (is (> (.size result) 0))
    (is (= (get-in (first result) [:synsem :sem :pred])
           :gatto))))

(deftest parse-test-4
  (let [result (parse "il gatto nero dorme")]
    (is (> (.size result) 0))
    (is (= (get-in (first result) [:synsem :sem :pred])
           :dormire))
    (is (= (get-in (first result) [:synsem :sem :subj :pred])
           :gatto))
    (is (= (get-in (first result) [:synsem :sem :subj :mod :pred])
           :nero))))

(deftest tokenization-1
  "there should be only 2 tokens, even though there's 3 tokens at first according to initial tokenization."
  (let [result (toks "la sua birra")]
    (is (= (.size result) 2))))

(deftest tokenization-1
  "there should be 3 tokens, for each of the tokens at first according to initial tokenization."
  (let [result (toks "il gatto nero")]
    (is (= (.size result) 3))))




