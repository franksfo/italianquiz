(ns italianverbs.translate
  (:refer-clojure :exclude [get-in merge resolve find]))
(require '[clojure.tools.logging :as log])
(require '[italianverbs.grammar.english :as en])
(require '[italianverbs.grammar.italiano :as it])
(require '[italianverbs.morphology :refer [finalize fo fo-ps]])
(require '[italianverbs.unify :refer [get-in]])

(declare get-meaning)

(defn translate [input]
  (let [return-val
        (map fo
             (lazy-cat
              (en/generate (get-meaning (it/parse input)))
              (it/generate (get-meaning (en/parse input)))))]
    (if (= 1 (.size return-val))
      (first return-val))))

(defn get-meaning [input-map]
  "create a language-independent syntax+semantics that can be translated efficiently. The :cat specification helps speed up generation by avoiding searching syntactic constructs that are different from the desired input."
  (if (seq? input-map)
    (map get-meaning
         input-map)
    {:synsem {:cat (get-in input-map [:synsem :cat] :top)
              :sem (get-in input-map [:synsem :sem] :top)
              :subcat (get-in input-map [:synsem :subcat] :top)}}))

(defn parse [input]
  (lazy-cat (it/parse input)
            (en/parse input)))

(defn lookup [input]
  (lazy-cat (it/lookup input)
            (en/lookup input)))

