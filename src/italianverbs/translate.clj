(ns italianverbs.translate
  (:refer-clojure :exclude [get-in merge resolve find]))

(require '[italianverbs.grammar.english :as en])
(require '[italianverbs.grammar.italiano :as it])
(require '[italianverbs.morphology :refer [finalize fo fo-ps]])
(require '[italianverbs.unify :refer [get-in]])

(defn get-meaning [input-map]
  "create a language-independent syntax+semantics that can be translated efficiently. The :cat specification helps speed up generation by avoiding searching syntactic constructs that are different from the desired input."
  {:synsem {:sem (get-in input-map [:synsem :sem] :top)
            :cat (get-in input-map [:synsem :cat] :top)}})

(defn translate-it2en [italian]
  (fo (en/generate (get-meaning (first (it/parse italian))))))

(defn translate-en2it [english]
  (fo (it/generate (get-meaning (first (en/parse english))))))

(defn translate [input]
  (let [italian-parse (first (it/parse input))
        english-parses (en/parse input)] ;; lazy: no english parsing will be done unless it's needed (i.e. if italian parsing fails).
    ;; since we can parse it as italian, translate it to english.
    (if italian-parse
      (fo (en/generate (get-meaning italian-parse)))
      ;; otherwise, try the other direction (english to italian):
      (let [english-parse (first (take 1 english-parses))]
        (fo (it/generate (get-meaning english-parse)))))))



