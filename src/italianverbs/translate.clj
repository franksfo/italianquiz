(ns italianverbs.translate
  (:refer-clojure :exclude [get-in merge resolve find]))

(require '[italianverbs.grammar.english :as en])
(require '[italianverbs.grammar.italiano :as it])
(require '[italianverbs.morphology :refer [finalize fo fo-ps]])
(require '[italianverbs.unify :refer [get-in]])

(defn get-meaning [input-map]
  "create a language-independent syntax+semantics that can be translated efficiently. The :cat specification helps speed up generation by avoiding searching syntactic constructs that are different from the desired input."
  (if (seq? input-map)
    (map get-meaning
         input-map)
    {:synsem {:cat (get-in input-map [:synsem :cat] :top)
              :sem (get-in input-map [:synsem :sem] :top)
              :subcat (get-in input-map [:synsem :subcat] :top)}}))

(defn translate-it2en [italian]
  (fo (en/generate (get-meaning (first (it/parse italian))))))

(defn translate-en2it [english]
  (fo (it/generate (get-meaning (first (en/parse english))))))

(defn translate [input]
  (if (seq? input)
    (map translate
         input)
    (let [italian-parse (it/parse input)]
      ;; since we can parse it as italian, translate it to english:
      (if (not (empty? italian-parse))
        (fo (en/generate (get-meaning italian-parse)))
        ;; otherwise, try parsing it as english and translating it to italian:
        (fo (it/generate (get-meaning (en/parse input))))))))

(defn parse [input]
  (let [italian-parses (it/parse input)]
    (if (not (empty? italian-parses))
      italian-parses
      (en/parse input))))

(defn lookup [input]
  (lazy-cat (it/lookup input)
            (en/lookup input)))
