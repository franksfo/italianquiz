(ns italianverbs.parse
 (:refer-clojure :exclude [get-in merge resolve find]))

(require '[clojure.string :as str])
(require '[italianverbs.grammar.italiano :as it-g])
(require '[italianverbs.lexicon.italiano :as it-l])
(require '[italianverbs.morphology :refer (fo fo-ps)])
(require '[italianverbs.morphology.italiano :refer (analyze get-string)])
(require '[italianverbs.over :as over])

(declare parse-tokens)

(def it-grammar it-g/grammar)
(def it-lexicon it-l/lexicon)

(defn lookup [token & [lexicon]]
  "return the subset of lexemes that match this token from the lexicon."
  (let [lexicon (if lexicon lexicon it-lexicon)]
    (analyze token (fn [k]
                     (get lexicon k)))))

(defn parse [a & [ b ]]
  "return a list of all possible parse trees for the given list of tokens, given the lexicon and grammar."
  (let [grammar it-grammar]
    (cond (and (seq? a)
               (nil? b))
          (parse (first a)
                 (second a))

          (string? a)
          (parse (map #(lookup %)
                      (str/split a #"[ ']"))
                 b)

          (string? b)
          (parse a
                 (map #(lookup %)
                      (str/split b #"[ ']")))

          (map? a)
          (parse (list a) b)
          
          (map? b)
          (parse a (list b))

          true
          (parse-tokens a b grammar))))

(defn parse-tokens [a b grammar]
  (over/over grammar a b))

