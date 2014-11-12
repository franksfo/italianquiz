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


(defn parse-tokenz [toks grammar]
  toks)

(defn parse [input-string & [more-tokens]]
  "return a list of all possible parse trees for the given list of tokens, given the lexicon and grammar."
  (let [lexicon it-lexicon
        grammar it-grammar
        tokens (str/split input-string #"[ ']")
        looked-up (if more-tokens
                    (cons (map #(lookup % lexicon)
                               tokens)
                          more-tokens)
                    (map #(lookup % lexicon)
                         tokens))]
    (parse-tokens looked-up grammar)))

(defn parse-tokens [tokens grammar]
  (cond (and (= (.size tokens) 2)
             (not (empty? (nth tokens 0)))
             (not (empty? (nth tokens 1))))
        (over/over grammar
                   (nth tokens 0)
                   (nth tokens 1))

        (or (and (= (.size tokens) 1)
                 (not (empty? (nth tokens 0))))
            (and (= (.size tokens) 2)
                 (empty? (nth tokens 1))))
        (over/over grammar
                   (nth tokens 0))

        (and (= (.size tokens) 2)
             (empty? (nth tokens 0))
             (not (empty? (nth tokens 1))))
        (over/over grammar
                   :top
                   (nth tokens 1))

        true
        nil))

