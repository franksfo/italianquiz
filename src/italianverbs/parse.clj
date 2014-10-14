(ns italianverbs.parse)

(require '[italianverbs.generate :as gen])

(require '[italianverbs.grammar.english :as en])
(require '[italianverbs.grammar.italiano :as it])

(require '[italianverbs.lexicon :as lex])
(require '[italianverbs.morphology :as morph])

(declare lookup-en)
(declare lookup-it)
         
(defn parse [tokens lexicon grammar]
  "return a list of all possible parse trees for the given list of tokens, given the lexicon and grammar."
  (let [looked-up (map #(lookup-en %)
                       tokens)]
    (list)))

(defn lookup-en [token]
  "return the subset set of lexemes that match this token from the lexicon."
  (lex/en token))

(defn lookup-it [token]
  "return the subset set of lexemes that match this token from the lexicon."
  (lex/it token))

