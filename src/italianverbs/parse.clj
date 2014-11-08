(ns italianverbs.parse
 (:refer-clojure :exclude [get-in merge resolve find]))

(require '[clojure.string :as str])

(require '[italianverbs.generate :as gen])

(require '[italianverbs.grammar.english :as en-g])
(require '[italianverbs.grammar.italiano :as it-g])

(require '[italianverbs.lexicon.english :as en-l])
(require '[italianverbs.lexicon.italiano :as it-l])
(require '[italianverbs.lexiconfn :as lexfn])

(require '[italianverbs.morphology :refer (fo fo-ps)])
(require '[italianverbs.morphology.italiano :refer (analyze)])
(require '[italianverbs.over :as over])
(require '[italianverbs.unify :refer :all])

(declare lookup)

(def en-grammar en-g/grammar)
(def it-grammar it-g/grammar)

(def en-lexicon en-l/lexicon)
(def it-lexicon it-l/lexicon)

(defn parse [input-string & [ lexicon grammar ]]
  "return a list of all possible parse trees for the given list of tokens, given the lexicon and grammar."
  (if (or (nil? lexicon)
          (nil? grammar))
    ;; if not lexicon or grammar supplied, assume italian (for now)
    (parse input-string it-lexicon it-grammar)

    ;; else, grammar and lexicon supplied:
    (let [tokens (str/split input-string #"[ ']")
          looked-up (map #(lookup lexicon %)
                         tokens)]
      (cond (and (not (empty? (nth looked-up 0)))
                 (not (empty? (nth looked-up 1))))
            (over/over grammar
                       (nth looked-up 0)
                       (nth looked-up 1))

            (not (empty? (nth looked-up 0)))
            (over/over grammar
                       (nth looked-up 0))

            (not (empty? (nth looked-up 1)))
            (over/over grammar
                       :top
                       (nth looked-up 1))
            
            true
            nil))))

(defn parse-both [input-string]
  {:english (parse input-string en-lexicon en-grammar)
   :italiano (parse input-string it-lexicon it-grammar)})
  
(defn lookup [lexicon token]
  "return the subset of lexemes that match this token from the lexicon."
  (analyze token (fn [k]
                   (get lexicon k))))
