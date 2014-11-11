(ns italianverbs.parse
 (:refer-clojure :exclude [get-in merge resolve find]))

(require '[clojure.string :as str])
(require '[italianverbs.grammar.italiano :as it-g])
(require '[italianverbs.lexicon.italiano :as it-l])
(require '[italianverbs.morphology :refer (fo fo-ps)])
(require '[italianverbs.morphology.italiano :refer (analyze get-string)])
(require '[italianverbs.over :as over])

(def it-grammar it-g/grammar)
(def it-lexicon it-l/lexicon)

(defn lookup [token & [lexicon]]
  "return the subset of lexemes that match this token from the lexicon."
  (let [lexicon (if lexicon lexicon it-lexicon)]
    (analyze token (fn [k]
                     (get lexicon k)))))

(defn parse [input-string & [ lexicon grammar ]]
  "return a list of all possible parse trees for the given list of tokens, given the lexicon and grammar."
  (if (or (nil? lexicon)
          (nil? grammar))
    ;; if not lexicon or grammar supplied, assume italian (for now)
    (parse input-string it-lexicon it-grammar)

    ;; else, grammar and lexicon supplied:
    (let [tokens (str/split input-string #"[ ']")
          looked-up (map #(lookup % lexicon)
                         tokens)]
      (cond (and (= (.size looked-up) 2)
                 (not (empty? (nth looked-up 0)))
                 (not (empty? (nth looked-up 1))))
            (over/over grammar
                       (nth looked-up 0)
                       (nth looked-up 1))

            (or (and (= (.size looked-up) 1)
                     (not (empty? (nth looked-up 0))))
                (and (= (.size looked-up) 2)
                     (empty? (nth looked-up 1))))
            (over/over grammar
                       (nth looked-up 0))

            (and (= (.size looked-up) 2)
                 (empty? (nth looked-up 0))
                 (not (empty? (nth looked-up 1))))
            (over/over grammar
                       :top
                       (nth looked-up 1))

            true
            nil))))
