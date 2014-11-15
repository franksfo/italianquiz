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

(declare parse)

(defn parse [arg]
  "return a list of all possible parse trees for a string or a list of lists of maps (a result of looking up in a dictionary a list of tokens from the input string)"
  (cond (string? arg)
        (parse (map #(lookup %)
                    (str/split arg #"[ ']")))
        
        (and (seq? arg)
             (empty? (rest arg)))
        (first arg)
        
        (seq? arg)
        ;; TODO: figure out how to do concurrency and memoization.
        (concat ;; consider using lazy-cat here
         (over/over it-grammar
                    (first arg)
                    (parse (rest arg)))
         (over/over it-grammar
                    (parse (butlast arg))
                    (last arg)))

        true
        :error))
