(ns italianverbs.parse
 (:refer-clojure :exclude [get-in merge resolve find]))

(require '[clojure.string :as str])
(require '[clojure.tools.logging :as log])
(require '[italianverbs.grammar.italiano :as it-g])
(require '[italianverbs.lexicon.italiano :as it-l])
(require '[italianverbs.morphology :refer (fo fo-ps)])
(require '[italianverbs.morphology.italiano :refer (analyze get-string)])
(require '[italianverbs.over :as over])
(require '[italianverbs.unify :refer (get-in)])

(def it-grammar it-g/grammar)
(def it-lexicon it-l/lexicon)

(defn lookup [token & [lexicon]]
  "return the subset of lexemes that match this token from the lexicon."
  (let [lexicon (if lexicon lexicon it-lexicon)]
    (analyze token (fn [k]
                     (get lexicon k)))))

(defn toks [s]
  (vec (map #(lookup %)
            (str/split s #"[ ']"))))

(declare parse)

(defn parse-at [args index grammar & [runlevel]]
  (let [runlevel (if runlevel runlevel 0)]
    (log/debug (str "parse-at: rl=" runlevel "; i=" index ":" (fo args) "; size args: " (.size args)))
    (if (< index (.size args))
      (concat
       (over/over grammar
                  (parse (subvec args 0 index))
                  (parse (subvec args index (.size args))))
       (parse-at args (+ 1 index) grammar (+ 1 runlevel))))))

(defn parse [arg]
  "return a list of all possible parse trees for a string or a list of lists of maps (a result of looking up in a dictionary a list of tokens from the input string)"
  (cond (string? arg)
        (parse (toks arg))
        
        (and (vector? arg)
             (empty? (rest arg)))
        (first arg)

        (vector? arg)
        (let [result
              (parse-at arg 1 it-grammar)]
          (do
            (if (not (empty? result))
              (log/info (str "parse: " (str/join " + "
                                                 (map (fn [tok]
                                                        (fo tok))
                                                      arg))
                             " => " (fo result))))
            result))

        true
        :error))
