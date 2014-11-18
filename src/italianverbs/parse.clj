(ns italianverbs.parse
 (:refer-clojure :exclude [get-in resolve find]))

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

(defn parse-at [args index grammar & [runlevel bigrams]]
  ;; TODO: currently this algorithm is bottom up:
  ;; Instead, make it bottom up by calling (over) on token bigrams:
  ;; [0 1],[1 2],[2 3],[3 4], ..etc.
  ;; then use that to compute [0 [1 2]],[[0 1] 2],..etc.
  ;; This can be used to compute entire parse while not recomputing anything.
  (if (and (> index 0)
           (< index (.size args)))
    (let [runlevel (if runlevel runlevel 0)
          cached-left-side (get bigrams (subvec args 0 index) :notfound)
          cached-right-side (get bigrams (subvec args index (.size args)) :notfound)
          left-side (if (not (= :notfound cached-left-side))
                      cached-left-side
                      (parse (subvec args 0 index) bigrams))
          right-side (if (not (= :notfound cached-right-side))
                       cached-right-side
                       (parse (subvec args index (.size args)) bigrams))
          bigrams (merge bigrams
                         (if (= :notfound cached-left-side)
                           {(subvec args 0 index)
                            left-side}
                           {})
                         (if (= :notfound cached-right-side)
                           {(subvec args index (.size args))
                            right-side}
                           {}))]
      (log/debug (str "parse-at: rl=" runlevel "; i=" index ":" (fo args) "; size args: " (.size args)))
      (log/info (str "bigrams size: " (.size (keys bigrams))))
      (if (= :notfound cached-left-side)
        (log/info (str "save this(l): [" 0 "," index "] => " (fo left-side)))
        (log/info (str "used cache for(l): [" 0 "," index "] => " (fo left-side))))
      (if (= :notfound cached-right-side)
        (log/info (str "save this(r): [" index "," (.size args) "] => " (fo right-side)))
        (log/info (str "used cache for(r): [" index "," (.size args) "] => " (fo right-side))))
      (lazy-cat
       (over/over grammar
                  left-side
                  right-side)
       (parse-at args (+ 1 index) grammar (+ 1 runlevel) bigrams)))))

(defn parse [arg & [bigrams]]
  "return a list of all possible parse trees for a string or a list of lists of maps (a result of looking up in a dictionary a list of tokens from the input string)"
  (let [bigrams (if bigrams bigrams
                    {})] ;; TODO: create bigram map
    (cond (string? arg)
          (parse (toks arg))
        
          (and (vector? arg)
               (empty? (rest arg)))
          (first arg)

          (vector? arg)
          (let [result
                (parse-at arg 1 it-grammar 0 bigrams)]
            (do
              (if (not (empty? result))
                (log/info (str "parse: " (str/join " + "
                                                   (map (fn [tok]
                                                          (fo tok))
                                                        arg))
                               " => " (fo result))))
              result))

          true
          :error)))

