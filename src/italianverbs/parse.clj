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

(defn create-bigram-map [args index grammar]
  (if (< (+ 1 index) (.size args))
    (let [left-side (subvec args index (+ 1 index))
          right-side (subvec args (+ 1 index) (+ 2 index))]
      (merge
       {index
        (over/over grammar left-side right-side)}
       (create-bigram-map args (+ index 1) grammar)))
    {}))

(defn parse-at [args index grammar & [ {runlevel :runlevel
                                        bigrams :bigrams
                                        offset :offset}]]
  ;; TODO: currently this algorithm is bottom up:
  ;; Instead, make it bottom up by calling (over) on token bigrams:
  ;; [0 1],[1 2],[2 3],[3 4], ..etc.
  ;; then use that to compute [0 [1 2]],[[0 1] 2],..etc.
  ;; This can be used to compute entire parse while not recomputing anything.
  (if (and (> index 0)
           (< index (.size args)))
    (let [runlevel (if runlevel runlevel 0)
          bigrams (if bigrams bigrams {})
          left-side (parse (subvec args 0 index)
                           {:bigrams bigrams
                            :left 0
                            :right index
                            :offset offset})
          right-side (parse (subvec args index (.size args))
                            {:bigrams bigrams
                             :left index
                             :right (.size args)
                             :offset (+ 1 offset)})]
      (if (= (.size args) 2)
        (do
          (log/info "")
          (log/info (str " offset=" offset "; index=" index "; size=" (.size args)))
          (log/info (str " left-side: " (fo left-side)))
          (log/info (str " right-side: " (fo right-side)))
          (log/info (str " cached: " (fo (get bigrams offset))))))
      (lazy-cat
       (over/over grammar
                  left-side
                  right-side)
       (parse-at args (+ 1 index) grammar {:runlevel (+ 1 runlevel)
                                           :bigrams bigrams
                                           :offset offset})))))

(defn parse [arg & [{bigrams :bigrams
                    left :left
                    right :right
                    offset :offset}]]
  "return a list of all possible parse trees for a string or a list of lists of maps (a result of looking up in a dictionary a list of tokens from the input string)"
  (let [offset (if offset offset 0)
        bigrams (if bigrams bigrams
                    (if (vector? arg)
                      (create-bigram-map arg 0 it-grammar)))]
    (cond (string? arg)
          (parse (toks arg))
        
          (and (vector? arg)
               (empty? (rest arg)))
          (first arg)

          (vector? arg)
          (let [result
                (parse-at arg 1 it-grammar {:runlevel 0
                                            :bigrams bigrams
                                            :offset offset})]
            (do
              (if (not (empty? result))
                (log/debug (str "parse: " (str/join " + "
                                                   (map (fn [tok]
                                                          (fo tok))
                                                        arg))
                               " => " (fo result))))
              result))

          true
          :error)))

