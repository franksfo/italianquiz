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

(defn create-unigram-map [args index]
  (if (< index (.size args))
    (merge
     {[index (+ 1 index)]
      (subvec args index (+ 1 index))}
     (create-unigram-map args (+ 1 index)))))

(defn create-bigram-map [args index grammar]
  (if (< (+ 1 index) (.size args))
    (let [left-side (subvec args index (+ 1 index))
          right-side (subvec args (+ 1 index) (+ 2 index))]
      (merge
       {[index (+ 2 index)]
        (over/over grammar left-side right-side)}
       (create-bigram-map args (+ index 1) grammar)))
    (create-unigram-map args 0)))

(defn create-trigram-map [args index grammar & [bigrams]]
  (let [bigrams (if bigrams bigrams (create-bigram-map args index grammar))]
    (if (< (+ 2 index) (.size args))
      (do
        (log/debug (str "over(1): " (fo (get bigrams index))))
        (log/debug (str "over(2): " (fo (get bigrams (+ 1 index)))))
        (merge
         {[index (+ 3 index)]
          (lazy-cat
           ;; [a b | c ]
           (over/over grammar
                      (get bigrams [index (+ 2 index)])
                      (get bigrams [(+ 2 index) (+ 3 index)]))
           ;; [a | b c ]
           (over/over grammar 
                      (subvec args index (+ 1 index))
                      (get bigrams [(+ 1 index) (+ 3 index)])))}
         (create-trigram-map args (+ index 1) grammar bigrams)))
      bigrams)))

(defn create-ngram-map-middle [left ngrams grammar split-at length]
  (if (< split-at length)
    (do
      (log/debug (str "create-ngram-middle: (" left "," split-at "," length ")"))
      (lazy-cat
       (over/over grammar
                  (get ngrams [left split-at] '())
                  (get ngrams [(+ split-at left) (+ length left)] '()))

       (create-ngram-map-middle left ngrams grammar (+ 1 split-at) length)))))

(defn create-ngram-map [args left ngrams grammar split-at length]
  (if (< split-at length)
    (do
      (lazy-cat
       (let [left-parses (get ngrams [left (+ left (- split-at 0))] '())
             right-parses (get ngrams [(+ left split-at) (- (.size args) 0)] '())]
         (if (and (not (empty? left-parses))
                  (not (empty? right-parses)))
           (do
             (log/info (str "create-ngram-map: " 
                            (str/join " " (range left (+ left split-at))) " | "
                            (str/join " " (range (+ left split-at) (+ left length)))))

             (log/info (str "create-ngram-map: " 
                            (str/join " " (map #(fo (subvec args % (+ 1 %)))
                                               (range left (+ left split-at))))
                            " | "
                            (str/join " " (map #(fo (subvec args % (+ 1 %)))
                                               (range (+ left split-at) (+ left length))))))

             (log/info (str "create-ngram-map: over(1): " [left (+ left (- split-at 0))]))
             (log/info (str "create-ngram-map: over(1): " (fo (get ngrams
                                                                   [left (+ left (- split-at 0))]))))
             (log/info (str "create-ngram-map: over(2): " [(+ left split-at) (- (.size args) 0)]))
             (log/info (str "create-ngram-map: over(2): " (fo (get ngrams
                                                                   [(+ left split-at) (- (.size args) 0)]))))
             (over/over grammar left-parses right-parses))))
       (create-ngram-map args left ngrams grammar (+ 1 split-at) length)))))

(defn create-xgram-map [args x index grammar & [nminus1grams]]
  (cond (= x 2) (create-bigram-map args index grammar)
        (= x 3) (create-trigram-map args index grammar)
        true
        (let [nminus1grams (if nminus1grams nminus1grams
                               (create-xgram-map args (- x 1) 0 grammar))]
          (if (< (+ x index) (+ 1 (.size args)))
            (do
              (log/info (str "create-xgram-map: x:" x "; index:" index ";size=" (.size args)))
              (merge
               {[index (+ x index)]
                (create-ngram-map args index nminus1grams grammar 1 x)}
               (create-xgram-map args x (+ index 1) grammar nminus1grams)
               nminus1grams))
            nminus1grams))))

(defn parse [arg & [{all :all
                     offset :offset
                     length :length
                     ngrams :ngrams}]]
  "return a list of all possible parse trees for a string or a list of lists of maps (a result of looking up in a dictionary a list of tokens from the input string)"
  (let [offset (if offset offset 0)
        all (if all all (if (vector? arg)
                          arg))
        length (if length length (if (vector? arg)
                                   (.size arg)))]
    (log/debug (str "parse: arg: " (fo arg)))
    (log/debug (str "parse: all: " (fo all)))
    (cond (string? arg)
          (parse (toks arg))
        
          (and (vector? arg)
               (empty? (rest arg)))
          (first arg)

          (vector? arg)
          (get (create-xgram-map arg (.size arg) 0 it-grammar)
               [0 (.size arg)])
          true
          :error)))

(def init-workbook (parse "io dorme"))

