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

(defn create-trigram-map [args index grammar bigrams]
  (if (< (+ 2 index) (.size args))
    (let []
      (log/debug (str "over(1): " (fo (get bigrams index))))
      (log/debug (str "over(2): " (fo (get bigrams (+ 1 index)))))
      (merge
       {index
        (concat
         (over/over grammar 
                    (get bigrams index)
                    (subvec args (+ 2 index) (+ 3 index)))
         (over/over grammar 
                    (subvec args index (+ 1 index))
                    (get bigrams (+ 1 index))))}
       (create-trigram-map args (+ index 1) grammar bigrams)))
    {}))

(defn parse-at [all & [ {length :length
                         bigrams :bigrams
                         grammar :grammar
                         split-at :split-at
                         trigrams :trigrams
                         offset :offset}]]
  (if (and (> split-at 0)
           (< split-at length))
      (let [args (subvec all offset (+ offset length))
            left-side (parse
                       (subvec args 0 split-at)
                       {:all all
                        :length split-at
                        :offset offset
                        :bigrams bigrams
                        :trigrams trigrams})
            right-side (parse
                        (subvec args split-at length)
                        {:all all
                         :offset (+ split-at offset)
                         :bigrams bigrams
                         :trigrams trigrams
                         :left split-at
                         :length (- (.size args) split-at)
                         :right (.size args)})]

        (log/info (str "parse-at: " (fo (subvec all offset (+ offset length)))))

        (lazy-cat
         (cond (= length 2)
               (let [retval (get bigrams offset)]
                 (do (if (not (empty? retval))
                       (log/info (str " =2> " (fo retval))))
                     retval))
               (= length 3)
               (let [retval (get trigrams offset)]
                 (do (if (not (empty? retval))
                       (log/info (str " =3> " (fo retval))))
                     retval))
               true
               (let [retval (over/over grammar left-side right-side)]
                 (do
                   (if (not (empty? retval))
                     (log/info (str " =m> " (fo retval))))
                   retval)))
         (parse-at all {:offset offset
                        :bigrams bigrams
                        :trigrams trigrams
                        :length length
                        :grammar grammar
                        :split-at (+ 1 split-at)})))))

(defn parse [arg & [{all :all
                     offset :offset
                     length :length
                     bigrams :bigrams
                     trigrams :trigrams}]]
  "return a list of all possible parse trees for a string or a list of lists of maps (a result of looking up in a dictionary a list of tokens from the input string)"
  (let [offset (if offset offset 0)
        all (if all all (if (vector? arg)
                          arg))
        length (if length length (if (vector? arg)
                                   (.size arg)))
        bigrams (if bigrams bigrams
                    (if (vector? arg)
                      (create-bigram-map arg 0 it-grammar)))
        trigrams (if trigrams trigrams
                     (if (vector? arg)
                       (create-trigram-map arg 0 it-grammar bigrams)))]

    (log/debug (str "parse: arg: " (fo arg)))
    (log/debug (str "parse: all: " (fo all)))
    (cond (string? arg)
          (parse (toks arg))
        
          (and (vector? arg)
               (empty? (rest arg)))
          (first arg)

          (vector? arg)
          (let [result
                (parse-at all {:offset offset
                               :bigrams bigrams
                               :grammar it-grammar
                               :length length
                               :split-at 1
                               :trigrams trigrams})]
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

(def init-workbook (parse "io dorme"))

