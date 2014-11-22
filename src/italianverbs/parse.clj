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

(defn parse-at [args & [ {all :all
                          bigrams :bigrams
                          grammar :grammar
                          split-at :split-at
                          left :left
                          offset :offset
                          absolute-offset :absolute-offset
                          right :right
                          runlevel :runlevel}]]
  ;; TODO: currently this algorithm is bottom up:
  ;; Instead, make it bottom up by calling (over) on token bigrams:
  ;; [0 1],[1 2],[2 3],[3 4], ..etc.
  ;; then use that to compute [0 [1 2]],[[0 1] 2],..etc.
  ;; This can be used to compute entire parse while doing no parsing of the same material more than once.

  (if (and (> split-at 0)
           (< split-at (.size args)))
      (let [runlevel (if runlevel runlevel 0)
            bigrams (if bigrams bigrams {})
            left (if left left 0)
            left (if right right 0)
            left-side (parse (subvec args 0 split-at)
                             {:all all
                              :absolute-offset absolute-offset
                              :bigrams bigrams
                              :left left
                              :right split-at
                              :offset offset})
            right-side (parse (subvec args split-at (.size args))
                              {:all all
                               :absolute-offset (+ split-at absolute-offset)
                               :bigrams bigrams
                               :left split-at
                               :right (.size args)
                               :offset (+ 1 offset)})]

        (log/info "")
        (log/debug (str "parse-at: all : " (fo all)))
        (log/debug (str "parse-at: args: " (fo args)))
        (log/info (str "parse-at: args2: " (fo (subvec all absolute-offset (+ absolute-offset (.size args))))))

        (log/info (str " offset=" offset "; split-at=" split-at "; size=" (.size args)))
        (log/info (str " absolute-offset: " absolute-offset))
        (log/info (str " left-side: " (fo left-side)))
        (log/info (str " right-side: " (fo right-side)))
;        (log/info (str " cached: " (fo (get bigrams offset))))

;        (log/info (str "parse-at: offs: " offset))
;        (log/info (str "parse-at: left: " left))
        (lazy-cat
         (if (and true (= (.size args) 2) (get bigrams absolute-offset))
           (get bigrams absolute-offset)
           (over/over grammar
                      left-side
                      right-side))
         (parse-at args {:all all
                         :absolute-offset absolute-offset
                         :bigrams bigrams
                         :grammar grammar
                         :split-at (+ 1 split-at)
                         :left left
                         :offset offset
                         :right right
                         :runlevel (+ 1 runlevel)})))))

(defn parse [arg & [{all :all
                     absolute-offset :absolute-offset
                     bigrams :bigrams
                     left :left
                     right :right
                     offset :offset}]]
  "return a list of all possible parse trees for a string or a list of lists of maps (a result of looking up in a dictionary a list of tokens from the input string)"
  (let [offset (if offset offset 0)
        absolute-offset (if absolute-offset absolute-offset offset)
        all (if all all (if (vector? arg)
                          arg))
        left (if left left 0)
        right (if right right (if (vector? arg)
                                (.size arg)))
        bigrams (if bigrams bigrams
                    (if (vector? arg)
                      (create-bigram-map arg 0 it-grammar)))]
    (log/debug (str "parse: arg: " (fo arg)))
    (log/debug (str "parse: all: " (fo all)))
    (cond (string? arg)
          (parse (toks arg))
        
          (and (vector? arg)
               (empty? (rest arg)))
          (first arg)

          (vector? arg)
          (let [result
                (parse-at arg {:all all
                               :absolute-offset absolute-offset
                               :bigrams bigrams
                               :grammar it-grammar
                               :split-at 1
                               :left left
                               :offset offset
                               :right right
                               :runlevel 0
                               })]
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

