(ns italianverbs.forest
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [clojure.set :refer :all]
   [clojure.tools.logging :as log]
   ;; italianverbs.config is not used yet but hopefully will be in the future.
   [italianverbs.config :as config]
   [italianverbs.lexiconfn :refer (sem-impl)]
   [italianverbs.morphology :refer (fo-ps)]
   [italianverbs.over :refer (overc overh)]
   [italianverbs.unify :refer :all]))

(defn map-lexicon [head lexicon]
  "TODO: determine if this is done lazily or not: it should. If not, will have to do laziness with (lazy-seq (cons..) over the lexicon."
  (filter (fn [lexeme]
            (not (fail? lexeme)))
          (map (fn [lexeme]
                 (unifyc
                  head
                  (unifyc
                   {:synsem {:sem (sem-impl (get-in head '(:synsem :sem)))}}
                   lexeme)))
               lexicon)))

(declare lightning-bolt)

(defn comp-phrases [phrases-with-heads all-phrases lexicon]
  (if (not (empty? phrases-with-heads))
    (let [phrase-with-head (first phrases-with-heads)
          remove-some-paths
          (dissoc-paths
           (get-in phrase-with-head '(:comp))
           '((:synsem :subcat)
             (:english :initial)
             (:italian :initial)))
          debug (log/debug (str "comp-phrases: phrase-with-head (minus subcat, english-initial and italian-initial): " remove-some-paths))]
      (cond

       ;; Lexemes with certain grammatical categories (for now, only :det) cannot be heads of
       ;; a phrase, but only lexemes that are complements of a phrase, so save time by not trying
       ;; to recursively generate phrases that are headed with such lexemes.
       (= :det (get-in remove-some-paths '(:synsem :cat)))
       (do
         (log/trace (str "this cannot be head of a phrase: " remove-some-paths))
         (comp-phrases (rest phrases-with-heads) all-phrases lexicon))

       true
       (lazy-cat
        (overc phrase-with-head
               (lightning-bolt
                remove-some-paths
                lexicon
                all-phrases
                0))
        (comp-phrases (rest phrases-with-heads) all-phrases lexicon))))))

(defn get-bolts [heads lexicon phrases depth]
  (if (not (empty? heads))
    (lazy-seq
     (cons
      (lightning-bolt (first heads) lexicon phrases depth)
      (get-bolts (rest heads) lexicon phrases depth)))))

(defn get-parents-with-phrasal-head [headed-parents-at-this-depth lexicon phrases depth]
  (if (not (empty? headed-parents-at-this-depth))
    (lazy-cat
     (let [bolts (lightning-bolt (get-in (first headed-parents-at-this-depth) '(:head))
                                 lexicon phrases (+ 1 depth))]
       (overh headed-parents-at-this-depth bolts))
     (get-parents-with-phrasal-head (rest headed-parents-at-this-depth) lexicon phrases depth))))

;; TODO: move this to inside lightning-bolt.
(defn decode-gen-ordering2 [rand2]
  (cond (= rand2 0)
        "hLcP + hPcP"
        true
        "hPcP + hLcP"))

;; TODO: make option to just call (lazy-cat seq1 seq2 seq3) for efficiency:
;; this is simply a diagnostic tool.
(defn try-all-debug [seq1 seq2 seq3 seq1-label seq2-label seq3-label]
  (if (not (empty? seq1))
    (let [first-of-seq1 (first seq1)]
      (do
        (log/debug (str "seq1 ("seq1-label") has a candidate:" (fo-ps first-of-seq1)))
        (lazy-seq
         (cons
          first-of-seq1
          (try-all-debug (rest seq1) seq2 seq3 seq1-label seq2-label seq3-label)))))
    (if seq2
      (try-all-debug
       seq2 seq3 nil seq2-label seq3-label nil)
      (if seq3
        (try-all-debug
         seq3 nil nil seq3-label nil nil)))))

(defn try-all [seq1 seq2 seq3 seq1-label seq2-label seq3-label]
  (lazy-cat seq1 seq2 seq3))

;; TODO: move this to inside lightning-bolt.
(defn decode-generation-ordering [rand1 rand2]
  (cond (= rand1 0)
        (str "hLcL + " (decode-gen-ordering2 rand2) " + hPcL")
        (= rand 1)
        (str (decode-gen-ordering2 rand2) " + hLcL + hPcL")
        (= rand 2)
        (str (decode-gen-ordering2 rand2) " + hPcL + hLcL")
        true
        (str "hPcL + "  (decode-gen-ordering2 rand2) " + hLcL")))

(defn lightning-bolt [ & [head lexicon phrases depth]]
  (log/debug (str "--lb---depth: " depth))
  (log/debug (str "--lb---head cat: " (get-in head '(:synsem :cat))))
  (let [maxdepth 2
        depth (if depth depth 0)

        headed-parents-at-this-depth
        (filter (fn [each]
                  (not (fail? each)))
                (map (fn [phrase]
                       ;; TODO: possibly: remove-paths such as (subcat) from head: would make it easier to call with lexemes:
                       ;; e.g. "generate a sentence whose head is the word 'mangiare'" (i.e. user passes the lexical entry as
                       ;; head param of (lightning-bolt)".
                       (unifyc phrase head))
                     (cond (= depth 0) ;; if depth is 0 (top-level), only allow phrases with empty subcat.
                           (filter (fn [phrase]
                                     (empty? (get-in phrase '(:synsem :subcat))))
                                   phrases)
                           (= depth 1)
                           (filter (fn [phrase]
                                     (and (not (empty? (get-in phrase '(:synsem :subcat))))
                                          (empty? (get-in phrase '(:synsem :subcat :2)))))
                                   phrases)
                           true
                           '())))
        head (if head head :top)
        ]

    (cond

     ;; optimization: if a head's :cat is in a set of certain categories (e.g. :det),
     ;; don't try to create phrases with it: just return nil.
     (= (get-in head '(:synsem :cat)) :det)
     nil

     (= 0 (.size headed-parents-at-this-depth))
     nil

     true
     (let [debug (log/debug (str "lb start: depth:" depth "; head: " head))
           parents-with-lexical-heads
           (let [head (dissoc-paths head '((:synsem :subcat)
                                           (:english :initial)
                                           (:italian :initial)))]
             (overh headed-parents-at-this-depth (map-lexicon head (lazy-shuffle lexicon))))

           one-level-trees (overc parents-with-lexical-heads (lazy-shuffle lexicon))

           parents-with-phrasal-head (if (< depth maxdepth)
                                       (get-parents-with-phrasal-head
                                        headed-parents-at-this-depth
                                        lexicon
                                        phrases
                                        depth))

           rand-order (if true (rand-int 4) 1)
           rand-parent-type-order (if true (rand-int 2) 1)

           the-comp-phrases (comp-phrases
                             (cond (= rand-parent-type-order 0)
                                   (lazy-cat parents-with-lexical-heads parents-with-phrasal-head)
                                   true
                                   (lazy-cat parents-with-phrasal-head parents-with-lexical-heads))
                             phrases (lazy-shuffle lexicon))

          ]

       (log/debug (str "lightning-bolt rand-order at depth:" depth " is: " (decode-generation-ordering rand-order rand-parent-type-order) "(" rand-order "/" rand-parent-type-order ")"))

       (cond (< depth maxdepth)
             (cond (= rand-order 0)
                   (try-all
                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    the-comp-phrases

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc parents-with-phrasal-head
                           (lazy-shuffle lexicon)) ;; complement (the lexicon).

                    "hLcL"
                    (cond (= rand-parent-type-order 0)
                          (str "hLcP " "hPcP")
                          true
                          (str "hPcP " "hLcP"))
                    "hPcL")

                   (= rand-order 1)
                   (try-all
                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    the-comp-phrases

                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc parents-with-phrasal-head
                           (lazy-shuffle lexicon)) ;; complement (the lexicon).

                    (cond (= rand-parent-type-order 0)
                          (str "hLcP " "hPcP")
                          true
                          (str "hPcP " "hLcP"))
                    "hLcL"
                    "hPcL")

                   (= rand-order 2)
                   (try-all

                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    the-comp-phrases

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc parents-with-phrasal-head
                           (lazy-shuffle lexicon)) ;; complement (the lexicon).
                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    (cond (= rand-parent-type-order 0)
                          (str "hLcP " "hPcP")
                          true
                          (str "hPcP " "hLcP"))
                    "hPcL"
                    "hLcL")

                   (= rand-order 3)
                   (try-all

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc parents-with-phrasal-head
                           (lazy-shuffle lexicon)) ;; complement (the lexicon).


                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    the-comp-phrases

                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    (cond (= rand-parent-type-order 0)
                          (str "hLcP" "hPcP")
                          true
                          (str "hPcP" "hLcP"))
                    "hPcL"
                    "hLcL")

                   true
                   (try-all

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc parents-with-phrasal-head
                           (lazy-shuffle lexicon)) ;; complement (the lexicon).

                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    the-comp-phrases

                    (cond (= rand-parent-type-order 0)
                          (str "hLcP" "hPcP")
                          true
                          (str "hPcP" "hLcP"))
                    "hLcL"
                    "hPcL"))

             true
             one-level-trees)))))

;; aliases that are easier to use in a repl:
(defn lb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

(defn lightningb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

