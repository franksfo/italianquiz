(ns italianverbs.forest
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [clojure.core :as core]
   [clojure.set :refer :all]
   [clojure.stacktrace :refer :all]
   [clojure.string :as string]
   [clojure.tools.logging :as log]

   [italianverbs.config :as config]
   [italianverbs.html :as html]

   [italianverbs.grammar :refer :all]
   [italianverbs.lexiconfn :refer (sem-impl)]
   [italianverbs.morphology :refer :all]
   [italianverbs.over :refer :all]

   [italianverbs.lev :as lev]
   [italianverbs.search :as search]
   [italianverbs.unify :refer :all]
   [italianverbs.unify :as unify]))

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
             (:italian :initial)))]

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

(defn get-bolts [heads lexicon phrases depth one-level-trees parents-with-lexical-heads]
  (log/debug (str "get-bolts: heads size: " (.size heads))) ;;; REALIZES
  (log/debug (str "calling get-bolts with depth: " depth " and first head: " (first heads)))
  (if (not (empty? heads))
    (lazy-seq
     (cons
      (lightning-bolt (first heads) lexicon phrases depth one-level-trees parents-with-lexical-heads)
      (get-bolts (rest heads) lexicon phrases depth one-level-trees parents-with-lexical-heads)))))

;; TODO: move this to inside lightning-bolt.
(defn decode-gen-ordering2 [rand2]
  (cond (= rand2 0)
        "hLcP + hPcP"
        true
        "hPcP + hLcP"))

;; TODO: make option to just call (lazy-cat seq1 seq2 seq3) for efficiency:
;; this is simply a diagnostic tool.
(defn try-all [seq1 seq2 seq3 seq1-label seq2-label seq3-label]
  (if (not (empty? seq1))
    (let [first-of-seq1 (first seq1)]
      (do
        (log/debug (str "seq1 ("seq1-label") has a candidate:" (fo-ps first-of-seq1)))
        (lazy-seq
         (cons
          first-of-seq1
          (try-all (rest seq1) seq2 seq3 seq1-label seq2-label seq3-label)))))
    (if seq2
      (try-all
       seq2 seq3 nil seq2-label seq3-label nil)
      (if seq3
        (try-all
         seq3 nil nil seq3-label nil nil)))))

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

(defn lightning-bolt [ & [head lexicon phrases depth one-level-trees parents-with-lexical-heads]]
  (log/debug (str "--lb---depth: " depth))
  (log/debug (str "--lb---head: " head))
  (let [maxdepth 2
        depth (if depth depth 0)

        headed-parents-at-this-depth
        (filter (fn [phrase]
                  ;; TODO: possibly: remove-paths such as subcat from head: would make it easier to call with lexemes:
                  ;; e.g. "generate a sentence whose head is 'mangiare'".
                  (not (fail? (unifyc phrase head))))
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
                      '()))
        head (if head head :top)
        ]

    (log/debug (str "--lb---headed-parents-at-this-depth: (size): " (.size headed-parents-at-this-depth))) ;; REALIZES
    (log/debug (str "--lb---headed-parents-at-this-depth: (first): " (fo-ps (first headed-parents-at-this-depth))))


    (cond

     ;; optimization: if a head's :cat is in a set of certain categories (e.g. :det),
     ;; don't try to create phrases with it: just return nil.
     (= (get-in head '(:synsem :cat)) :det)
     nil

     (= 0 (.size headed-parents-at-this-depth))
     nil

     true
     (let [debug (log/debug (str "lb start: depth:" depth "; head: " head))
           debug (log/debug (str "parents: " (.size headed-parents-at-this-depth) "; " (fo-ps headed-parents-at-this-depth)))
           parents-with-lexical-heads
           (if (and false parents-with-lexical-heads)
             (do
               parents-with-lexical-heads)
             (do
               (let [head (dissoc-paths head '((:synsem :subcat)
                                                   (:english :initial)
                                                   (:italian :initial)))
                     result (overh headed-parents-at-this-depth (map-lexicon head lexicon))]
                 result)))

           one-level-trees (if one-level-trees one-level-trees
                               (overc parents-with-lexical-heads lexicon))

           parents-with-phrasal-head (if (< depth maxdepth)
                               (let [debug (log/debug (str "doing bolts : " (fo-ps (first headed-parents-at-this-depth)))) ;; REALIZES:
                                     debug (log/debug (str "doing bolts with (first) head: " (get-in (first headed-parents-at-this-depth)
                                                                                          '(:head))))
                                     bolts (get-bolts (map (fn [each-phrase]
                                                             (get-in each-phrase '(:head)))
                                                           headed-parents-at-this-depth)
                                                      lexicon phrases (+ 1 depth) one-level-trees parents-with-lexical-heads)]
                                 (let [debug (log/trace (str "creating parents-with-phrasal-head at depth:" depth))]
                                   (overh headed-parents-at-this-depth bolts))))

           rand-order (rand-int 4)
;           rand-order 0

           rand-parent-type-order (rand-int 2)

           the-comp-phrases (comp-phrases
                             (cond (= rand-parent-type-order 0)
                                   (lazy-cat parents-with-lexical-heads parents-with-phrasal-head)
                                   true
                                   (lazy-cat parents-with-phrasal-head parents-with-lexical-heads))
                             phrases lexicon)

          ]

       (log/debug (str "lightning-bolt rand-order at depth:" depth " is: " (decode-generation-ordering rand-order rand-parent-type-order) "(" rand-order "/" rand-parent-type-order ")"))
       (log/debug (str "lightning-bolt parents-with-lexical-heads at depth:" depth " is: " (.size parents-with-lexical-heads))) ;; REALIZES

       (cond (< depth maxdepth)
             (cond (= rand-order 0)
                   (try-all
                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    the-comp-phrases

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc parents-with-phrasal-head
                           lexicon) ;; complement (the lexicon).

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
                           lexicon) ;; complement (the lexicon).

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
                           lexicon) ;; complement (the lexicon).

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
                           lexicon) ;; complement (the lexicon).


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
                           lexicon) ;; complement (the lexicon).

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

