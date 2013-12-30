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
          (remove-path-from
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
  (if (not (empty? heads))
    (log/debug (str "do-bolts with first heads: " (fo (first heads))))
    (lazy-seq
     (cons
      (lightning-bolt (first heads) lexicon phrases depth one-level-trees parents-with-lexical-heads)
      (get-bolts (rest heads) lexicon phrases depth one-level-trees parents-with-lexical-heads)))))

;; TODO: move this to inside lightning-bolt.
(defn decode-gen-ordering2 [rand2]
  (cond (= rand2 0)
        "hL/cP + hP/cP"
        true
        "hP/cP + hL/cP"))

;; TODO: move this to inside lightning-bolt.
(defn decode-generation-ordering [rand1 rand2]
  (cond (= rand1 0)
        (str "hL/cL + " (decode-gen-ordering2 rand2) " + hP/cL")
        (= rand 1)
        (str (decode-gen-ordering2 rand2) " + hL/cL + hP/cL")
        (= rand 2)
        (str (decode-gen-ordering2 rand2) " + hP/cL + hLcL")
        true
        (str "hP/cL + "  (decode-gen-ordering2 rand2) " + hLcL")))

(defn lightning-bolt [ & [head lexicon phrases depth one-level-trees parents-with-lexical-heads]]
  (let [maxdepth 2
        depth (if depth depth 0)
        headed-parents-at-this-depth
        (filter (fn [phrase]
                  (not (fail? (unifyc phrase {:head head}))))
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
        debug (log/debug (str "-lb-start- depth:" depth "; head: " head))
        ]
    (cond

     ;; optimization: if a head's :cat is in a set of certain categories (e.g. :det),
     ;; don't try to create phrases with it: just return nil.
     (= (get-in head '(:synsem :cat)) :det)
     nil

     true
     (let [parents-with-lexical-heads (if parents-with-lexical-heads
                                (do
                                  (log/debug (str "using cached parents-with-lexical-heads at depth:" depth))
                                  parents-with-lexical-heads)
                                  (do
                                    (log/debug (str "finding parents-with-lexical-heads with head: " head " at depth:" depth))
                                    (let [result (overh headed-parents-at-this-depth (map-lexicon head lexicon))]
                                      ;; REALIZES:
;                                      (log/debug (str "lb:lexical-headed-parents at depth :" depth ": " (.size result) " : "
;                                                     (fo-ps result)))
                                      result)))

           one-level-trees (if one-level-trees one-level-trees
                               (do
                                 (log/debug (str "lb:doing one-level-trees for head: " head))
                                 (overc parents-with-lexical-heads lexicon)))

           parents-with-phrasal-head (if (< depth maxdepth)
                               (let [debug (log/debug (str "recursing for parents-with-phrasal-head at depth:" depth " with head: " head))
                                     ;; REALIZES:
                                     ;; debug (log/debug (str "doing bolts with: " (fo-ps parents-with-lexical-heads)))
                                     bolts (get-bolts (map (fn [each-phrase]
                                                         (get-in each-phrase '(:head)))
                                                       parents-with-lexical-heads)
                                                      lexicon phrases depth one-level-trees parents-with-lexical-heads)]
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

       ;; TODO: add scrambling of the call: (lazy-cat parents-with-lexical-heads parents-with-phrasal-head) phrases lexicon) below.
       (log/debug (str "lightning-bolt rand-order: " (decode-generation-ordering rand-order rand-parent-type-order)))

       (cond (< depth maxdepth)
             (cond (= rand-order 0)
                   (lazy-cat
                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    the-comp-phrases

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc parents-with-phrasal-head
                           lexicon)) ;; complement (the lexicon).

                   (= rand-order 1)
                   (lazy-cat
                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    the-comp-phrases

                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc parents-with-phrasal-head
                           lexicon)) ;; complement (the lexicon).

                   (= rand-order 2)
                   (lazy-cat

                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    the-comp-phrases

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc parents-with-phrasal-head
                           lexicon) ;; complement (the lexicon).

                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees)


                   (= rand-order 3)
                   (lazy-cat

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc parents-with-phrasal-head
                           lexicon) ;; complement (the lexicon).


                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    the-comp-phrases

                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees)

                   true
                   (lazy-cat

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc parents-with-phrasal-head
                           lexicon) ;; complement (the lexicon).

                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    the-comp-phrases))


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

