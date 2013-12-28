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

      (log/debug (str "comp-phrases: looking for phrases with phrase-with-head's comp: "
                      remove-some-paths))
      (lazy-cat
       (overc phrase-with-head
              (lightning-bolt
               remove-some-paths
               lexicon
               all-phrases
               0))
       (comp-phrases (rest phrases-with-heads) all-phrases lexicon)))))

(defn lightning-bolt [ & [head lexicon phrases depth one-level-trees with-lexical-heads]]
  (let [maxdepth 2
        depth (if depth depth 0)
        headed-phrases-at-this-depth
        (cond (= depth 0) ;; if depth is 0 (top-level), only allow phrases with empty subcat.
              (filter (fn [phrase]
                        (empty? (get-in phrase '(:synsem :subcat))))
                      phrases)
              (= depth 1)
              (filter (fn [phrase]
                        (and (not (empty? (get-in phrase '(:synsem :subcat))))
                             (empty? (get-in phrase '(:synsem :subcat :2)))))
                      phrases)
              (= depth 2)
              (filter (fn [phrase]
                        (and (not (empty? (get-in phrase '(:synsem :subcat))))
                             (not (empty? (get-in phrase '(:synsem :subcat :2))))
                             (empty? (get-in phrase '(:synsem :subcat :3)))))
                      phrases)
              true
              phrases)
        head (if head head :top)]
    (cond

     ;; optimization: if a head's :cat is in a set of certain categories (e.g. :det),
     ;; don't try to create phrases with it: just return nil.
     (= (get-in head '(:synsem :cat)) :det)
     nil

     true
     (let [debug (log/debug (str "lightning-bolt head (fo): " (fo head)))
           debug (log/info (str "lightning-bolt head: " head))
           debug (log/info (str "lightning-bolt depth: " depth "; head sem: " (get-in head '(:synsem :sem))))
           debug (log/info (str "number of phrases: " (.size phrases)))
           debug (log/info (str "headed-phrases-at-this-depth: " (.size headed-phrases-at-this-depth)))
           with-lexical-heads (if with-lexical-heads with-lexical-heads
                                  (overh headed-phrases-at-this-depth (map-lexicon head lexicon)))

           one-level-trees (if one-level-trees one-level-trees
                               (overc with-lexical-heads lexicon))

           phrases-with-head (if (< depth maxdepth)
                             (let [recursive-head-lightning-bolt
                                   (lightning-bolt head lexicon phrases (+ 1 depth)
                                                   one-level-trees with-lexical-heads)]
                               (overh phrases recursive-head-lightning-bolt)))
           rand-order (rand-int 4)
;           rand-order 0
           ]

       ;; TODO: add scrambling of the call: (lazy-cat with-lexical-heads phrases-with-head) phrases lexicon) below.
       (cond (< depth maxdepth)
             (cond (= rand-order 0)
                   (lazy-cat
                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    (comp-phrases (lazy-cat with-lexical-heads phrases-with-head) phrases lexicon)

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc phrases-with-head
                           lexicon)) ;; complement (the lexicon).

                   (= rand-order 1)
                   (lazy-cat


                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    (comp-phrases (lazy-cat with-lexical-heads phrases-with-head) phrases lexicon)

                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc phrases-with-head
                           lexicon)) ;; complement (the lexicon).


                   (= rand-order 2)
                   (lazy-cat

                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    (comp-phrases (lazy-cat with-lexical-heads phrases-with-head) phrases lexicon)


                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc phrases-with-head
                           lexicon) ;; complement (the lexicon).

                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees)


                   (= rand-order 3)
                   (lazy-cat

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc phrases-with-head
                           lexicon) ;; complement (the lexicon).


                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    (comp-phrases (lazy-cat with-lexical-heads phrases-with-head) phrases lexicon)



                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees)

                   true
                   (lazy-cat



                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc phrases-with-head
                           lexicon) ;; complement (the lexicon).

                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    (comp-phrases (lazy-cat with-lexical-heads phrases-with-head) phrases lexicon)))


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

