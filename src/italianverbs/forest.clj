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
  (log/info (str "mapping lexicon using head with synsem: " (get-in head '(:synsem))))
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

(defn lightning-bolt [ & [head lexicon phrases depth] ]
  (let [maxdepth 2
        depth (if depth depth 0)
        head (if head head :top)]
    (let [debug (log/debug (str "lightning-bolt head (fo): " (fo head)))
          debug (log/debug (str "lightning-bolt head: " head))
          debug (log/info (str "lightning-bolt depth: " depth "; head: " (fo head) "; head sem: " (get-in head '(:synsem :sem))))]

      (let [debug (log/debug (str "-- /depth: " depth))
            debug (log/debug (str "lightning-bolt: end"))
            debug (log/debug (str "head's sem-impl: " (sem-impl (get-in head '(:synsem :sem)))))
            with-lexical-heads (overh phrases (map-lexicon head lexicon))
;;            debug (log/info (str "overh phrases with lexical heads (size=" (.size with-lexical-heads)  ") : " (fo-ps with-lexical-heads))) ;; realizes
            ]
        (lazy-cat

         ;; 1. both head and comp are lexemes.
         (overc with-lexical-heads lexicon)

         ;; 2. head is a lexeme, comp is a phrase.
         (if (< depth maxdepth)
           (comp-phrases with-lexical-heads phrases lexicon))

         (if (< depth maxdepth)
           ;; 3. head is a phrase, comp is a lexeme:
           (overhc phrases
                   (lightning-bolt head lexicon phrases (+ 1 depth)) ;; head
                   lexicon)) ;; complement (the lexicon).

         ;; 4. head is a phrase, comp is a phrase.
         (if (< depth maxdepth)
           (comp-phrases (overh phrases (lightning-bolt head lexicon phrases (+ 1 depth))) phrases lexicon)))))))

;; aliases that are easier to use in a repl:
(defn lb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

(defn lightningb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

