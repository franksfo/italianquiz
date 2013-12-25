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
   [italianverbs.lexicon :refer :all]
   [italianverbs.lexiconfn :refer (sem-impl)]
   [italianverbs.morphology :refer :all]
   [italianverbs.over :refer :all]

   [italianverbs.lev :as lev]
   [italianverbs.search :as search]
   [italianverbs.ug :refer :all]
   [italianverbs.unify :refer :all]
   [italianverbs.unify :as unify]))

(def parents (set (list (merge (unifyc cc10
                                       {:synsem {:infl :present
                                                 :cat :verb
                                                 :sem {:tense :present}}})
                               {:comment "parent1/cc10"})

                        (merge (unifyc hh21
                                       {:synsem {:infl :present
                                                 :cat :verb
                                                 :sem {:tense :present}}})
                               {:comment "parent2/hh21"})

                        (merge (unifyc cc10
                                       {:synsem {:cat :noun}})
                               {:comment "parent3/cc10"}))))

(def lex (seq (union (it "il") (it "cane") (it "i")
                     (it "io") (it "pane") (it "tu")
                     (it "lui") (it "lei")
                     (it "dormire") (it "sognare") (it "mangiare"))))

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
    (let [phrase-with-head (first phrases-with-heads)]
      (log/debug (str "comp-phrases: looking for phrases with phrase-with-head's comp: " (get-in phrase-with-head '(:comp))))
      (log/info (str "comp-phrases: looking for complements with phrase-with-head: " (fo-ps phrase-with-head)))
      (lazy-cat
       (overc phrase-with-head
              (lightning-bolt
               (remove-path-from
                (get-in phrase-with-head '(:comp))
                '((:synsem :subcat)
                  (:english :initial)
                  (:italian :initial)
                  ))
               lexicon
               all-phrases
               0))
       (comp-phrases (rest phrases-with-heads) all-phrases lexicon)))))

(defn lightning-bolt [ & [head lexicon phrases depth] ]
  (let [depth (if depth depth 0)
        head (if head head :top)
        lexicon (if lexicon lexicon lex)
        phrases (if phrases phrases parents)]
    (let [debug (log/debug (str "lightning-bolt head (fo): " (fo head)))
          debug (log/debug (str "lightning-bolt head: " head))
          debug (log/info (str "lightning-bolt depth: " depth))
          debug (log/debug (str "lightning-bolt lexicon: " (fo lexicon)))
          recursive-head
          (cond (= depth 0)
                (lightning-bolt head lexicon phrases (+ 1 depth))

                (< depth 2)
                (lightning-bolt head lexicon phrases (+ 1 depth))

                true  ;; bounded depth: if depth is greater than any matched above, don't branch any more.
                nil)]

      (let [debug (log/debug (str "lightning-bolt: recursive head type: " (type recursive-head)))
            debug (log/debug (str "-- /depth: " depth))
            debug (log/debug (str "lightning-bolt: end"))
            debug (log/debug (str "head's sem-impl: " (sem-impl (get-in head '(:synsem :sem)))))
            with-lexical-heads (overh phrases (map-lexicon head lexicon))
            debug (log/debug (str "first phrase with lexical heads: " (first with-lexical-heads)))]
        (lazy-cat

         ;; 1. both head and comp are lexemes.
         (overc with-lexical-heads lexicon)

         ;; 2. head is a lexeme, comp is a phrase.
         (comp-phrases with-lexical-heads phrases lexicon)

         ;; 3. head is a phrase, comp is a lexeme.
         (overhc phrases recursive-head lexicon)

         ;; 4. head is a phrase, comp is a phrase.
         (comp-phrases (overh phrases recursive-head) phrases lexicon))))))

(defn lb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)
        lexicon (if lexicon lexicon lex)
        phrases (if phrases phrases parents)]
    (lightning-bolt head lexicon phrases depth)))

(defn lightningb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)
        lexicon (if lexicon lexicon lex)
        phrases (if phrases phrases parents)]
    (lightning-bolt head lexicon phrases depth)))

