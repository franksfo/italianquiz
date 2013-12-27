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

;; agents allow asynchronous, independent changes:  Rathore, p. 131
(def lexicon-index (agent {}))

(defn update-lexicon-index [key val]
  (log/debug (str "updating lexicon index: key: " key))
  (log/debug (str "updating lexicon index: val: " val))
  (send-off lexicon-index (fn [current-index new-key new-val] (conj current-index {new-key new-val})) key val)
  (log/debug (str "updated lexicon-index; new size is: " (.size (deref lexicon-index)))))

(defn map-lexicon [head lexicon]
  "TODO: determine if this is done lazily or not: it should. If not, will have to do laziness with (lazy-seq (cons..) over the lexicon."
  (let [stripped (strip-refs head)
        cached-value (get (deref lexicon-index) stripped :not-in-cache)]
    (if (not (= cached-value :not-in-cache))
      (do
        (log/debug "returning cached value.")
        cached-value)
      (let [debug (log/debug (str "no cached value found: mapping over lexicon and caching result."))
            retval
            (filter (fn [lexeme]
                      (not (fail? lexeme)))
                (map (fn [lexeme]
                       (unifyc
                        head
                        (unifyc
                         {:synsem {:sem (sem-impl (get-in head '(:synsem :sem)))}}
                         lexeme)))
                     lexicon))]
        (update-lexicon-index (strip-refs head) retval)
        retval))))

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

;; TODO: add param to memoize (overh phrases (map lexicon head lexicon)).
(defn lightning-bolt [ & [head lexicon phrases depth] ]
  (let [maxdepth 2
        depth (if depth depth 0)
        head (if head head :top)
        lexemes-for-head (map-lexicon head lexicon)]
    (cond

     ;; optimization: if a head's :cat is in a set of certain categories (e.g. :det),
     ;; don't try to create phrases with it: just return nil.
     (= (get-in head '(:synsem :cat)) :det)
     nil

     true
     (let [debug (log/debug (str "lightning-bolt head (fo): " (fo head)))
           debug (log/debug (str "lightning-bolt head: " head))
           debug (log/info (str "lightning-bolt depth: " depth "; head sem: " (get-in head '(:synsem :sem))))
           with-lexical-heads (overh phrases lexemes-for-head)
           recursive-head-lightning-bolt (if (< depth maxdepth) (lightning-bolt head lexicon phrases (+ 1 depth)))]
         (lazy-cat

          ;; 1. both head and comp are lexemes.
          (overc with-lexical-heads lexicon)

          ;; 2. head is a lexeme, comp is a phrase.
          (if (< depth maxdepth)
            (comp-phrases with-lexical-heads phrases lexicon))

          (if (< depth maxdepth)
            ;; 3. head is a phrase, comp is a lexeme:
            (overhc phrases
                    recursive-head-lightning-bolt ;; head
                    lexicon)) ;; complement (the lexicon).

          ;; 4. head is a phrase, comp is a phrase.
          (if (< depth maxdepth)
            (comp-phrases (overh phrases recursive-head-lightning-bolt) ;; head
                          phrases lexicon)))))))

;; aliases that are easier to use in a repl:
(defn lb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

(defn lightningb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

