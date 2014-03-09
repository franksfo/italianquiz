(ns italianverbs.cache
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [clojure.core :exclude [get-in]]

   ;; TODO: comment is misleading in that we never call core/get-in from this file.
   [clojure.core :as core] ;; This allows us to use core's get-in by doing "(core/get-in ..)"

   [clojure.set :refer :all]
   [clojure.string :as string]

   [clojure.tools.logging :as log]

   [italianverbs.lexicon :refer :all]
   [italianverbs.lexiconfn :refer :all]
   [italianverbs.morphology :refer [finalize fo fo-ps]]
   [italianverbs.over :exclude [overc overh]]
   [italianverbs.over :as over]
   [italianverbs.unify :refer :all :exclude [unify]]))

;; For now, this cache is just a stub; no actual caching is done; it simply calls 
;; the over/ equivalents of each of the defined functions.

(def head-cache {})
(def comp-cache {})

(defn build-lex-sch-cache [phrases lexicon all-phrases]
  "Build a mapping of phrases onto subsets of the lexicon. The two values (subsets of the lexicon) to be
   generated for each key (phrase) are: 
   1. the subset of the lexicon that can be the head of this phrase.
   2. the subset of the lexicon that can be the complement of this phrase.

   End result is a set of phrase => {:comp subset-of-lexicon 
                                     :head subset-of-lexicon}."
  (if (not (empty? phrases))
    (conj
     {(:rule (first phrases))
      {:comp
       (filter (fn [lex]
                 (not (fail? (unifyc (first phrases)
                                     {:comp lex}))))
               lexicon)

       :comp-phrases
       (filter (fn [comp-phrase]
                 (not (fail? (unifyc (first phrases)
                                     {:comp comp-phrase}))))
               all-phrases)

       :head-phrases
       (filter (fn [head-phrase]
                 (not (fail? (unifyc (first phrases)
                                     {:head head-phrase}))))
               all-phrases)

       :head
       (filter (fn [lex]
                 (not (fail? (unifyc (first phrases)
                                     {:head lex}))))
               lexicon)}}
     (build-lex-sch-cache (rest phrases) lexicon all-phrases))
    {}))

(defn over [parents child1 & [child2]]
  (over/over parents child1 child2))

(defn overh [parent head]
  (if (not (seq? head))
    (over/overh parent head)
    (do
      (log/trace (str "overh head: " (show-spec (get-in parent '(:head :synsem)))))
      (log/trace (str "overh head fo: " (fo-ps parent)))
      (log/trace (str "overh size of head candidates: " (.size head)))
      (let [result (over/overh parent head)]
        (log/trace (str "survivor type is: " result))
        (if (seq? result) 
          (do (log/trace (str "overh size of survivors: " (.size result))))
          (if (not (empty? result))
            (log/trace (str "survivors are nonempty."))
            (log/trace (str "survivors are empty."))))
        result))))

(defn overc [parent comp]
  (if (or true (not (seq? comp)))
    (do (log/trace (str "comp is not a seq; returning over/overc directly."))
        (over/overc parent comp))
    (do
      (log/trace (str "overc comp: " (show-spec (get-in parent '(:comp :synsem)))))
      (if (not (nil? comp))
        (log/trace (str "overc size of comp: " (.size comp))))
      (let [result (over/overc parent comp)]
        (if (not (nil? result))
          (log/trace (str "overc size of result: " (.size result))))
        result))))

(defn get-lex [schema head-or-comp cache lexicon]
  (if (nil? schema)
    #{}
    (do
      (log/debug (str "get-lex: " (:rule schema) " ; " head-or-comp))
      (if (not (map? schema))
        (throw (Exception. (str "first arguments should have been a map, but instead was of type: " (type schema) "; fo: " (fo schema)))))
      (log/trace (str "get-lex schema: " (:rule schema) " for: " head-or-comp))
      (if (nil? (:rule schema))
        (log/error (str "no schema for: " schema)))
      (let [result (cond (= :head head-or-comp)
                         (if (and (= :head head-or-comp)
                                  (not (nil? (:head (get cache (:rule schema))))))
                           (do
                             (log/debug (str "get-lex hit: head for schema: " (:rule schema)))
                             (:head (get cache (:rule schema))))
                           (do
                             (log/warn (str "CACHE MISS 1"))
                             lexicon))

                         (= :comp head-or-comp)
                         (if (and (= :comp head-or-comp)
                                  (not (nil? (:comp (get cache (:rule schema))))))
                           (do
                             (log/debug (str "get-lex hit: comp for schema: " (:rule schema)))
                             (:comp (get cache (:rule schema))))
                           (do
                             (log/warn (str "CACHE MISS 2"))
                             lexicon))
                       
                         true
                         (do (log/warn (str "CACHE MISS 3"))
                             lexicon))]
        (lazy-shuffle result)))))
  
(defn get-head-phrases-of [parent cache]
  (let [result (:head-phrases (get cache (:rule parent)))
        result (if (nil? result) (list) result)
        label (label-of parent)]
    (if (empty? result)
      (log/trace (str "headed-phrases of parent: " label " is empty.")))
    (lazy-shuffle result)))

(defn get-comp-phrases-of [parent cache]
  (let [result (:comp-phrases (get cache (:rule parent)))
        result (if (nil? result) (list) result)]
    (if (empty? result)
      (log/trace (str "comp-phrases of parent: " (label-of parent) " is empty.")))
    (lazy-shuffle result)))

(defn overc-with-cache-1 [parent lex]
  (if (not (empty? lex))
    (do
      (log/trace (str "overc-with-cache-1 with parent: " (fo-ps parent) 
                      " and lex: " (fo (first lex))))
      (lazy-cat (overc parent (first lex))
                (overc-with-cache-1 parent (rest lex))))))

(defn overc-with-cache [parents cache lexicon]
  (if (not (empty? parents))
    (let [parent (first parents)
          use-spec {:synsem (get-in parent '(:comp :synsem))}
          debug (log/debug (str "overc-with-cache: parent: " (fo-ps parent) 
                                " ; filter by spec: " (show-spec use-spec)))]
      (lazy-cat (overc-with-cache-1 parent (filter (fn [lexeme]
                                                     (not (fail? (unifyc lexeme
                                                                         use-spec))))
                                                   (get-lex parent :comp cache lexicon)))
                (overc-with-cache (rest parents) cache lexicon)))))

(defn overh-with-cache-1 [parent lex]
  (if (not (empty? lex))
    (lazy-seq (cons (overh parent (first lex))
                    (overh-with-cache-1 parent (rest lex))))))

(defn overh-with-cache [parents cache lexicon]
  (if (not (empty? parents))
    (let [parent (first parents)]
      (lazy-cat (overh-with-cache-1 parent (get-lex parent :head cache lexicon))
                (overh-with-cache (rest parents) cache lexicon)))))

