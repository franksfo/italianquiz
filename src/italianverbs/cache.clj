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

(defn specs-to-subsets [lexicon-of-heads lexicon]
  (if (not (empty? lexicon-of-heads))
    (let [lexeme (first lexicon-of-heads)]
      (if (keyword? (show-spec (get-in lexeme '(:synsem :subcat :1 :cat))))
        (conj {(show-spec (get-in lexeme '(:synsem :subcat :1 :cat)))
               (filter (fn [each-lex]
                         (not (fail? (unifyc (get-in each-lex '(:synsem :cat))
                                             (get-in lexeme '(:synsem :subcat :1 :cat))))))
                       lexicon)}
              (specs-to-subsets (rest lexicon-of-heads)
                                lexicon))
        (specs-to-subsets (rest lexicon-of-heads)
                          lexicon)))
    {}))

(declare spec-to-phrases)

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
    {:lexical-subsets (specs-to-subsets lexicon lexicon)}))

(defn spec-to-phrases [specs all-phrases]
  (if (not (empty? specs))
    (let [spec (first specs)]
      (conj
       {spec 
        (filter #(not (fail? %))
                (map (fn [each-phrase]
                       (unifyc each-phrase spec))
                     ;; TODO: possibly: remove-paths such as (subcat) from head: would make it easier to call with lexemes:
                     ;; e.g. "generate a sentence whose head is the word 'mangiare'" (i.e. user passes the lexical entry as
                     ;; head param of (lightning-bolt)".
                     all-phrases))}
       (spec-to-phrases (rest specs) all-phrases)))
    {}))

(defn over [parents child1 & [child2]]
  (over/over parents child1 child2))

(defn overh [parent head]
  (if (seq? parent)
    (mapcat (fn [each-parent]
              (log/debug (str "overh: each-parent: " (fo-ps each-parent)))
              (overh each-parent head))
            parent)
    (do
      (log/trace (str "overh head: " (show-spec (get-in parent '(:head :synsem)))))
      (log/trace (str "overh head fo: " (fo-ps parent)))
      (log/trace (str "overh size of head candidates: " (.size head)))
      (if (not (nil? head))
        (log/debug (str "overh first head: " (fo (first head)) " for parent: " (fo-ps parent))))

      (let [result (over/overh parent head)]
        (log/trace (str "survivor type is: " result))
        (if (seq? result) 
          (do (log/trace (str "overh size of survivors: " (.size result))))
          (if (not (empty? result))
            (log/trace (str "survivors are nonempty."))
            (log/trace (str "survivors are empty."))))
        result))))

(defn overc [parent comp]
  (if (not (seq? comp))
    (do (log/trace (str "comp is not a seq; returning over/overc directly."))
        (over/overc parent comp))
    (do
      (log/trace (str "overc comp: " (show-spec (get-in parent '(:comp :synsem)))))
      (if (not (nil? comp))
        (log/trace (str "overc size of comp: " (.size comp))))
      (if (not (nil? comp))
        (log/debug (str "overc first comp: " (fo (first comp)) " for parent: " (fo-ps parent))))
      (let [result (over/overc parent comp)]
        (if (not (nil? result))
          (log/trace (str "overc size of result: " (.size result))))
        result))))

(defn get-lex [schema head-or-comp cache]
  (if (nil? schema)
    #{}
    (do
      (log/debug (str "get-lex: " (:rule schema) " ; " head-or-comp))
      (if (not (map? schema))
        (throw (Exception. (str "first arguments should have been a map, but instead was of type: " (type schema) "; fo: " (fo schema)))))
      (log/trace (str "get-lex schema: " (:rule schema) " for: " head-or-comp))
      (if (nil? (:rule schema))
        (throw (Exception. (str "no schema for: " schema))))
      (let [result (cond (= :head head-or-comp)
                         (if (and (= :head head-or-comp)
                                  (not (nil? (:head (get cache (:rule schema))))))
                           (do
                             (log/trace (str "get-lex hit: head for schema: " (:rule schema)))
                             (:head (get cache (:rule schema))))
                           (do
                             (log/warn (str "CACHE MISS 1"))
                             lexicon))

                         (= :comp head-or-comp)
                         (if (and (= :comp head-or-comp)
                                  (not (nil? (:comp (get cache (:rule schema))))))
                           (do
                             (log/trace (str "get-lex hit: comp for schema: " (:rule schema)))
                             (:comp (get cache (:rule schema))))
                           (do
                             (log/warn (str "CACHE MISS 2"))
                             nil))
                       
                         true
                         (do (log/warn (str "CACHE MISS 3"))
                             nil))]
        (lazy-shuffle result)))))
  
(defn get-parent-phrases-for-spec [cache spec]
  (log/error (str "Looking up spec: " (show-spec spec)))
  (let [result (get (get cache :phrases-for-spec) (show-spec spec))
        result (if (nil? result) (list) result)]
    (if (empty? result)
      (log/trace (str "parent-phrases for spec: " (show-spec spec) " is empty.")))
    (lazy-shuffle result)))

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

(defn get-subset-from-cache [cache use-spec]
  (let [debug (log/debug (str "looking for use-spec 1: " use-spec))
        use-spec (get-in use-spec '(:synsem :cat))
        debug (log/debug (str "looking for use-spec 2: " use-spec))
        ls-part (get cache :lexical-subsets :notfound)]
    (if (= :notfound ls-part)
      :notfound
      (get ls-part (show-spec use-spec) :notfound))))

(defn overc-with-cache [parents cache]
  (if (not (empty? parents))
    (let [parent (first parents)
          use-spec {:synsem (get-in parent [:comp :synsem])}
          lexicon (let [cached (get-subset-from-cache cache (show-spec use-spec))]
                    (lazy-shuffle (if (= cached :notfound)
                                    (get-lex parent :comp cache nil)
                                    cached)))]
      (lazy-cat (overc-with-cache-1 parent (filter (fn [lexeme]
                                                     (not (fail? (unifyc lexeme
                                                                         use-spec))))
                                                   lexicon))
                (overc-with-cache (rest parents) cache lexicon)))))

(defn overh-with-cache [parent cache lexicon]
  (let [lexicon (lazy-shuffle (get-lex parent :head cache lexicon))
        use-spec {:synsem (get-in parent [:head :synsem])}]
    (overh parent
            (filter (fn [lexeme]
                      (not (fail? (unifyc lexeme
                                          use-spec))))
                    lexicon))))




