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

(defn over [parents child1 & [child2]]
  (over/over parents child1 child2))

(defn overh [parent head]
  (if (or true (not (seq? head)))
    (over/overh parent head)
    (do
      (log/debug (str "overh head: " (show-spec (get-in parent '(:head :synsem)))))
      (log/debug (str "overh head fo: " (fo-ps parent)))
      (log/debug (str "overh size of head candidates: " (.size head)))
      (let [result (over/overh parent head)]
        (log/debug (str "survivor type is: " result))
        (if (seq? result) 
          (log/debug (str "overh size of survivors: " (.size result))))
        (if (seq? result)
          (if (> (.size result) 0)
            (log/debug (str "survivors are nonempty."))
            (log/debug (str "survivors are empty."))))
        result))))

(defn overc [parent comp]
  (if (or true (not (seq? comp)))
    (do (log/trace (str "comp is not a seq; returning over/overc directly."))
        (over/overc parent comp))
    (do
      (log/debug (str "overc comp: " (show-spec (get-in parent '(:comp :synsem)))))
      (if (not (nil? comp))
        (log/debug (str "overc size of comp: " (.size comp))))
      (let [result (over/overc parent comp)]
        (if (not (nil? result))
          (log/trace (str "overc size of result: " (.size result))))
        result))))

(defn get-lex [schema head-or-comp cache lexicon]
  (if (not (map? schema))
    (throw (Exception. (str "'schema' not a map: " schema))))
  (log/debug (str "get-lex for schema: " (:comment schema)))
  (if (nil? (:comment schema))
    (log/error (str "no schema for: " schema)))
  (let [result (cond (= :head head-or-comp)
                     (if (and (= :head head-or-comp)
                              (not (nil? (:head (get cache (:comment schema))))))
                       (do
                         (log/trace (str "get-lex hit: head for schema: " (:comment schema)))
                         (:head (get cache (:comment schema))))
                       (do
                         (log/warn (str "CACHE MISS 1"))
                         lexicon))

                     (= :comp head-or-comp)
                     (if (and (= :comp head-or-comp)
                              (not (nil? (:comp (get cache (:comment schema))))))
                       (do
                         (log/trace (str "get-lex hit: comp for schema: " (:comment schema)))
                         (:comp (get cache (:comment schema))))
                       (do
                         (log/warn (str "CACHE MISS 2"))
                         lexicon))

                     true
                     (do (log/warn (str "CACHE MISS 3"))
                         lexicon))]
    (lazy-shuffle result)))

(defn get-head-phrases-of [parent cache]
  (let [result (:head-phrases (get cache (:comment parent)))
        result (if (nil? result) (list) result)]
    (if (empty? result)
      (log/warn (str "headed-phrases of parent: " (:comment parent) " is empty.")))
    (lazy-shuffle result)))

(defn get-comp-phrases-of [parent cache]
  (let [result (:comp-phrases (get cache (:comment parent)))
        result (if (nil? result) (list) result)]
    (if (empty? result)
      (log/warn (str "comp-phrases of parent: " (:comment parent) " is empty.")))
    (lazy-shuffle result)))

(defn overc-with-cache-1 [parent lex]
  (log/trace (str "overc-with-cache-1 with parent: " (fo-ps parent)))
  (if (not (empty? lex))
    (lazy-cat (overc parent (first lex))
              (overc-with-cache-1 parent (rest lex)))))

(defn overc-with-cache [parents cache lexicon]
  (log/trace (str "overc-with-cache with parents type: " (type parents)))
  (if (not (empty? parents))
    (let [parent (first parents)]
      (lazy-cat (overc-with-cache-1 parent (get-lex parent :comp cache lexicon))
                (overc-with-cache (rest parents) cache lexicon)))))

(defn overh-with-cache-1 [parent lex]
  (if (not (empty? lex))
    (lazy-seq (cons (overh parent (first lex))
                    (overh-with-cache-1 parent (rest lex))))))

(defn overh-with-cache [parents cache lexicon]
  (if (not (empty? parents))
    (lazy-seq
     (let [parent (first parents)]
       (lazy-cat (overh-with-cache-1 parent (get-lex parent :head cache lexicon))
                 (overh-with-cache (rest parents) cache lexicon))))))
