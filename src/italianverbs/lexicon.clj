(ns italianverbs.lexicon
  (:refer-clojure :exclude [get-in merge resolve find])
  (:require
   [clojure.set :refer (union)]
   [clojure.tools.logging :as log]
   [italianverbs.lexiconfn :refer (cache-serialization sem-impl subcat0 subcat1)]
   [italianverbs.lex.a_noi :refer :all]
   [italianverbs.lex.notizie_potere :refer :all]
   [italianverbs.lex.qualche_volta_volere :refer :all]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.unify :as unify]
   [italianverbs.unify :refer (fail? get-in isomorphic? lazy-shuffle ref? serialize unifyc)]))

;; stub that is redefined by italianverbs/mongo or interfaces to other dbs.
(defn clear! [])

(def lookup-in
  "find all members of the collection that matches with query successfully."
  (fn [query collection]
    (loop [coll collection matches nil]
      (if (not (empty? coll))
        (let [first-val (first coll)
              result (unify/match (unify/copy query) (unify/copy first-val))]
          (if (not (unify/fail? result))
            (recur (rest coll)
                   (cons first-val matches))
            (recur (rest coll)
                   matches)))
        matches))))

(declare lexicon)

(defn lookup [query]
  (lookup-in query lexicon))

(defn choose-lexeme [spec]
  (first (lazy-shuffle (lookup spec))))

(defn it [italian]
  "same as it but no type conversion of singleton sets to take the first member."
  (let [result
        (union (set (lookup {:italian italian}))
               (set (lookup {:italian {:infinitive italian}}))
               (set (lookup {:italian {:infinitive {:infinitive italian}}}))
               (set (lookup {:italian {:italian italian}}))
               (set (lookup {:italian {:irregular {:passato italian}}})))]
    result))

(defn en [english]
  (lookup {:english english}))

(clear!)

(load "lex/a_noi")
(load "lex/notizie_potere")
(load "lex/qualche_volta_volere")

(defn pronoun-rule [lexical-entry]
  ;; subcat non-empty: pronoun is false
  (cond (and (= (get-in lexical-entry '(:synsem :cat)) :noun)
             (= (not (empty? (get-in lexical-entry '(:synsem :subcat)))))
             (not (= (get-in lexical-entry '(:synsem :pronoun)) true)))
        (unifyc lexical-entry {:synsem {:pronoun false}})
        true
        lexical-entry))

(defn put-a-bird-on-it [lexical-entry]
  "example lexical entry transformer."
  (cond (map? lexical-entry)
        (conj {:bird 42}
              lexical-entry)
        true
        lexical-entry))

(defn implied [map]
  "things to be added to lexical entries based on what's implied about them in order to canonicalize them."
  ;; for example, if a lexical entry is a noun with no :number value, or
  ;; the :number value equal to :top, then set it to :singular, because
  ;; a noun is canonically singular.
  ;; TODO: remove this first test: probably doesn't match anything
  ;; - should be (get-in map '(:synsem :cat)), not (:cat map).
  (let [map
        (if (or (= (get-in map '(:synsem :cat)) :det)
                (= (get-in map '(:synsem :cat)) :adverb))
          (unifyc
           subcat0
           map)
          map)

        map
        (if (and (= (get-in map '(:synsem :cat)) :adjective)
                 (not (= (get-in map '(:synsem :sem :comparative)) true)))
          (unifyc
           subcat1
           map)
          map)

        map
        (if (= (get-in map '(:synsem :cat)) :sent-modifier)
          (unifyc
           {:synsem {:subcat {:1 {:cat :verb
                                  :subcat '()}
                              :2 '()}}}
           map)
          map)

        ;; in italian, prepositions are always initial (might not need this); TODO: cleanup this commented-out code.
        map
        (if (= (get-in map '(:synsem :cat)) :prep)
          map map)
;          (let [italian (get-in map '(:italian))]
;            (if (string? italian)
;              (merge map
;                     {:italian {:italian italian
;                                :initial true}})
;              (unify map
;                     {:italian italian}
;                     {:italian {:initial true}})))
;          map)
        ]
    map))

;(def rules (list implied pronoun-rule put-a-bird-on-it sem-impl))
(def rules (list implied pronoun-rule sem-impl))

(defn transform [lexical-entry]
  "keep transforming lexical entries until there's no changes (isomorphic? input result) => true"
  (log/debug (str "Transforming: " (fo lexical-entry)))
  (log/debug (str "transform: input :" lexical-entry))
  (let [result (reduce unifyc (map (fn [rule] (apply rule (list lexical-entry)))
                                   rules))]
    (if (isomorphic? result lexical-entry)
      (cache-serialization result)
      (transform result))))

(def lexicon
  ;; this filter is for debugging purposes to restrict lexicon to particular entries, if desired.
  ;; default shown is (not (nil? entry)) i.e. no restrictions except that an entry must be non-nil.
  ;;  (currently there is one nil below: "chiunque (anyone)").
  (filter (fn [entry]
            (or false
                (not (nil? entry))))

          ;; TODO: move this fn to lexiconfn: keep any code out of the lexicon proper.
          ;; this (map) adds, to each lexical entry, a copy of the serialized form of the entry.
          (map transform
               (concat
                a-noi
                notizie-potere
                qualche_volta-volere
                ))))

