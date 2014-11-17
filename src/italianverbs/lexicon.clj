(ns italianverbs.lexicon
  (:refer-clojure :exclude [get-in resolve find merge])
  (:require
   [clojure.set :refer (union)]
   [clojure.tools.logging :as log]
   [italianverbs.lexiconfn :refer (apply-multi-rules cache-serialization sem-impl subcat0 subcat1 transform)]
   ;; TODO: i.l.a_essere,i.l.esso_noi,i.l.notizie_potere,i.l.qualche_volte_volere are going away:
   ;; instead use i.l.italiano and i.l.english.
   [italianverbs.lex.a_essere :refer :all]
   [italianverbs.lex.esso_noi :refer :all]
   [italianverbs.lex.notizie_potere :refer :all]
   [italianverbs.lex.qualche_volta_volere :refer :all]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.pos :refer :all]
   [italianverbs.unify :as unify]
   [italianverbs.unify :refer :all]))

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
  (lookup-in query @lexicon))

(defn choose-lexeme [spec]
  (first (lazy-shuffle (lookup spec))))

(defn it [italian]
  "same as it but no type conversion of singleton sets to take the first member."
  (let [result
        (union (set (lookup {:italiano italian}))
               (set (lookup {:italiano {:infinitive italian}}))
               (set (lookup {:italiano {:infinitive {:infinitive italian}}}))
               (set (lookup {:italiano {:italiano italian}}))
               (set (lookup {:italiano {:irregular {:passato italian}}})))]
    result))

(defn en [english]
  (let [result
        (union (set (lookup {:english english}))
               (set (lookup {:english {:infinitive english}}))
               (set (lookup {:english {:infinitive {:infinitive english}}}))
               (set (lookup {:english {:english english}}))
               (set (lookup {:english {:irregular {:passato english}}})))]
    result))

(clear!)

(load "pos")

(load "lex/a_essere")
(load "lex/esso_noi")
(load "lex/notizie_potere")
(load "lex/qualche_volta_volere")

(def lexicon
  ;; this filter is for debugging purposes to restrict lexicon to particular entries, if desired.
  ;; default shown is (not (nil? entry)) i.e. no restrictions except that an entry must be non-nil.
  ;;  (currently there is one nil below: "chiunque (anyone)").
  (future
    (filter (fn [entry]
              (or (= entry :fail)
                  (not (nil? entry))))

            ;; TODO: move this fn to lexiconfn: keep any code out of the lexicon proper.
            ;; this (map) adds, to each lexical entry, a copy of the serialized form of the entry.

            (mapcat #(apply-multi-rules %)
                    (map transform
                         (concat
                          a-essere
                          esso-noi
                          notizie-potere
                          qualche_volta-volere))))))
