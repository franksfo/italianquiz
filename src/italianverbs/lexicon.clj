(ns italianverbs.lexicon
  (:refer-clojure :exclude [get-in merge resolve find])
  (:require
   [clojure.set :refer (union)]
   [clojure.tools.logging :as log]
   [italianverbs.lexiconfn :refer (implied)]
   [italianverbs.lex.a_noi :refer :all]
   [italianverbs.lex.notizie_potere :refer :all]
   [italianverbs.lex.qualche_volta_volere :refer :all]
   [italianverbs.unify :as unify]
   [italianverbs.unify :refer (fail? get-in serialize)]))


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

(defn it [italian]
  "same as it but no type conversion of singleton sets to take the first member."
  (let [result
        (union (set (lookup {:italian italian}))
               (set (lookup {:italian {:infinitive italian}}))
               (set (lookup {:italian {:infinitive {:infinitive italian}}}))
               (set (lookup {:italian {:italian italian}}))
               (set (lookup {:italian {:irregular {:passato italian}}})))]
    result))

(def it1 it) ; backwards compatibility

(defn en [english]
  (lookup {:english english}))

(clear!)

(def lexicon

   ;; this filter is for debugging purposes to restrict lexicon to particular entries, if desired.
   ;; default shown is (not (nil? entry)) i.e. no restrictions except that an entry must be non-nil.
   ;;  (currently there is one nil below: "chiunque (anyone)").
   (filter (fn [entry]
             (or false
                 (not (nil? entry))))

           ;; TODO: move this fn to lexiconfn: keep any code out of the lexicon proper.
           ;; this (map) adds, to each lexical entry, a copy of the serialized form of the entry.
           (map (fn [entry]
                  (if (fail? entry)
                   (log/warn (str "Ignoring this lexeme because (fail?=true): " entry))
                   ;; else, not fail, so add to lexicon.
                   (do
                     (log/debug (str "serializing entry: " entry))
                     (let [italian (get-in entry '(:italian))
                           entry
                           (conj
                            {:italian (if (string? italian)
                                        {:italian italian}
                                        italian)}
                    (dissoc
                     (if (not (= :none (get entry :serialized :none)))
                       (conj {:serialized (serialize entry)}
                             entry)
                       (conj {:serialized (serialize (dissoc entry :serialized))}
                         entry))
                     :italian))]
                       (log/debug (str "successfully serialized: " entry))
                       (implied entry)))))
               (concat
                a-noi
                notizie-potere
                qualche_volta-volere
                ))))
