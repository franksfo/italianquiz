(ns italianverbs.mongo
  (:refer-clojure :exclude [get-in merge resolve find])
  (:use [clojure.set])
  (:require
   [clojure.set :as set]
   [clojure.tools.logging :as log]
   [clojure.core :as core]
   [italianverbs.lexiconfn :refer (encode-where-query italian-pluralize english-pluralize unify)]
   [italianverbs.morphology :as morph]
   ;; We redefine unify here: TODO: just use unifyc where appropriate.
   [italianverbs.unify :as unify :exclude (unify)]
   [somnium.congomongo :as congo]))

(congo/mongo! :db "mydb")
(congo/make-connection "mydb" :host "localhost")

(defn fetch2 [& where]
  (let [where (encode-where-query where)]
    (mapcat (fn [entry]
              (let [deserialized (unify/deserialize (:entry entry))]
                (if (not (= (unify deserialized where) :fail))
                  (list deserialized))))
            (congo/fetch :lexicon))))

(defn fetch-all []
  (mapcat (fn [entry]
            (let [deserialized (unify/deserialize (:entry entry))]
              (list deserialized)))
          (congo/fetch :lexicon)))

(defn fetch [& where]
  (if where
    (congo/fetch :lexicon :where (first where))
    (congo/fetch :lexicon)))

(defn fetch-one [& where]
  (if where
    (congo/fetch-one :lexicon :where (first where))
    (congo/fetch-one :lexicon)))

(defn clear! [& args]
  (congo/destroy! :lexicon {}))

(defn add-lexeme [fs]
  (congo/insert! :lexicon {:entry (unify/serialize fs)})
  fs)

(defn choose-lexeme [ & [struct dummy]]
  "Choose a random lexeme from the set of lexemes
   that match search criteria.
   dummy: ignored for compatibility with gram/np"
  ;; do a query based on the given struct,
  ;; and choose a random element that satisfies the query.
  (let [results (fetch struct)]
    (if (= (count results) 0)
      {:english "??" :italian "??"
       :cat :error :note (str "<tt>(choose-lexeme)</tt>: no results found. <p/>See <tt>:choose</tt> feature below for query.")
       :choose struct
       }
      (nth results (rand-int (count results))))))

;; end db-specific stuff.

;; italian and english are strings, featuremap is a map of key->values.
(defn add [italian english & featuremaps]
  (let [merged
        (apply unify/merge
               (concat (map #'unify/copy featuremaps) ;; copy here to prevent any structure sharing between new lexical entry on the one hand, and input featuremaps on the other.
                       (list {:english english}
                             {:italian italian})))]
    (add-lexeme merged)))

;; _italian is a string; _types is a list of symbols (each of which is a map of key-values);
;; _result is an accumulator which contains the merge of all of the maps
;; in _types.
;; no _english param needed; _result should be assumed to contain a :root key-value.
(defn add-infl [italian & [types result]]
  (if (first types)
    (add-infl
     italian
     (rest types)
     (unify/merge (first types) result))
    (add italian nil result)))

(defn add-plural [fs types & [italian-plural english-plural]]
  (add
   (if italian-plural italian-plural
       (italian-pluralize (get fs :italian)
                          (get fs :gender)))
   (if english-plural english-plural
     (english-pluralize (get fs :english)))
   (unify/merge
    types
    fs
    {:det {:number :plural}}
    {:number :plural})))

(defn add-with-plural [italian english featuremap types & [italian-plural english-plural]]
  (add-plural
   (add italian english
        (unify/merge
         types
         featuremap
         {:det {:number :singular}}
         {:number :singular}))
   types
   italian-plural english-plural))

;; _italian and _english are strings; _types is a list of symbols (each of which is a map of key-values);
;; _result is an accumulator which is the merge of all of the maps in _types.
;; Key-values in earlier types have precedence over those in later types
;; (i.e. the later key-value pair do NOT override original value for that key).
(defn add-as [italian english & [types result]]
  (if (first types)
    (add-as
     italian
     english
     (rest types)
     (unify/merge (first types) result))
    (add italian nil (unify/merge {:english english} result))))
