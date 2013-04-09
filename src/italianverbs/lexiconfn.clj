(ns italianverbs.lexiconfn
  (:use [clojure.set])
  (:require
   [clojure.core :as core]
   [somnium.congomongo :as mongo]
   [italianverbs.morphology :as morph]
   [italianverbs.fs :as fs]))

;; begin db-specific stuff. for now, mongodb; might switch/parameterize later.
(mongo/mongo! :db "mydb")
(mongo/make-connection "mydb" :host "localhost")

(defn unify [& args]
  "like fs/unify, but fs/copy each argument before unifying."
  (apply fs/unify
         (map (fn [arg]
                (fs/copy arg))
              args)))

(defn encode-where-query [& where]
  "encode a query as a set of index queries."
  where)

(defn fetch2 [& where]
  (let [where (encode-where-query where)]
    (mapcat (fn [entry]
              (let [deserialized (fs/deserialize (:entry entry))]
                (if (not (= (fs/unify deserialized where) :fail))
                  (list deserialized))))
            (mongo/fetch :lexicon))))

(defn fetch-all []
  (mapcat (fn [entry]
            (let [deserialized (fs/deserialize (:entry entry))]
              (list deserialized)))
          (mongo/fetch :lexicon)))

(defn fetch [& where]
  (if where
    (mongo/fetch :lexicon :where (first where))
    (mongo/fetch :lexicon)))

(defn fetch-one [& where]
  (if where 
    (mongo/fetch-one :lexicon :where (first where))
    (mongo/fetch-one :lexicon)))

(defn clear! [& args]
  (mongo/destroy! :lexicon {}))

(defn add-lexeme [fs]
  (mongo/insert! :lexicon {:entry (fs/serialize fs)})
  fs)

;; end db-specific stuff.

(defn italian [lexeme]
  (get (nth lexeme 1) :lexicon))

(defn synsem [lexeme]
  (nth lexeme 1))

(defn english [lexeme]
  (get (nth lexeme 1) :english))

(defn implied [map]
  "things to be added to lexical entries based on what's implied about them in order to canonicalize them."
  ;; for example, if a lexical entry is a noun with no :number value, or
  ;; the :number value equal to :top, then set it to :singular, because
  ;; a noun is canonically singular.
  (if (and (= (:cat map) :noun)
           (or (= (:number map :notfound) :notfound)
               (and (= (type (:number map)) clojure.lang.Ref)
                    (= @(:number map) :top))))
    (implied (fs/merge map
                       {:number :singular}))
    map))

;; italian and english are strings, featuremap is a map of key->values.
(defn add [italian english & featuremaps]
  (let [merged
        (apply fs/merge
               (concat (map #'fs/copy featuremaps) ;; copy here to prevent any structure sharing between new lexical entry on the one hand, and input featuremaps on the other.
                       (list {:english english}
                             {:italian italian})))]
    (add-lexeme (implied merged))))


;; _italian is a string; _types is a list of symbols (each of which is a map of key-values);
;; _result is an accumulator which contains the merge of all of the maps
;; in _types.
;; no _english param needed; _result should be assumed to contain a :root key-value.
(defn add-infl [italian & [types result]]
  (if (first types)
    (add-infl
     italian
     (rest types)
     (merge (first types) result))
    (add italian nil result)))

(def firstp
  {:person :1st})
(def secondp
  {:person :2nd})
(def thirdp
  {:person :3rd})
(def sing
  {:number :singular})
(def plural
  {:number :plural})
(def present
  {:cat :verb :infl :present})

(defn italian-pluralize [singular gender]
  (cond
   (= gender :masc)
   (replace #"([oe])$" "i" singular)
   (= gender :fem)
   (replace #"([a])$" "e" singular)))

(defn english-pluralize [singular]
  (str (replace #"([sxz])$" "$1e" singular) "s"))

(defn add-plural [fs types & [italian-plural english-plural]]
  (add
   (if italian-plural italian-plural
       (italian-pluralize (get fs :italian)
                          (get fs :gender)))
   (if english-plural english-plural
     (english-pluralize (get fs :english)))
   (fs/merge
    types
    fs
    {:det {:number :plural}}
    {:number :plural})))

(defn add-with-plural [italian english featuremap types & [italian-plural english-plural]]
  (add-plural
   (add italian english
        (fs/merge
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
     (merge (first types) result))
    (add italian nil (merge {:english english} result))))

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
