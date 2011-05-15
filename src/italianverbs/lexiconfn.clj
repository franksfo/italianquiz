(ns italianverbs.lexiconfn
  (:use [hiccup core page-helpers]
	[somnium.congomongo])
  (:require
   [clojure.contrib.string :as stringc]))

; global initializations go here, i guess..
(mongo! :db "mydb")
(make-connection "mydb" :host "localhost")

(defn italian [lexeme]
  (get (nth lexeme 1) :lexicon))

(defn synsem [lexeme]
  (nth lexeme 1))

(defn english [lexeme]
  (get (nth lexeme 1) :english))

;; CRUD-like functions:
(defn add-fs [fs]
  (let [function-to-symbol fs]
    (insert! :lexicon fs)
    fs))

;; italian and english are strings, featuremap is a map of key->values.
(defn add [italian english & [featuremap types result]]
  (if (first types)
    (add
     italian
     english
     featuremap
     (rest types)
     (merge (first types) result))
    (let [featuremap
          (merge featuremap
                 (merge result
                        (if english
                          (assoc {} :italian italian :english english)
                          (assoc {} :italian italian))))]
      (add-fs featuremap))))

(defn add-with-pass-pross [italian-infinitive italian-pass-pross english-infinitive english-past avere-o-assere & [ fs ]  ]
  "add an infinitive form of a verb and the participio passato form. _fs_ contains additional lexical info." 
  (let [inf
        (add italian-infinitive italian-pass-pross
             (merge
              {:cat :verb}
              fs))]
    (add italian-pass-pross english-past
         {:cat :verb
          :root inf
          :infl :passato-prossimo
          :aux avere-o-assere})))

  


(defn italian-pluralize [singular gender]
  (cond
   (= gender :masc)
   (stringc/replace-re #"([oe])$" "i" singular)
   (= gender :fem)
   (stringc/replace-re #"([a])$" "e" singular)
   true (str "error: gender: " gender  " unknown.")))

(defn english-pluralize [singular]
  (str (stringc/replace-re #"([sxz])$" "$1e" singular) "s"))

(defn add-plural [fs types & [italian-plural english-plural]]
  (add
   (if italian-plural italian-plural
       (italian-pluralize (get fs :italian)
                          (get fs :gender)))
   (if english-plural english-plural
     (english-pluralize (get fs :english)))
   (merge
    fs 
    {:number :plural})
   types))

(defn add-with-plural [italian english featuremap types & [italian-plural english-plural]]
  (add-plural
   (add italian english
        (merge featuremap
               {:number :singular})
               types)
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

(defn lookup [italian & [where ]]
  (fetch-one :lexicon :where (merge where {:italian italian})))

(defn clear []
  (destroy! :lexicon {}))

