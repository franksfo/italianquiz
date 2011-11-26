(ns italianverbs.lexiconfn
  (:use [hiccup core page-helpers]
        [clojure.set]
        [italianverbs.rdutest]        
        )
  (:require
   [clojure.core :as core]
   [somnium.congomongo :as mongo]
   [clojure.contrib.string :as stringc]
   [italianverbs.morphology :as morph]
   [italianverbs.fs :as fs]))

;; begin db-specific stuff. for now, mongodb; might switch/parameterize later.
(mongo/mongo! :db "mydb")
(mongo/make-connection "mydb" :host "localhost")
(defn fetch [& where]
  (if where
    (mongo/fetch :lexicon :where (first where))
    (mongo/fetch :lexicon)))

(defn fetch-one [& where]
  (if where 
    (mongo/fetch-one :lexicon :where (first where))
    (mongo/fetch-one :lexicon)))

(defn clear [& args]
  (mongo/destroy! :lexicon {}))

(defn add-lexeme [fs]
  (mongo/insert! :lexicon fs)
  fs)

;; end db-specific stuff.

(defn italian [lexeme]
  (get (nth lexeme 1) :lexicon))

(defn synsem [lexeme]
  (nth lexeme 1))

(defn english [lexeme]
  (get (nth lexeme 1) :english))

;; italian and english are strings, featuremap is a map of key->values.
(defn add [italian english & featuremaps]
  (add-lexeme
   (apply fs/merge-like-core
          (concat featuremaps
                  (list {:english english}
                        {:italian italian})))))


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


(defn futuro-semplice [infinitive & [ prefix ]]
  "_infinitive_ should be a lexical entry."
  (let [english-modal
        (str
         "will "
         (if (get infinitive :future-english)
           (get infinitive :future-english)
           (get (morph/remove-to infinitive) :remove-to)))]
    (add
     (morph/conjugate-future-italian
      infinitive
      {:person :1st
       :number :singular}
      prefix)
     (str "(i) " english-modal)
     {:cat :verb
      :subj {:person :1st
             :number :singular}
      :infl :futuro-semplice
      :root infinitive})

    (add
     (morph/conjugate-future-italian
      infinitive
      {:person :2nd
       :number :singular}
      prefix)
     (str "(you) " english-modal)
     {:cat :verb
      :subj {:person :2nd
             :number :singular}
      :infl :futuro-semplice
      :root infinitive})
    
    (add
     (morph/conjugate-future-italian
      infinitive
      {:person :3rd
       :number :singular}
      prefix)
     (str "(he/she) " english-modal)
     {:cat :verb
      :subj {:person :3rd
             :number :singular}
      :infl :futuro-semplice
      :root infinitive})
    
    (add
     (morph/conjugate-future-italian
      infinitive
      {:person :1st
       :number :plural}
      prefix)
     (str "(we) " english-modal)
     {:cat :verb
      :subj {:person :1st
             :number :plural}
      :infl :futuro-semplice
      :root infinitive})
    (add
     (morph/conjugate-future-italian
      infinitive
      {:person :2nd
       :number :plural}
      prefix)
     (str "(you all) " english-modal)
     {:cat :verb
      :subj {:person :2nd
             :number :plural}
      :infl :futuro-semplice
      :root infinitive})
    (add
     (morph/conjugate-future-italian
      infinitive
      {:person :3rd
       :number :plural}
      prefix)
     (str "(they) " english-modal)
     {:cat :verb
      :subj {:person :3rd
             :number :plural}
      :infl :futuro-semplice
      :root infinitive})))



;; TODO: use a param map; this is getting unweildy: too many params.
;; right now, _fs_ is acting as the param map.
(defn add-with-pass-pross [italian-infinitive italian-pass-pross english-infinitive english-past avere-o-assere & [ fs present-indicative-list futuro-semplice-stem ]  ]
  "add an infinitive form of a verb and the participio passato form. _fs_ contains additional lexical info." 
  (let [futuro-semplice-stem
        (if futuro-semplice-stem
          futuro-semplice-stem
          (if (get fs :futuro-semplice-stem)
            (get fs :futuro-semplice-stem)))
        inf
        (add italian-infinitive english-infinitive
             (merge
              {:cat :verb
               :infl :infinitive}
              fs))
        first-sing (nth present-indicative-list 0)
        second-sing (nth present-indicative-list 1)
        third-sing (nth present-indicative-list 2)
        first-plur (nth present-indicative-list 3)
        second-plur (nth present-indicative-list 4)
        third-plur (nth present-indicative-list 5)]
    (add-infl
     (if first-sing first-sing
       (morph/conjugate-italian-verb-regular
        inf
        (merge firstp sing present)))
     (list (merge firstp sing present))
     {:root inf})

    (add-infl
     (if second-sing second-sing
         (morph/conjugate-italian-verb-regular
          inf
          (merge secondp sing present)))
     (list (merge secondp sing present))
     {:root inf})

    (add-infl
     (if third-sing third-sing
         (morph/conjugate-italian-verb-regular
          inf
          (merge thirdp sing present)))
     (list (merge thirdp sing present))
     {:root inf})
    
    (add-infl
     (if first-plur first-plur
         (morph/conjugate-italian-verb-regular
          inf
          (merge firstp plural present)))
     (list (merge firstp plural present))
     {:root inf})

    (add-infl
     (if second-plur second-plur
         (morph/conjugate-italian-verb-regular
          inf
          (merge secondp plural present)))
     (list (merge secondp plural present))
     {:root inf})

    (add-infl
     (if third-plur third-plur
         (morph/conjugate-italian-verb-regular
          inf
          (merge thirdp plural present)))
     (list (merge thirdp plural present))
     {:root inf})

    (add italian-pass-pross english-past
         {:cat :verb
          :root inf
          :infl :passato-prossimo
          :aux avere-o-assere})
    (futuro-semplice inf futuro-semplice-stem)))

(defn italian-pluralize [singular gender]
  (cond
   (= gender :masc)
   (stringc/replace-re #"([oe])$" "i" singular)
   (= gender :fem)
   (stringc/replace-re #"([a])$" "e" singular)))

(defn english-pluralize [singular]
  (str (stringc/replace-re #"([sxz])$" "$1e" singular) "s"))

(defn add-plural [fs types & [italian-plural english-plural]]
  (add
   (if italian-plural italian-plural
       (italian-pluralize (get fs :italian)
                          (get fs :gender)))
   (if english-plural english-plural
     (english-pluralize (get fs :english)))
   (fs/merge-like-core
    types
    fs
    {:det {:number :plural}}
    {:number :plural})))

(defn add-with-plural [italian english featuremap types & [italian-plural english-plural]]
  (add-plural
   (add italian english
        (fs/merge-like-core
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

(defn add-infl-reg [infinitive & [passato-prossimo past aux]]
  (let [firstp {:person :1st}
        secondp {:person :2nd}
        thirdp {:person :3rd}
        sing {:number :singular}
        plural {:number :plural}
        present {:cat :verb :infl :present}]
    (add-infl (morph/conjugate-italian-verb infinitive (merge firstp sing))
              (list firstp sing present
                    {:root infinitive}))

    (add-infl (morph/conjugate-italian-verb infinitive (merge secondp sing))
              (list secondp sing present
                    {:root infinitive}))

    (add-infl (morph/conjugate-italian-verb infinitive (merge thirdp sing))
              (list thirdp sing present
                    {:root infinitive}))

    (add-infl (morph/conjugate-italian-verb infinitive (merge firstp plural))
              (list firstp plural present
                    {:root infinitive}))

    (add-infl (morph/conjugate-italian-verb infinitive (merge secondp plural))
              (list secondp plural present
                    {:root infinitive}))

    (add-infl (morph/conjugate-italian-verb infinitive (merge thirdp plural))
              (list thirdp plural present
                    {:root infinitive}))

    (if (and passato-prossimo past aux)
      (add passato-prossimo past
           {:cat :verb
            :root infinitive
            :infl :passato-prossimo
            :aux aux}))))

(defn lookup [italian & [where]]
  (mongo/fetch-one :lexicon :where (merge where {:italian italian})))

;; for testing.
(def mangiare (lookup "mangiare"))

;; looking for transitive verbs (:obj|:cat = noun)
;; which happen in a place (:adjunct|:obj|:place = true).
;; result should include mangiare.
(def place-verbs
  {:cat :verb
   :obj {:cat :noun}
   :adjunct {:cat :prep
             :obj {:place true}}})

;;usage : (query (pathify place-verbs)))

;; transitive verbs only
;; result should include mangiare.
(def trans-verbs
  {:cat :verb
   :obj {:cat :noun}})

(def verbs
  {:cat :verb})

;; do: ((:3s tests) tests) to see test results.
;; do (:test-result ((:3s tests) tests) to see "foobaziare" structure.
(def tests
  {:parla
   (rdutest
    "A lexical entry for the word: 'parlare'."
    (let [verb {:cat :verb}
          human-subj {:subj {:human true}}
          third-sing {:subj {:number :singular :person :3rd}}
          parlare (add "parlare" "to speak"
                       verb
                       human-subj
                       {
                        :obj {:speakable true}
                        })
          merge (fs/merge-like-core
                 parlare
                 third-sing
                 {:root parlare})
          parla (add "parla" "speaks"
                     parlare
                     third-sing
                     {:root parlare})]
      {:merge merge
       :parla parla})
    (fn [merge-and-parla]
      (let [merge (:merge merge-and-parla)
            parla (:parla merge-and-parla)]
        (and
         (= (:cat parla) :verb)
         (= (fs/get-path parla (list :subj :human)) true)
;         (= (fs/get-path parla (list :root :subj :human)) true)
;         (= (fs/get-path parla (list :subj :number)) :singular)
;         (= (fs/get-path parla (list :subj :person)) :3rd)
;         (= (fs/get-path parla (list :obj :speakable)) true)
;         (= (:english parla) "speaks")
;         (= (:italian parla) "parla")
         )))
      
    :parla)})

;(def parla
;  (:test-result ((:3s tests) tests)))

;;usage : (run-query (pathify trans-verbs)))


