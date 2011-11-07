(ns italianverbs.search
  (:use [hiccup core page-helpers]
        [clojure.set])
  (:require
   [clojure.contrib.string :as string]
   [italianverbs.html :as html]
   [italianverbs.lexicon :as lexicon]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.lev :as lev]
   [italianverbs.grammar :as gram]
   [italianverbs.morphology :as morph]
   [clojure.contrib.duck-streams :as duck]))

;;(duck/spit "verbs.html"
;;      (html/static-page
;;            (str (html/fs lexfn/verbs) (string/join " " (map (fn [fs] (html/fs fs)) (lexfn/query (lexfn/pathify lexfn/verbs))))
;;
;;(duck/spit "trans-verbs.html"
;;      (html/static-page
;;            (str (html/fs lexfn/trans-verbs) (string/join " " (map (fn [fs] (html/fs fs)) (lexfn/query (lexfn/pathify lexfn/trans-verbs))))

(defn pathify-r [fs & [prefix]]
"Transform a map into a map of paths/value pairs,
 where paths are lists of keywords, and values are atomic values.
 e.g.:
 {:foo {:bar 42, :baz 99}} =>  { { (:foo :bar) 42}, {(:foo :baz) 99} }
The idea is to map the :feature foo to the (recursive) result of pathify on :foo's value."
  (mapcat (fn [kv]
            (let [key (first kv)
                  val (second kv)]
              (if (= (type val) clojure.lang.PersistentArrayMap)
                (pathify-r val (concat prefix (list key)))
                (list {(concat prefix (list key))
                       val}))))
          fs))

(defn pathify [fs]
  (pathify-r fs))

(defn get-path [fs path]
  (if (> (.size path) 0)
    (get-path (get fs (first path))
              (rest path))
    fs))

(defn pv-not-matches [lexical-entry path value]
  (let [path-value (get-path lexical-entry path)]
    (if (not (or (= path-value value)
                 (= (keyword path-value) value)))
      (list lexical-entry))))

(defn pv-matches [lexical-entry path value]
  "might need a more complicated equality predicate later."
  (if (= (last path) :not)
    (pv-not-matches lexical-entry (butlast path) value)
    (let [path-value (get-path lexical-entry path)]
      (if (or (= path-value value)
              (= (keyword path-value) value))
        (list lexical-entry)))))

;; http://stackoverflow.com/questions/2352020/debugging-in-clojure/2352280#2352280
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

;; TODO: use recur:
;; see http://clojure.org/functional_programming#Functional Programming--Recursive Looping
(defn query-r [path-value-pairs]
  "
      map(lexicon,<p1,v1>)  => S1 |
      map(lexicon,<p2,v2>)  => S2 |
       ...                        |=> intersection(s:S) => return value.
       ...                        |
      map(lexicon,<pn,vn>)  => Sn |
  "
  (if (> (.size path-value-pairs) 0)
    (let [path (first (keys (first path-value-pairs)))
          value (get (first path-value-pairs) path)
          result (set (mapcat
                       (fn [entry] (pv-matches entry path value))
                       (lexfn/fetch)))]
      (if (> (.size path-value-pairs) 1)
        (intersection result (query-r (rest path-value-pairs)))
        result))
    #{})) ;; base case : return an empty set.

(defn query [& constraints]
  (query-r (pathify (first constraints))))

;; test data for (run-query)
;; (pathify transitive-verb) returns a list of path-value-pairs
;; which can be passed to run-query (above). run-query
;; does an intersection over the entire lexicon with each
;; path-value-pair as a filter.
(def tv {:cat "verb" :obj {:cat "noun"}})

(defn myfn [fs] (= (get-path fs '(:obj :cat)) "noun"))

;; How to map over (fetch :lexicon) results:
;; 
;; (get all lexical items with path=>value: :obj/:cat => "noun")
;; 1. (defn myfn [fs] (= (get (get fs :obj) :cat) "noun"))
;; 
;; 2. (def results (mapcat (fn [fs] (if (myfn fs) (list fs))) (fetch :lexicon)))
;;
(defn search [constraints]
  (seq (query constraints)))

(def grammatical-terminology-term
  {:transitive {:cat :verb
                :obj {:cat :noun}}
   :legible {:legible true}
   :lit-verbs {:cat :verb
              :obj {:legible true}}
   :place-preps {:cat :prep
                   :obj {:place true}}})


(defn searchq [search-exp attrs]
  "search with query. attrs is converted into filtering attribute-value pairs in the feature structures."
  (if search-exp
    (string/join " "
                 (concat
                  (map (fn [attr]
                         (let [constraints {(keyword attr) search-exp}
                               results (gram/choose-lexeme constraints)]
                           (if (and results
                                    (not (= (get results :cat) :error))) ;; currently 'no results found' is a {:cat :error}.
                             (html/fs (gram/choose-lexeme constraints)))))
                       (string/split (java.util.regex.Pattern/compile " ") (if attrs attrs "italian english")))
                  (mapcat (fn [search-term]
                            (let [grammatical-terminology-term (get grammatical-terminology-term (keyword search-term))]
                              (if grammatical-terminology-term
                                (map (fn [fs] (html/fs fs)) (query (pathify grammatical-terminology-term))))))
                          (string/split (java.util.regex.Pattern/compile " ") search-exp))))
    nil))

(defn search-ui [request]
  (let [search-query (get (get request :query-params) "search")]
    (html
     [:div#search-ui {:class "quiz-elem"}
      [:h2 "cerca"]
      [:div#searchbar
       [:input {:size "50" :id "search" :type "text"  :value search-query }]
       [:button {:onclick "search()"} "cerca"]]
      [:div#searchresults
       (if search-query
         (searchq search-query nil))]])))

(defn test []
  (list
     {:comment "show the count of nouns."
      :test (.size (search {:cat :noun}))}
     {:comment "show the count of verbs."
      :test (.size (search {:cat :verb}))}
     {:comment "simple query"
      ;; :assert (> (.size results-of-query 0))
      :test (nth (seq (query {:cat :noun})) 0)}
     {:comment "2-conjuncts query"
      ;; :assert (> (.size results-of-query 0))
      :test (nth (seq (query {:cat :noun :case :nom})) 0)}
     {:comment "2-conjuncts query, with negation"
      ;; :assert (> (.size results-of-query 0)).
      :test (nth (seq (query {:cat :noun :case {:not :nom}})) 0)}
     (let [test (intersection (query {:cat :noun :case :nom}) (query {:cat :noun :case {:not :nom}}))]
       {:comment "null set intersection of mutually-exclusive queries."
        ;; :assert: should be empty set (= (.size results) 0).
        :test test
        :result (= (.size test) 0)})
     {:comment "nested query"
      :test (nth (seq (query {:cat :verb :obj {:edible true}})) 0)}))
