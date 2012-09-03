(ns italianverbs.search
  (:use [hiccup core]
        [clojure.set])
  (:require
   [somnium.congomongo :as mongo]
   [clojure.contrib.logging :as log]
   [clojure.contrib.string :as string]
   [clojure.contrib.repl-utils :as repl-utils]
   [italianverbs.fs :as fs]
   [italianverbs.html :as html]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.lev :as lev]
   [italianverbs.grammar :as gram]
   [clojure.contrib.duck-streams :as duck]))

;;(duck/spit "verbs.html"
;;      (html/static-page
;;            (str (html/fs lexfn/verbs) (string/join " " (map (fn [fs] (html/fs fs)) (lexfn/query (lexfn/pathify lexfn/verbs))))
;;
;;(duck/spit "trans-verbs.html"
;;      (html/static-page
;;            (str (html/fs lexfn/trans-verbs) (string/join " " (map (fn [fs] (html/fs fs)) (lexfn/query (lexfn/pathify lexfn/trans-verbs))))


;; TODO: remove *exclude-keys*,(pathify-r) and (pathify) in favor of fs's versions.
(def *exclude-keys* (set #{:_id :ref :refmap}))

(defn pathify-r [fs & [prefix]]
"Transform a map into a map of paths/value pairs,
 where paths are lists of keywords, and values are atomic values.
 e.g.:
 {:foo {:bar 42, :baz 99}} =>  { { (:foo :bar) 42}, {(:foo :baz) 99} }
The idea is to map the key :foo to the (recursive) result of pathify on :foo's value."
  (mapcat (fn [kv]
            (let [key (first kv)
                  val (second kv)]
;              (println (str "K:" key))
              (if (not (contains? *exclude-keys* key))
                (if (or (= (type val) clojure.lang.PersistentArrayMap)
                        (= (type val) clojure.lang.PersistentHashMap))
                  (do
;                    (println (str "PAM"))
                    (pathify-r val (concat prefix (list key))))
                  (if (and (= (type val) clojure.lang.Ref)
                           (let [val @val]
                             (or (= (type val) clojure.lang.PersistentArrayMap)
                                 (= (type val) clojure.lang.PersistentHashMap))))
                    (pathify-r @val (concat prefix (list key)))
                  (do
;                    (println (str "not PAM" (type val)))
                    (list {(concat prefix (list key))
                           (if (= (type val) clojure.lang.Ref) @val ;; simply resolve references rather than trying to search for graph isomorphism.
                               val)})))))))
          fs))

(defn pathify [fs]
  (pathify-r fs))

(defn pv-not-matches [lexical-entry path value]
  (let [path-value (get-in lexical-entry path)]
    (if (not (or (= path-value value)
                 (and (not (= (keyword path-value) nil))
                      (= (keyword path-value) value))))
      (list lexical-entry))))

(defn pv-matches [lexical-entry path value]
  "might need a more complicated equality predicate later."
  (if (= (last path) :not)
    (pv-not-matches lexical-entry (butlast path) value)
    (let [path-value (get-in lexical-entry path)]
      (if (or (= path-value value)
              (and (= (type path-value) clojure.lang.Ref)
                   (= value @path-value))
              (= (keyword path-value) value)
              (and (not (nil? (get-in lexical-entry path)))
                   (or (= value :top) ; searching for :top means: find any entry that has any value at all for the path (but it must have _some_ value).
                       (= value "top"))))  ; TODO: should not need to check for "top" (string): should only have to check for :top.
        (list lexical-entry)))))

;; http://stackoverflow.com/questions/2352020/debugging-in-clojure/2352280#2352280
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

;; TODO: use recur:
;; see http://clojure.org/functional_programming#Functional Programming--Recursive Looping
(defn query-r [path-value-pairs lexicon]
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
          result (set ;; <- removes duplicates
                  (mapcat
                   (fn [entry]
                     (pv-matches entry path value))
                   lexicon))]
      (if (> (.size path-value-pairs) 1)
        (intersection result (query-r (rest path-value-pairs) lexicon))
        result))
    #{})) ;; base case : return an empty set.

(defn query [& constraints]
  (query-r (mapcat (fn [constraint]
                     (pathify constraint))
                   constraints)
           (map (fn [entry]
                  (fs/deserialize (:entry entry)))
                (lexfn/fetch)))) ;; <- fetch the entire lexicon (!)

(defn query-with-lexicon [lexicon & constraints]
  "search the supplied lexicon for entries matching constraints."
  (let [lexicon (set lexicon) ;; hopefully O(1) if _lexicon_ is already a set.
        pathified
        (mapcat (fn [constraint]
                  (pathify constraint))
                constraints)]
    (println (str "pathified:" (seq pathified)))
    ;; TODO: Find out: does calling (set) on (already) a set have
    ;; a penalty?
    (query-r pathified lexicon)))

;; How to map over (fetch :lexicon) results:
;; 
;; (get all lexical items with path=>value: :obj/:cat => "noun")
;; 1. (defn myfn [fs] (= (get (get fs :obj) :cat) "noun"))
;; 
;; 2. (def results (mapcat (fn [fs] (if (myfn fs) (list fs))) (fetch :lexicon)))
;;
(defn search [& constraints]
  ;; TODO: figure out how to get log/info to print to console in REPL.
  (log/info (str "searching with constraints : " constraints))
  (println  (str "searching with constraints : " constraints))
  (if (= (first constraints) :fail)
    (list :fail)
;; TODO: s/query/create-query/
    (seq (apply query constraints))))

;; convenience function:search-one: find the first lexeme that matches constraints.
(defn search-one [& constraints]
  (first (apply search constraints)))

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

;; example usage: (take 5 (lazy-query {:cat :verb}))
(defn lazy-query [search]
  (lazy-seq (query search)))

(defn non-empty-set [set]
  (> (.size set) 0))

(defn empty-set [set]
  (= (.size set) 0))

;; TODO: remove generate.clj version.
(defn random-lexeme [& constraints]
  (let [lexemes (seq (apply query constraints))]
    (if lexemes
      (if (> (.size lexemes) 0)
        (nth lexemes (rand-int (.size lexemes)))))))

;; convenience fn for preceding fn.
(defn random [& constraints]
  (apply random-lexeme constraints))

(defn lookup [italian & [where]]
  (first (search {:italian italian})))

