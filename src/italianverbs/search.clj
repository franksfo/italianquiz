(ns italianverbs.search
  (:use [hiccup core]
        [clojure.set])
  (:require
   [somnium.congomongo :as mongo]
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [italianverbs.fs :as fs]
   [italianverbs.html :as html]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.lev :as lev]))

;;(duck/spit "verbs.html"
;;      (html/static-page
;;            (str (html/fs lexfn/verbs) (string/join " " (map (fn [fs] (html/fs fs)) (lexfn/query (lexfn/pathify lexfn/verbs))))
;;
;;(duck/spit "trans-verbs.html"
;;      (html/static-page
;;            (str (html/fs lexfn/trans-verbs) (string/join " " (map (fn [fs] (html/fs fs)) (lexfn/query (lexfn/pathify lexfn/trans-verbs))))


;; TODO: remove *exclude-keys*,(pathify-r) and (pathify) in favor of fs's versions.
(def ^:dynamic *exclude-keys* (set #{:_id :ref :refmap}))

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
  (let [path-value (fs/get-in lexical-entry path)]
    (if (not (or (= path-value value)
                 (and (not (= (keyword path-value) nil))
                      (= (keyword path-value) value))))
      (list lexical-entry))))

(defn pv-matches [lexical-entry path value]
  "if lexical-entry has path, and the path's value is _value_, return a list containing _lexical-entry_. otherwise, return nil."
  ;; might need a more complicated equality predicate later.
  (if (= (last path) :not)
    (pv-not-matches lexical-entry (butlast path) value)
    (let [;debug (println (str "pv-matches: " lexical-entry " , " (seq path) " , " value))
          ;debug (println (str "result:" (fs/get-in lexical-entry path)))
          path-value (fs/get-in lexical-entry path)]
      (if (or (= path-value value)
              (and (= (type path-value) clojure.lang.Ref)
                   (= value @path-value))
              (= (keyword path-value) value)
              (and (not (nil? (fs/get-in lexical-entry path)))
                   (or (= value :top) ; searching for :top means: find any entry that has any value at all for the path (but it must have _some_ value).
                       (= value "top"))))  ; TODO: should not need to check for "top" (string): should only have to check for :top.
        (list lexical-entry)
        (do
;          (println (str "FAILED TO MATCH: " path-value " and " value))
          nil)))))

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
;          debug (println (str "path=" (seq path) "; value=" value))
          result (set ;; <- removes duplicates
                  (mapcat
                   (fn [entry]
                     (pv-matches entry path value))
                   lexicon))]
      (if (= (.size result) 0)
        ;; no results for _path_:_value_: short-circuit: return emptyset without trying remaining path-value-pairs,
        ;; since ultimate result will be emptyset regardless of remaining path-value-pairs.
        result
        (if (> (.size path-value-pairs) 1)
          (intersection result (query-r (rest path-value-pairs) lexicon))
          result)))
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
        ;; TODO: Find out: does calling (set) on (already) a set have
        ;; a penalty?
        pathified
        (mapcat (fn [constraint]
                  (pathify constraint))
                constraints)]
;    (println (str "query-with-lexicon: " (seq pathified)))
    (log/debug (str "query-with-lexicon: " (seq pathified)))
    (let [result
          (query-r pathified lexicon)]
      (if (nil? result)
        (do
;          (println  (str "searching with constraints : " constraints))
          (log/info "(returned null)")
          result)
        result))))

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
  (do
    (log/info (str "searchq: searching with search-exp: " search-exp " and attrs: " attrs))
    (if search-exp
      (string/join " "
                   (concat
                    (map (fn [attr]
                           (let [constraints {(keyword attr) search-exp}
                                 results (lexfn/choose-lexeme constraints)]
                             (if (and results
                                      (not (= (get results :cat) :error))) ;; currently 'no results found' is a {:cat :error}.
                               (html/fs (lexfn/choose-lexeme constraints)))))
                         (string/split (if attrs attrs "italian english") #"[ ]+"))
                    (mapcat (fn [search-term]
                              (let [grammatical-terminology-term (get
                                                                  grammatical-terminology-term
                                                                  (keyword search-term))]
                                (if grammatical-terminology-term
                                  (map (fn [fs]
                                         (str
                                          (html/tablize fs)))
                                       (query (pathify grammatical-terminology-term))))))
                            (string/split search-exp #"[ ]+"))
                    (let [loaded
                          (try
                            (load-string search-exp) ;; SECURITY: clean search-exp before evaluating.
                            (catch Exception e
                              (log/error (str "failed to load-string: " search-exp))))]
                      (if (= (type loaded) clojure.lang.PersistentArrayMap)
                        (list
                         (str
                          "<div class='evalresult'>"
                          (html/tablize loaded)
                          "</div>"))
                        ;; not a map: just stringify result of
                        ;; evaluation
                        (list
                         (str
                          "<div class='evalresult'>"
                          (str "<div style='font-family:monospace'>" loaded "</div>")
                          "</div>"))))))
      nil)))

(defn search-ui [request]
  (let [search-query (get (get request :query-params) "search")]
    (html
     [:div#search-ui {:class "quiz-elem"}
      [:h2 "cerca"]
      [:div#searchbar
       [:input {:size "50" :id "search" :type "text"  :value search-query }]
       [:button {:onclick "search()"} "cerca.."]]
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

