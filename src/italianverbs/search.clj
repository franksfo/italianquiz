(ns italianverbs.search
  (:use [hiccup core page-helpers])
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

(defn search [constraints]
  (gram/choose-lexeme constraints))

(defn searchq [search-exp attrs]
  "search with query. attrs is converted into filtering attribute-value pairs in the feature structures."
  (if search-exp
    (string/join " "
                 (map (fn [attr]
                        (let [constraints {(keyword attr) search-exp}
                              results (gram/choose-lexeme constraints)]
                          (if (and results
                                   (not (= (get results :cat) :error))) ;; currently 'no results found' is a {:cat :error}.
                            (html/fs (gram/choose-lexeme constraints)))))
                      (string/split (java.util.regex.Pattern/compile " ") (if attrs attrs "italian english"))))
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
     {:comment "show the first (database's choice) noun."
      :test (search {:cat :noun})}
     {:comment "show the first (database's choice) verb."
      :test (search {:cat :verb})}))




  