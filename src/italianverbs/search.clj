(ns italianverbs.search
  (:use [hiccup core page-helpers])
  (:require
   [clojure.contrib.string :as string]
   [italianverbs.html :as html]
   [italianverbs.lexicon :as lexicon]
   [italianverbs.lev :as lev]
   [italianverbs.grammar :as gram]
   [italianverbs.morphology :as morph]))

(defn search [constraints]
  (gram/choose-lexeme constraints))

(defn searchq [search-exp attrs]
  "search with query."
  (string/join " "
               (map (fn [attr]
                      (let [constraints {(keyword attr) search-exp}
                            results (gram/choose-lexeme constraints)]
                        (if (and results
                                 (not (= (get results :cat) :error))) ;; currently 'no results found' is a {:cat :error}.
                          (html/fs (gram/choose-lexeme constraints)))))
                    (string/split (java.util.regex.Pattern/compile " ") attrs))))

(defn search-ui [request]
  (html
   [:div#search-ui {:class "quiz-elem"}
    [:h2 "cherca"]
    [:div#searchbar
     [:input {:size "50" :id "search" :type "text"}]
     [:button {:onclick "search()"} "cherca"]]
    [:div#searchresults "" ]]))

(defn test []
  (list
     {:comment "show the first (database's choice) noun."
      :test (search {:cat :noun})}
     {:comment "show the first (database's choice) verb."
      :test (search {:cat :verb})}))




  