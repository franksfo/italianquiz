(ns italianverbs.search
  (:use [hiccup core page-helpers])
  (:require
   [italianverbs.html :as html]
   [italianverbs.lexicon :as lexicon]
   [italianverbs.lev :as lev]
   [italianverbs.grammar :as gram]
   [italianverbs.morphology :as morph]))

(defn search [constraints]
  (gram/choose-lexeme constraints))

(defn searchq [search-exp]
  "search with query."
  (let [constraints {:italian search-exp}]
    (html/fs (gram/choose-lexeme constraints))))

(defn search-ui [request]
  (html
   [:div#search-ui
    [:div#searchbar
     [:input {:size "50" :id "search" :type "text"}]
     [:button {:onclick "search()"} "Search"]]
    
    [:div#searchresults "" ]]))


(defn test []
  (list
     {:comment "show the first (database's choice) noun."
      :test (search {:cat :noun})}
     {:comment "show the first (database's choice) verb."
      :test (search {:cat :verb})}))




  