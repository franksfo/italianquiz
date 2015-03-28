(ns italianverbs.test.editor
  (:refer-clojure :exclude [get-in])
  (:require
   [clojure.data.json :as json]
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [clojure.test :refer :all]
   [italianverbs.editor :refer :all]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.korma :as db]
   [italianverbs.unify :refer [get-in]]
   [italianverbs.verb :refer [generation-table predicates-from-lexicon]]
   [hiccup.core :refer (html)]
   [korma.core :as k]
))

(def select "SELECT surface FROM expression LIMIT 10")

(def results (k/exec-raw [select] :results))

(def table-results
  (html
   [:table
    (map (fn [result]
           [:tr [:td (:surface result)]])
         results)]))

(deftest insert_new_select_en2it
  (let [source-spec (json/write-str {:synsem {:sem {:tense :futuro}}})
        target-spec (json/write-str {})]
    (insert-constraint "Future Tense" "en" "it" source-spec target-spec)))

(deftest insert_new_select_en2es
  (let [source-spec (json/write-str {})
        target-spec (json/write-str {:head {:espanol {:espanol "enseñar"}}})]
    (insert-constraint "Enseñar: 'to show' or 'to teach'" "en" "es" source-spec target-spec)))

(deftest get-select
  (let [results (set (mapcat vals (k/exec-raw ["SELECT source,target,source_spec,target_spec FROM translation_select"] 
                                              :results)))]
    (log/debug (str "results: " results))))

(def foo (:target (nth (show-selects-of-game 5) 0)))

(def bar (.getArray foo))

(def baz (map #(json/read-str %)
              bar))
