(ns italianverbs.test.editor
  (:refer-clojure :exclude [get-in])
  (:require
   [clojure.data.json :as json]
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [clojure.test :refer :all]
   [italianverbs.borges.writer :refer [populate]]
   [italianverbs.engine :refer [generate]]
   [italianverbs.editor :refer :all]
   [italianverbs.engine :as engine]
   [italianverbs.english :as en]
   [italianverbs.espanol :as es]
   [italianverbs.italiano :as it]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.korma :as db]
   [italianverbs.unify :refer [get-in]]
   [italianverbs.verb :refer [generation-table predicates-from-lexicon]]
   [hiccup.core :refer (html)]
   [korma.core :as k]
))

(declare selects-of-game)

(deftest insert_new_game_en2es
  (let [future-tense (insert-grouping "Future tense" [{:synsem {:sem {:tense :futuro}}}])
        source-group future-tense
        target-group-1 (insert-grouping "Common Spanish verbs"
                                        [{:head {:espanol {:espanol "comer"}}}
                                         {:head {:espanol {:espanol "enseñar"}}}
                                         {:head {:espanol {:espanol "hablar"}}}])
        target-group-2 future-tense
        game-id (insert-game "The Useful Spanish Game" "en" "es" [source-group] [target-group-1 target-group-2])]
    (is (integer? game-id))
    (let [selects (selects-of-game game-id)]
      (is (map? selects))

      (is (= 1 (.size (:source selects))))
      (is (= 1 (.size (nth (:source selects) 0))))
      (is (= (first (first (:source selects)))
             {:synsem {:sem {:tense :futuro}}}))

      (is (= 2 (.size (:target selects))))
      (is (or (not (empty? (filter #(= % {:head {:espanol {:espanol :enseñar}}})
                                   (nth (:target selects) 0))))
              (not (empty? (filter #(= % {:head {:espanol {:espanol :enseñar}}})
                                   (nth (:target selects) 1))))))
      
      (is (or (not (empty? (filter #(= % {:head {:espanol {:espanol :hablar}}})
                                   (nth (:target selects) 0))))
              (not (empty? (filter #(= % {:head {:espanol {:espanol :hablar}}})
                                   (nth (:target selects) 1))))))

      (is (or (not (empty? (filter #(= % {:synsem {:sem {:tense :futuro}}})
                                   (nth (:target selects) 0))))
              (not (empty? (filter #(= % {:synsem {:sem {:tense :futuro}}})
                                   (nth (:target selects) 1)))))))


    ;; generate expressions in English and Spanish that should match our newly-created game's criteria,
    ;; so that (expressions-for-game <our game>) is not empty:
    (let [do-populate (do
                        (populate 1 en/small es/small {:synsem {:sem {:tense :futuro
                                                                      :subj {:pred :io}
                                                                      :pred :speak}}}))]
      
      (let [expressions (expressions-for-game 1)]
        (is (not (empty? expressions)))))))

;; scaffolding functions used to develop (expressions-for-game) (above).
(defn selects-of-game [game-id]
  (let [source-select 
        (str "SELECT source.any_of AS source
                FROM game 
          INNER JOIN grouping AS source
                  ON source.id = ANY(game.source_groupings)
               WHERE game.id=?")
        target-select
        (str "SELECT target.any_of AS target
                FROM game 
          INNER JOIN grouping AS target
                  ON target.id = ANY(game.target_groupings)
               WHERE game.id=?")
        source-results (k/exec-raw [source-select [game-id]] :results)
        target-results (k/exec-raw [target-select [game-id]] :results)]
    {:target (map (fn [target-result]
                    (map #(json/read-str %
                                         :key-fn keyword
                                         :value-fn (fn [k v]
                                                     (cond (string? v)
                                                           (keyword v)
                                                           :else v)))
                         (.getArray (:target target-result))))
                  target-results)
     :source (map (fn [source-result]
                    (map #(json/read-str %
                                         :key-fn keyword
                                         :value-fn (fn [k v]
                                                     (cond (string? v)
                                                           (keyword v)
                                                           :else v)))
                         (.getArray (:source source-result))))
                  source-results)}))

(defn expressions-for-target-groupings [game-id]
  (let [sql
        (str "SELECT target_expression.surface,
                     target_grouping.any_of 
                FROM expression AS target_expression
          INNER JOIN grouping AS target_grouping
                  ON target_expression.structure @> ALL(any_of)
          INNER JOIN game
                  ON (game.id=?
                 AND  target_grouping.id = ANY(game.source_groupings))
               WHERE (target_expression.language = game.target)")]
    (k/exec-raw [sql [game-id]] :results)))

;; Not used; an experiment that hasn't gone anywhere yet.
(def route-graph
  {:home {:create {:get "Create new game"}}
   :create {:home {:get "Cancel"}
            :create {:post {:button "New Game"}}}
   :read {:home {:get "Show all games"}}})
