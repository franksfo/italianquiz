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

(deftest insert_new_game_en2es
  (let [source-group (insert-grouping "English future tense" [{:synsem {:sem {:tense :futuro}}}])
        target-group-1 (insert-grouping "Common Spanish verbs"
                                        [{:head {:espanol {:espanol "comer"}}}
                                         {:head {:espanol {:espanol "enseñar"}}}
                                         {:head {:espanol {:espanol "hablar"}}}])
        target-group-2 (insert-grouping "Spanish future tense" [{:synsem {:sem {:tense :futuro}}}])
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





