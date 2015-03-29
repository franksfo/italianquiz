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

(deftest insert_new_game_en2es
  (let [source-spec {:synsem {:sem {:tense :futuro}}}
        target-spec {:head {:espanol {:espanol "enseñar"}}}
        source-group (insert-or-group "English future tense" [source-spec])
        target-group (insert-or-group "Enseñar: 'to show' or 'to teach'" [target-spec])]
    (let [game-id (insert-game "The Useful Spanish Game" "en" "es" source-group target-group)]
      (is (integer? game-id))
      (let [selects (selects-of-game game-id)]
        (is (map? selects))
        (is (= 1 (.size (:target selects))))
        (is (= 1 (.size (:source selects))))
        (is (= (first (:target selects))
               {:head {:espanol {:espanol :enseñar}}}))
        (is (= (first (:source selects))
               {:synsem {:sem {:tense :futuro}}}))))))
