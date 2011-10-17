(ns italianverbs.search
    (:require [italianverbs.lexicon :as lexicon]
              [italianverbs.lev :as lev]
              [italianverbs.grammar :as gram]
              [italianverbs.morphology :as morph]))

(defn search [constraints]
  (gram/choose-lexeme constraints))

(defn test []
  (list
     {:comment "show the first (database's choice) noun."
      :test (search {:cat :noun})}
     {:comment "show the first (database's choice) verb."
      :test (search {:cat :verb})}))




  