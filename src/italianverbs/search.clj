(ns italianverbs.search
    (:require [italianverbs.lexicon :as lexicon]
              [italianverbs.lev :as lev]
              [italianverbs.grammar :as gram]
              [italianverbs.html :as html] ;tmp hopefully; html package should be able to handle retval type of gram/choose-lexeme.
              [italianverbs.morphology :as morph]))

;; sadly cannot pass params yet.

(defn search-verbs []
  (gram/choose-lexeme {:cat :verb}))

(defn search-nouns []
  (gram/choose-lexeme {:cat :noun}))


(defn search [& constraints]
  (if constraints
                                        ;    (gram/choose-lexeme {:cat :verb})
    ; below does not work for some reason:
                                        ;    (gram/choose-lexeme constraints)
  (gram/choose-lexeme)))


(defn test []
  (list
     {:comment "show the first (database's choice) noun."
      :test (search-nouns)}
     {:comment "show the first (database's choice) verb."
      :test (search-verbs)}))




  