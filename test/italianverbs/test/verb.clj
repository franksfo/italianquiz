(ns italianverbs.test.verb
  (:require
   [clojure.test :refer :all]
   [italianverbs.italiano :as it]
   [italianverbs.verb :refer :all]))

(deftest predicates-from-lexicon-given-italian
  (is (not (empty? (predicates-from-lexicon @it/lexicon)))))





   
