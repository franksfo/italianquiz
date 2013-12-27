(ns italianverbs.test.forest
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [clojure.set :refer :all]
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]

   [italianverbs.generate :as generate]
   [italianverbs.forest :exclude (lightning-bolt) ]
   [italianverbs.forest :as forest]
   [italianverbs.lexicon :refer :all]
   [italianverbs.morphology :refer (fo fo-ps)]
   [italianverbs.over :refer :all]
   [italianverbs.ug :refer :all]
   [italianverbs.unify :refer :all]))

(def parents (set (list (merge (unifyc cc10
                                       {:synsem {:infl :present
                                                 :cat :verb
                                                 :sem {:tense :present}}})
                               {:comment "parent1/cc10"})

                        (merge (unifyc hh21
                                       {:synsem {:infl :present
                                                 :cat :verb
                                                 :sem {:tense :present}}})
                               {:comment "parent2/hh21"})

                        (merge (unifyc cc10
                                       {:synsem {:cat :noun}})
                               {:comment "parent3/cc10"}))))

(def lex (seq (union (it "dormire")
                     (it "cane")
                     (it "gatto")
                     (it "i")
                     (it "il")
                     (it "io")
                     (it "la")
                     (it "lei")
                     (it "lui")
                     (it "mangiare")
                     (it "pane")
                     (it "pasta")
                     (it "sognare")
                     (it "tu"))))

(defn lightning-bolt [ & [head lexicon phrases depth] ]
  (let [maxdepth 2
        depth (if depth depth 0)
        lexicon (if lexicon lexicon lex)
        phrases (if phrases phrases parents)
        head (if head head :top)]
    (forest/lightning-bolt head lexicon phrases depth)))

(deftest sleeper-1 []
  (let [sleeper (get-in (first (lightning-bolt {:synsem {:sem {:pred :dormire}}})) '(:synsem :sem :subj))]
    (is (not (nil? sleeper)))))
                     (it "ragazza")
                     (it "ragazzo")
                     (it "ragazza")
                     (it "ragazzo")

(deftest i-sleep-1 []
  (let [i-sleep (first (lightning-bolt {:synsem {:sem {:subj {:pred :io}
                                                       :pred :dormire}}}))]
    (is (or (= (list "Io dormo (I sleep).") (fo i-sleep))
            (= (list "Io dormirò (I will sleep).") (fo i-sleep))
            (= (list "Io dormivo (I was sleeping).") (fo i-sleep))))))


(deftest human-sleeper-1 []
  (let [human-sleeper (get-in (first (lightning-bolt {:synsem {:sem {:subj {:human true}
                                                                     :pred :dormire}}}))
                               '(:synsem :sem :subj))]
    (is (= (get-in human-sleeper '(:human)) true))))

(deftest animal-sleeper-1 []
  (let [animal-sleeper (get-in (first (lightning-bolt {:synsem {:sem {:subj {:human false}
                                                                      :pred :dormire}}}))
                               '(:synsem :sem :subj))]
    (is (= (get-in animal-sleeper '(:human)) false))))

(deftest edible-1
  (let [edible (get-in (first (lightning-bolt {:synsem {:sem {:pred :mangiare}}})) '(:synsem :sem))]
    (is (not (nil? edible)))))

(deftest animal-eater-1 []
  (let [eating-semantics (get-in (first (lightning-bolt {:synsem {:sem {:subj {:human false}
                                                                        :pred :mangiare}}}))
                                 '(:synsem :sem))]
    (is (= (get-in eating-semantics '(:subj :human)) false))))



