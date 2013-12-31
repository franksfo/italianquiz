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
   [italianverbs.ug :refer :all]
   [italianverbs.unify :refer :all]))

(def parents (list (merge (unifyc cc10
                                  {:synsem {:infl :present
                                            :cat :verb
                                            :sem {:tense :present}}})
                          {:comment "s-present"})

                   (merge (unifyc cc10
                                  {:synsem {:infl :futuro
                                            :cat :verb}})
                          {:comment "s-future"})

                   (merge (unifyc cc10
                                  {:synsem {:infl :imperfetto
                                            :cat :verb}})
                          {:comment "s-imperfetto"})

                   (merge (unifyc hh21
                                  {:synsem {:infl :present
                                            :cat :verb}})
                          {:comment "vp-present"})

                   (merge (unifyc hh21
                                  {:synsem {:infl :futuro
                                            :cat :verb}})
                          {:comment "vp-future"})

                   (merge (unifyc hh21
                                  {:synsem {:infl :imperfetto
                                            :cat :verb}})
                          {:comment "vp-imperfetto"})


                   (merge (unifyc cc10
                                  {:synsem {:cat :noun}})
                          {:comment "noun phrase"})

                   (merge (unifyc hc11
                                  (let [head-synsem {:cat :noun}]
                                    {:synsem head-synsem
                                     :comp {:synsem {:mod head-synsem}}}))
                          {:comment "nbar"})

))

(def vp-future (first (filter (fn [x]
                                (= (:comment x) "parent2/hh21/future"))
                              parents)))
(def s-future (first (filter (fn [x]
                                (= (:comment x) "parent1/cc10/future"))
                              parents)))
(def np1 (first (filter (fn [x]
                         (= (:comment x) "parent3/cc10/np"))
                       parents)))

(def lex (seq (union (set (filter (fn [each]
                                    (= :adjective (get-in each '(:synsem :cat))))
                                  lexicon))
                     (set (filter (fn [each]
                                    (= :det (get-in each '(:synsem :cat))))
                                  lexicon))
                     (set (filter (fn [each]
                                    (= :noun (get-in each '(:synsem :cat))))
                                  lexicon))
                     (set (filter (fn [each]
                                    (= :verb (get-in each '(:synsem :cat))))
                                  lexicon)))))

(def depth0
  (filter (fn [phrase]
            (empty? (get-in phrase '(:synsem :subcat))))
          parents))

(def depth1
  (filter (fn [phrase]
            (and (not (empty? (get-in phrase '(:synsem :subcat))))
                 (empty? (get-in phrase '(:synsem :subcat :2)))))
          parents))

(def depth2
  (filter (fn [phrase]
            (and (not (empty? (get-in phrase '(:synsem :subcat))))
                 (not (empty? (get-in phrase '(:synsem :subcat :2))))
                 (empty? (get-in phrase '(:synsem :subcat :3)))))
          parents))

(defn lightning-bolt [ & [head lexicon phrases depth] ]
  (let [maxdepth 2
        depth (if depth depth 0)
        lexicon (if lexicon lexicon (shuffle lex))
        phrases (if phrases phrases (shuffle parents))
        head (if head head :top)]
    (forest/lightning-bolt head lexicon phrases depth)))

(defn keep-trying [ & [head ] ]
  (let [head (if head head :top)
        try (first (take 1 (lightning-bolt head)))]
    (cond (fail? try)
          (do
            (log/warn (str "lightning failed: " try "; retrying."))
            (keep-trying head))
          (not (empty? (get-in try '(:synsem :subcat))))
          (do
            (log/warn (str "lightning returned a partial phrase, continuing."))
            (keep-trying head))

          true
          try)))

(deftest sleeper-1 []
  (let [sleeper (get-in (first (lightning-bolt {:synsem {:sem {:pred :dormire}}})) '(:synsem :sem :subj))]
    (is (not (nil? sleeper)))))

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



