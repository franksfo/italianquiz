(ns italianverbs.test.forest
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [clojure.set :refer (union)]
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]

   [italianverbs.generate :as generate]
   [italianverbs.forest :exclude (lightning-bolt) ]
   [italianverbs.forest :as forest]
   [italianverbs.lexicon :refer (lexicon it en)]
   [italianverbs.lexiconfn :exclude (unify)]
   [italianverbs.morphology :refer (fo fo-ps)]
   [italianverbs.over :refer :all]
   [italianverbs.ug :refer :all]
   [italianverbs.unify :refer (fail? get-in merge unify unifyc remove-top-values)]))

(def grammar (list (merge (unifyc cc10
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
                                  {:synsem {:infl :infinitive
                                            :cat :verb}})
                          {:comment "vp-infinitive"})

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
                                  {:synsem {:cat :noun}
                                   :comp {:phrasal false}}) ;; rathole prevention
                          {:comment "noun phrase"})

                   (merge (unifyc hc11
                                  (let [head-synsem {:cat :noun}]
                                    {:synsem head-synsem
                                     :comp {:phrasal false ;; rathole prevention
                                            :synsem {:cat :adjective
                                                     :mod head-synsem}}}))
                          {:comment "nbar"})

))

(def minip (filter (fn [x]
                     (or
                      (= (:comment x) "vp-imperfetto")))
                   grammar))

(def vp-future (first (filter (fn [x]
                                (= (:comment x) "vp-future"))
                              grammar)))
(def vp-imperfetto (first (filter (fn [x]
                                    (= (:comment x) "vp-imperfetto"))
                                  grammar)))
(def s-future (first (filter (fn [x]
                                (= (:comment x) "s-future"))
                              grammar)))
(def np1 (first (filter (fn [x]
                         (= (:comment x) "np"))
                       grammar)))


(def nbar1 (first (filter (fn [x]
                            (= (:comment x) "nbar"))
                          grammar)))

(def minil (filter (fn [x]
                     (or
                      (= (get-in x '(:italian :italian)) "per")
                      (= (get-in x '(:italian :infinitive)) "volere")))
                   lexicon))

(def test-cache (forest/build-lex-sch-cache grammar lexicon))

(defn lightning-bolt [ & [head lex phrases depth cache] ]
  (let [maxdepth 2
        depth (if depth depth 0)
        lexicon (if lex lex lexicon)
        phrases (if phrases phrases (shuffle grammar))
        head (if head head :top)
        cache (if cache cache test-cache)]
    (forest/lightning-bolt head lexicon phrases depth "" cache)))

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



