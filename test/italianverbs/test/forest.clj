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
   [italianverbs.ug :refer :all :exclude (np)]
   [italianverbs.unify :refer (fail? get-in lazy-shuffle merge remove-top-values unify unifyc)]))

;; 1 (of 3): define functions.
;; TODO: move all functions from here to italianverbs.forest

(declare lightning-bolt)
(declare test-cache)

(defn gen-sentence [ & head ]
  (let [head (if head head :top)]
    (log/debug (str "gen-sentence start with head: " head))
    (let [result
          (take 1 (lightning-bolt (unifyc head
                                          {:synsem {:cat :verb
                                                    :subcat '()}})))]
      (log/debug (str "gen-sentence done with head: " head ": " (fo result)))
      result)))

(defn gen-np [ & head ]
  (let [head (if head head :top)]
    (log/debug (str "gen-np start with head: " head))
    (let [result
          (take 1 (lightning-bolt (unifyc head
                                          {:synsem {:cat :noun
                                                    :subcat '()}})))]
      (log/debug (str "gen-sentence done with head: " head ": " (fo result)))
      result)))


(defn gen-nbar [ & head ]
  (let [head (if head head :top)]
    (log/debug (str "gen-np start with head: " head))
    (let [result
          (take 1 (lightning-bolt (unifyc head
                                          {:synsem {:cat :noun
                                                    :subcat {:1 :top
                                                             :2 '()}}})))]
      (log/debug (str "gen-sentence done with head: " head ": " (fo result)))
      result)))

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
  (let [i-sleep (first (lightning-bolt {:synsem {:subcat '()
                                                 :sem {:subj {:pred :io}
                                                       :pred :dormire}}}))]
    (is (or (= (list "Io dormo (I sleep).") (fo i-sleep))
            (= (list "Io dormirò (I will sleep).") (fo i-sleep))
            (= (list "Io dormivo (I was sleeping).") (fo i-sleep))))))


(deftest human-sleeper-1 []
  (let [human-sleeper (get-in (first (lightning-bolt {:synsem {:subcat '()
                                                               :sem {:subj {:human true}
                                                                     :pred :dormire}}}))
                               '(:synsem :sem :subj))]
    (is (= (get-in human-sleeper '(:human)) true))
    (is (= (get-in human-sleeper '(:animate)) true))

(deftest animal-sleeper-1 []
  (let [animal-sleeper (get-in (first (lightning-bolt {:synsem {:subcat '()
                                                                :sem {:subj {:human false}
                                                                      :pred :dormire}}}))
                               '(:synsem :sem :subj))]
    (is (= (get-in animal-sleeper '(:human)) false))))

(deftest edible-1
  (let [edible (get-in (first (lightning-bolt {:synsem {:subcat '()
                                                        :sem {:pred :mangiare}}})) '(:synsem :sem))]
    (is (not (nil? edible)))))))

(deftest animal-eater-1 []
  (let [eating-semantics (get-in (first (lightning-bolt {:synsem {:subcat '()
                                                                  :sem {:subj {:human false}
                                                                        :pred :mangiare}}}))
                                 '(:synsem :sem))]
    (is (= (get-in eating-semantics '(:subj :human)) false))))

(defn get-cached []
  (fo (:head (get test-cache "vp-infinitive"))))

;; 2 grammar and lexicon

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

;; minip and minil are for testing (move 'real' grammar out of test and into grammar).
(def minip (filter (fn [x]
                     (or
                      (= (:comment x) "vp-imperfetto")))
                   grammar))

;; TODO: calling (.size) because (map) is lazy, and I want to realize
;; the sequence - must be a better way to loop over the grammar.
(.size (map (fn [rule]
       (intern *ns* (symbol (:comment rule)) rule))
     grammar))

(def minil (filter (fn [x]
                     (or
                      (= (get-in x '(:italian :italian)) "per")
                      (= (get-in x '(:italian :infinitive)) "volere")))
                   lexicon))


;; 3 grammar runtime
(def test-cache (forest/build-lex-sch-cache grammar lexicon))

(defn lightning-bolt [ & [head lex phrases depth cache] ]
  (let [maxdepth 2
        depth (if depth depth 0)
        lexicon (if lex lex lexicon)
        phrases (if phrases phrases (shuffle grammar))
        head (if head head :top)
        cache (if cache cache test-cache)]
    (forest/lightning-bolt head lexicon phrases depth "" cache)))
