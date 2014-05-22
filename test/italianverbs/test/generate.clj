;; TODO: move tests that use gram/ and lexfn/ to their own namespaces, since
;; generate/ should not depend on those - the grammar and lexicon are independent of
;; generation.
(ns italianverbs.test.generate
  (:use [clojure.test])
  (:require
   [clojure.set :refer (union)]
   [clojure.tools.logging :as log]
   [italianverbs.forest :as forest]
   [italianverbs.generate :refer :all]
   [italianverbs.grammar :refer :all]
   [italianverbs.lexicon :refer (lexicon it en)]
   [italianverbs.morphology :refer (fo fo-ps)]
   [italianverbs.unify :refer (fail? get-in lazy-shuffle merge remove-top-values unify unifyc)]
   [italianverbs.unify :as unify]
   [italianverbs.lexiconfn :as lexfn]))

(def cache rule-cache)

(deftest stack-overflow-error
    "merge has a problem: we hit StackOverflowError java.util.regex.Pattern$BmpCharProperty.match (Pattern.java:3366) when this test is run.
   Code works as expected if merge is replaced with unify. However, currently this test passes for some reason."
    (lexfn/unify
     (unify/get-in (unify/merge (let [head-cat (ref :top)
                                      head-is-pronoun (ref :top)
                                      head-sem (ref :top)
                                      head-infl (ref :top)]
                                  {:synsem {:cat head-cat
                                            :pronoun head-is-pronoun
                                            :sem head-sem
                                            :infl head-infl}
                                   :head {:synsem {:cat head-cat
                                                   :pronoun head-is-pronoun
                                                   :infl head-infl
                                                   :sem head-sem}}})
                                (let [essere (ref :top)
                                      infl (ref :top)
                                      cat (ref :verb)]
                                  {:italian {:a {:infl infl
                                                 :cat cat}}
                                   :english {:a {:infl infl
                                                 :cat cat}}
                                   :synsem {:infl infl
                                            :essere essere}
                                   :head {:italian {:infl infl
                                                    :cat cat}
                                          :english {:infl infl
                                                    :cat cat}
                                          :synsem {:cat cat
                                                   :essere essere
                                                   :infl infl}}}))
                   '(:head))
     (lexfn/unify
      {:italian {:foo 42}}
      (let [infl (ref :top)]
        {:italian {:infl infl}
         :english {:infl infl}
         :synsem {:infl infl}}))))

(deftest il-libro
  (is (= "Il libro (The book)."
         (first (fo (first (take 1 (lightning-bolt {:synsem {:cat :noun :subcat '()}}
                                                   (list noun-phrase nbar s-future)
                                                   (seq (union (it "il") (it "libro")))))))))))

(deftest Antonio-impazze
  (is (= "Antonio impazze (Antonio goes mad)")
      (first (fo (first (take 1 (forest/hlcl cache (list s-present)
                                             {:synsem {:sem {:subj {:pred :antonio}
                                                             :pred :impazzire}}})))))))


(deftest dog-goes-mad
  (is (= "The dog will go mad")
      (first (fo (first (take 1 (forest/hlcp cache (list s-future (unifyc np {:comp {:synsem {:def true}}}))
                                             {:synsem {:sem {:subj {:pred :cane}
                                                             :pred :impazzire}}})))))))

      
