(ns italianverbs.test.english
  (:refer-clojure :exclude [get-in lookup])
  (:require
   [clojure.test :refer :all]
   [italianverbs.english :refer :all]
   [italianverbs.engine :as engine]
   [italianverbs.lexicon.english :as lex]
   [italianverbs.lexiconfn :refer :all]
   [italianverbs.morphology.english :as morph]
   [italianverbs.pos :as pos :refer [adjective animal
                                     cat-of-pronoun common-noun
                                     comparative
                                     countable-noun determiner
                                     drinkable-noun
                                     non-comparative-adjective noun
                                     pronoun-acc sentential-adverb
                                     verb verb-aux]]
    
   [italianverbs.pos.english :refer :all]
   [italianverbs.unify :as unify :refer [dissoc-paths get-in strip-refs]]))

(def test-lexicon {"buy" {:synsem {:cat :verb
                                   :sem {:pred :comprare
                                         :subj {:human true}
                                         :obj {:buyable true}}}}})

(def compiled-1 (compile-lex test-lexicon
                             morph/exception-generator
                             morph/phonize
                             morph/english-specific-rules))

(defn intransitivize [lexicon]
  (map-function-on-map-vals
   lexicon
   (fn [k vals]
     (cond (and (= (.size vals)
                   1)
                (not (nil? (get-in (first vals)
                                   [:synsem :sem :obj]
                                   nil))))
           (list (first vals)
                 (dissoc-paths (first vals)
                               (list [:serialized]
                                     [:synsem :sem :obj]
                                     [:synsem :subcat :2])))
           ;; else just return vals.
           true
           vals))))

(defn transitivize [lexicon]
  (map-function-on-map-vals
   lexicon
   (fn [k vals]
     (map (fn [val]
            (cond (and (= (get-in val [:synsem :cat])
                          :verb)
                       (not (nil? (get-in val [:synsem :sem :obj] nil))))
                  (unify/unifyc val
                                transitive)
                  
                  (= (get-in val [:synsem :cat]) :verb)
                  (unify/unifyc val
                                verb-subjective)
                  true
                  val))
          vals))))

(def compiled (-> compiled-1
                  ;; make an intransitive version of every verb which has an
                  ;; [:sem :obj] path.
                  intransitivize
                  ;; make a transitive version
                  transitivize))

(def buy (get compiled "buy"))

(deftest two-entries
  "There should be two entries for 'buy': one has both :subj and :obj; the other has only :subj. Make sure both are present and specified per the source lexicon."
  (is (= (.size buy) 2))

  ;; both transitive and intransitive: check subject spec
  (is (= {:human true} (get-in (nth buy 0) [:synsem :sem :subj])))
  (is (= {:human true} (get-in (nth buy 1) [:synsem :sem :subj])))

  ;; transitive sense: check object spec.
  (is (or (= {:buyable true} (get-in (nth buy 0) [:synsem :sem :obj]))
          (= {:buyable true} (get-in (nth buy 1) [:synsem :sem :obj]))))

  ;; intransitive sense: check subject spec.
  (is (or (= :none (get-in (nth buy 0) [:synsem :sem :obj] :none))
          (= :none (get-in (nth buy 1) [:synsem :sem :obj] :none)))))


