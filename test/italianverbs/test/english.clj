(ns italianverbs.test.english
  (:refer-clojure :exclude [get-in lookup])
  (:require
   [clojure.core :as core]
   [clojure.test :refer :all]
   [italianverbs.english :refer :all]
   [italianverbs.engine :as engine]
   [italianverbs.lexicon.english :as lex]
   [italianverbs.lexiconfn :as lexiconfn :refer (compile-lex)]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.morphology.english :as morph]
   [italianverbs.over :refer :all]
   [italianverbs.parse :as parse]
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
                                         :obj {:buyable true}}}}
                   "sleep" {:synsem {:cat :verb
                                     :sem {:subj {:animate true}
                                           :discrete false
                                           :pred :dormire}}
                            :english {:past "slept"}}})

;; TODO: specific calls to compile-lex -> intransitivize -> transitivize
;; are repeated in src/italianverbs/english.clj as well as here.
(def compiled-1 (compile-lex test-lexicon
                             morph/exception-generator
                             morph/phonize
                             morph/english-specific-rules))

(def compiled (-> compiled-1
                  ;; make an intransitive version of every verb which has an
                  ;; [:sem :obj] path.
                  intransitivize
                  ;; make a transitive version
                  transitivize))

(def buy (get compiled "buy"))
(def sleep (get compiled "sleep"))

(deftest buy-test
  "There should be two entries for 'buy': one has both :subj and :obj; the other has only :subj. Make sure both are present and specified in the source lexicon."
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

(deftest test-roundtrip-english
  (let [retval (generate (engine/get-meaning (parse "she sleeps")))]
    (is (seq? retval))
    (is (> (.size retval) 0))
    (is (string? (fo (first retval))))
    (is (= "she sleeps" (fo (first retval))))))

(deftest generate-with-spec
  (let [retval (generate {:synsem {:sem {:tense :past, 
                                         :obj :unspec, 
                                         :aspect :perfect, 
                                         :pred :tornare, 
                                         :subj {:pred :loro}}}})]
    (is (not (empty? retval)))))


(deftest antonio-speaks
  (let [antonio-speaks (fo (engine/generate {:synsem {:infl :present :sem {:subj {:pred :antonio} :pred :speak}}} small :enrich true))]
    (is (= "Antonio speaks" antonio-speaks))))

(deftest antonia-plays
  (let [antonia-plays (fo (engine/generate {:synsem {:infl :present :sem {:subj {:pred :antonia} :pred :suonare}}} small :enrich true))]
    (is (= "Antonia plays" antonia-plays))))
