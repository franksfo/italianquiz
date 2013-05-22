(ns italianverbs.test.lexicon
  (:use [clojure.test]
        [italianverbs.lexicon])
  (:require
   [italianverbs.unify :as fs]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.search :as search]))

(def third-person {:person :3rd :cat :noun})
(def common-noun (fs/merge third-person
                           (let [number-agreement (ref :top)
                                 gender-agreement (ref :top)]
                             {:comp {:cat :det
                                     ;; determiner must agree with number of noun:
                                     :number number-agreement
                                     :gender gender-agreement
                                     }
                              :morph "morph-noun"
                              :common true
                              :gender gender-agreement
                              :number number-agreement})))

(def artifact (fs/merge common-noun {:artifact true}))
(def masc {:gender :masc})
(def letto-fs (apply fs/merge
                     ;; copy here to prevent any structure sharing between new lexical entry on the one hand, and input featuremaps on the other.
                     (concat (map #'fs/copy (list artifact masc))
                             (list {:english "bed"}
                                   {:italian "letto"}))))

(def mangiare-search (search/lookup "mangiare"))

;; looking for transitive verbs (:obj|:cat = noun)
;; which happen in a place (:adjunct|:obj|:place = true).
;; result should include mangiare.
(def place-verbs
  {:cat :verb
   :obj {:cat :noun}
   :adjunct {:cat :prep
             :obj {:place true}}})

;;usage : (query (pathify place-verbs)))

;; transitive verbs only
;; result should include mangiare.
(def trans-verbs
  {:cat :verb
   :obj {:cat :noun}})

(def test-verbs
  {:cat :verb})

(deftest avere-test
  (let [to-have (search/lookup "avere")]
    ;; sanity checks: not related to reentrances.
    ;; Ideally these subtests would work for the keyword,
    ;; since lexicon.clj uses keywords for symbols.
    ;; But for now, we have to test for "det" because of
    ;; database serialization.

    (is (= (get-in to-have '(:number))
           (get-in to-have '(:subj :number))))


    ;;.. and number.
    (is (= (get-in to-have '(:number))
           (get-in to-have '(:subj :number))))))

