(ns italianverbs.test.lexicon
  (:use [clojure.test]
        [italianverbs.lexicon])
  (:require
   [italianverbs.fs :as fs]
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

(deftest noun-agreement-via-unify-1
  (let [np
    (let [cane (search/lookup "cane")
          determiner (search/random-lexeme (fs/unify {:cat :det}
                                                     (get-in cane '(:comp))))]
      (fs/unify cane {:comp determiner}))]
    (is (not (nil? np)))
    (is (not (= (get-in np '(:comp)) :fail)))
    (is (or (= (get-in np '(:comp :italian)) "il")
            (= (get-in np '(:comp :italian)) "un")))

    (is (= (type (get-in np '(:number))) clojure.lang.Ref))
    (is (or (= @(get-in np '(:number)) "singular")
            (= @(get-in np '(:number)) :singular)))

    (is (= (type (get-in np '(:gender))) clojure.lang.Ref))

    (is (or (= @(get-in np '(:gender)) "masc")
            (= @(get-in np '(:gender)) :masc)))

    (is (= (get-in np '(:gender))
           (get-in np '(:comp :gender))))
    (is (= (get-in np '(:number))
           (get-in np '(:comp :number))))))


(def mangiare (search/lookup "mangiare"))

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

(def verbs
  {:cat :verb})
