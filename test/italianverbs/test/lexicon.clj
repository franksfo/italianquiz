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

(def verbs
  {:cat :verb})

;; A lexical entry for the word: 'parlare' exists.
(deftest parlare-test
  (let [parlare (search/lookup "parlare")]
    (is (= (:italian parlare) "parlare"))))

(deftest calcio-test
  (let [calcio (search/lookup "calcio")]
    (is (or (= :nil! (:comp calcio))
            (= "nil!" (:comp calcio))))))

;; Test that number and gender agreement of nouns, as implemented using references, works."
;; :number is shared by the paths (:number) and (:comp :number).
;;
;; [:cat :noun
;;  :number [1] :singular
;;  :comp [:cat :det
;;         :number [1] ] ]
(deftest cane-test
  (let [dog (search/lookup "cane")]
    (is
     (and
      ;; sanity checks: not related to reentrances.
      (not (nil? dog))
      ;; Ideally these subtests would work for the keyword,
      ;; since lexicon.clj uses keywords for symbols.
      ;; But for now, we have to test for "det" because of
      ;; database serialization.
      (or (= (get-in dog (list :cat))
             :noun)
          (= (get-in dog (list :cat))
             "noun"))
      (or (= (get-in dog (list :comp :cat))
             :det)
          (= (get-in dog (list :comp :cat))
             "det"))
      
      ;; test referential equality:
      (= (type (get-in dog '(:number))) clojure.lang.Ref)
      
      (= (get-in dog '(:number))
         (get-in dog '(:comp :number)))
      
      ;; as above with respect to keyword vs string.
      (or (= @(get-in dog '(:number)) :singular)
          (= @(get-in dog '(:number)) "singular"))))))

(deftest avere-test
  (let [to-have (search/lookup "avere")]
    ;; sanity checks: not related to reentrances.
    (is (not (nil? to-have)))
    ;; Ideally these subtests would work for the keyword,
    ;; since lexicon.clj uses keywords for symbols.
    ;; But for now, we have to test for "det" because of
    ;; database serialization.
    (is 
     (or (= (get-in to-have (list :cat))
            :verb)
         (= (get-in to-have (list :cat))
            "verb")))
    (is
     (or (= (get-in to-have (list :obj :cat))
            :noun)
         (= (get-in to-have (list :obj :cat))
            "noun")))

    ;; test referential equality:
    (is (= (type (get-in to-have '(:number))) clojure.lang.Ref))

    (is (= (get-in to-have '(:number))
           (get-in to-have '(:subj :number))))

    ;; subject and verb must agree in person..
    (is (= (get-in to-have '(:person))
           (get-in to-have '(:subj :person))))

    ;;.. and number.
    (is (= (get-in to-have '(:number))
           (get-in to-have '(:subj :number))))))

