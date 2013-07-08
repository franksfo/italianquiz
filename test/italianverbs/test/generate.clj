(ns italianverbs.test.generate
  (:use [clojure.test]
        [italianverbs.unify]
        [italianverbs.generate])
  (:require
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [clojure.set :as set]
   [somnium.congomongo :as mongo]
   [italianverbs.html :as html]
   [italianverbs.unify :as unify]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.grammar :as gram]
   [italianverbs.morphology :as morph]
   [italianverbs.search :as search]))

(deftest il-libro
  (let [il-libro (finalize (first (over gram/np "il" "libro")))]
    (is (not (fail? il-libro)))
    (is (= "il libro"
           (get-in il-libro '(:italian))))
    (is (= "the book"
           (get-in il-libro '(:english))))))

(deftest il-cane
  (let [il-cane (finalize (first (over gram/np "il" "cane")))]
    (is (not (fail? il-cane)))
    (is (= "il cane"
           (get-in il-cane '(:italian))))
    (is (= "the dog"
           (get-in il-cane '(:english))))))

(deftest i-cani
  (let [i-cani (finalize (first (over gram/np "i" "cane")))]
    (is (not (fail? i-cani)))
    (is (= "i cani"
           (get-in i-cani '(:italian))))
    (is (= "the dogs"
           (get-in i-cani '(:english))))))

(deftest il-cane-nero
  (let [il-cane-nero (finalize (first (over gram/np "il" (over gram/nbar "cane" "nero"))))]
    (is (not (fail? il-cane-nero)))
    (is (= "il cane nero"
           (get-in il-cane-nero '(:italian))))
    (is (= "the black dog"
           (get-in il-cane-nero '(:english))))))

(deftest i-cani-neri
  (let [i-cani-neri (finalize (first (over gram/np "i" (over gram/nbar "cane" "nero"))))]
    (is (not (fail? i-cani-neri)))
    (is (= "i cani neri"
           (get-in i-cani-neri '(:italian))))
    (is (= "the black dogs"
           (get-in i-cani-neri '(:english))))))

(deftest all-children-done-old-style-1
  (is (nil? (add-child-where (first (over gram/nbar "studente" "brutto"))))))

(deftest all-children-done-old-style-2
  (is (nil? (add-child-where (first (over gram/np "i" (over gram/nbar "studente" "brutto")))))))

(deftest gli-studenti-brutti
  (is (= "gli studenti brutti"
         (get-in (finalize (first (over gram/np "i" (over gram/nbar "studente" "brutto"))))
                 '(:italian)))))

(deftest io-sogno
  (let [io-sogno (finalize (first (over gram/s-present "io" "sognare")))]
    (is (= "io sogno"
           (get-in io-sogno '(:italian))))
    (is (= "I dream"
           (get-in io-sogno '(:english))))))

(deftest lei-ci-vede
  (let [lei-ci-vede (finalize (first (over gram/s-present "lei" (over gram/vp-pron "ci" "vedere"))))]
    (is (= "lei ci vede"
           (get-in lei-ci-vede '(:italian))))
    (is (= "she sees us"
           (get-in lei-ci-vede '(:english))))))

(deftest io-parlo-la-parola
  (let [parlare-la-parola (first (over gram/vp "parlare" (over gram/np "la" "parola")))
        io-parlo-la-parola (first
                            (over gram/s-present "io"
                                  (over gram/vp "parlare" (over gram/np "la" "parola"))))]

    (is (nil? (add-child-where parlare-la-parola)))

    (is (nil? (add-child-where io-parlo-la-parola)))

    (is (= "io parlo la parola"
           (get-in (finalize io-parlo-la-parola) '(:italian))))
    (is (= "I speak the word"
           (get-in (finalize io-parlo-la-parola) '(:english))))
  ))

(deftest loro-hanno-il-pane
  (let [loro-hanno-il-pane (first (over gram/s-present "loro"
                                                  (over gram/vp "avere" (over gram/np "il" "pane"))))
        hanno-il-pane (first (over gram/vp "avere" (over gram/np "il" "pane")))]
    (is (nil? (add-child-where hanno-il-pane)))
    (is (nil? (add-child-where loro-hanno-il-pane)))
    (is (= "loro hanno il pane"
           (get-in (finalize loro-hanno-il-pane) '(:italian))))
;    (is (= "they have the bread"
;           (get-in loro-hanno-il-pane '(:english)))
  ))

(deftest generate-nbar
  (let [nbar (take 1 (generate gram/nbar))]
    (is (not (fail? nbar)))))

(deftest generate-np
  (let [np (take 1 (generate gram/np))]
    (is (not (fail? np)))))

(deftest generate-vp
  (let [vp (take 1 (generate gram/vp-present))]
    (is (not (fail? vp)))))

(deftest generate-s-present
  (let [sentence (take 1 (generate gram/s-present))]
    (is (not (fail? sentence)))))

(deftest add-child-where-1
  (let [cane (first (over gram/nbar "cane"))
        cane-rosso (first (over gram/nbar "cane" "rosso"))]
    (is (= (add-child-where cane) :comp))
    (is (nil? (add-child-where cane-rosso)))))


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

;; TODO: move this test to grammar.clj or lexicon.clj.
(deftest adj-agreement-with-subject
  "adjectives must agree with subjects - tests this behavior with intermediate 'meno ricco' between the subject and the adjective."
  (let [lei-e-piu-ricca-di-giorgio
        (over gram/s-present "lei"
              (over gram/vp "essere"
                    (over gram/intensifier-phrase "più"
                          (over gram/adj-phrase "ricco"
                                (over gram/prep-phrase "di" "Giorgio")))))]
    (is (= (morph/strip (morph/get-italian (get-in (first lei-e-piu-ricca-di-giorgio) '(:italian))))
           "lei è più ricca di Giorgio"))))
