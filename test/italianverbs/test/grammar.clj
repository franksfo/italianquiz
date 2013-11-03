(ns italianverbs.test.grammar
  (:refer-clojure :exclude [get-in resolve merge])
  (:require
   [clojure.core :exclude (get-in resolve merge)]
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [italianverbs.generate :refer :all]
   [italianverbs.grammar :refer :all]
   [italianverbs.lexicon :refer :all]
   [italianverbs.lexiconfn :refer :all]
   [italianverbs.morphology :refer :all]
   [italianverbs.rules :refer :all]
   [italianverbs.workbook :refer :all]
   [italianverbs.ug :refer :all]
   [italianverbs.unify :refer :all :exclude [unify]]))

(deftest io-dormo
  (let [result (sentence {:synsem {:sem {:pred :dormire
                                         :subj {:pred :io}}}})]
    (is (not (fail? result)))
    (is (= (get-in (finalize result) '(:italian)) "io dormo"))
    (is (= (get-in (finalize result) '(:english)) "I sleep"))))

(defn successful [result]
  (or (not (map? result))
      (not (fail? result)))
  (or (not (seq? result))
      (not (nil? (seq result)))))


(def vp-plus-adverb hh32)
(def fare-bene (over vp-plus-adverb "fare" "bene"))
(deftest fare-bene-test
  (is (successful fare-bene)))

(def prep-plus-verb-inf hh10)
(def a-vendere-la-casa (over prep-plus-verb-inf "a"
                             (over hh21
                                   "vendere"
                                   (over np "la" "casa"))))
(deftest a-vendere-la-casa-test
  (is (successful a-vendere-la-casa)))

(def fare-bene-a-vendere-la-casa
  (over vp fare-bene a-vendere-la-casa))
(deftest fare-bene-a-vendere-la-casa-test
  (is (successful fare-bene-a-vendere-la-casa)))

(def avere-fare-bene-a-vendere-la-casa
  ;; TODO: should not have to look for second or first member: (over) should handle it.
  (second (over hh21 "avere" fare-bene-a-vendere-la-casa)))

(deftest avere-fare-bene-a-vendere-la-casa-test
  (is (successful avere-fare-bene-a-vendere-la-casa)))

(def s-past cc10)

(def tu-hai-fatto-bene-a-vendere-la-casa
  ;; TODO: (over) can't take a sequence as the 2nd argument, so can't do this (commented-out):
                                        ;  (let [result (over s-past "tu" avere-fare-bene-a-vendere-la-casa)]
  ;; have to do this instead:
  (let [result (over s-past "tu" avere-fare-bene-a-vendere-la-casa)]

    ;; symptoms of the same problem: (inflexibility of input of (over))
    (if (seq? result)
      (first result)
      result)))

(deftest tu-hai-fatto-bene-a-vendere-la-casa-test
  (is (successful tu-hai-fatto-bene-a-vendere-la-casa))
  (let [english (get-in (finalize (copy tu-hai-fatto-bene-a-vendere-la-casa))
                              '(:english))]
    ;; TODO: figure out why extra space is being generated after "you".
    (is (or (= english "you  (&#x2642;) did well to sell the house")
            (= english "you  (&#x2640;) did well to sell the house"))))
  (is (= "tu hai fatto bene a vendere la casa"
         (get-in (finalize (copy tu-hai-fatto-bene-a-vendere-la-casa))
                       '(:italian)))))

(def s-past cc10)
(def s-present cc10)
(def adj-phrase hh21)

(deftest adj-agreement-with-subject
  "adjectives must agree with subjects - tests this behavior with intermediate 'meno ricco' between the subject and the adjective."
  (let [lei-e-piu-ricca-di-giorgio
        (over s-present "lei"
              (over vp "essere"
                    (over intensifier-phrase "più"
                          ;; error: (over adj-phrase (second (it "ricco")))
                          (over adj-phrase "ricco"
                                (over prep-phrase "di" "Giorgio")))))]
    (is (= (strip (get-italian (get-in (first lei-e-piu-ricca-di-giorgio) '(:italian))))
           "lei è più ricca di Giorgio"))))

(deftest fare-bene
  (let [result (first (take 1 (generate (unifyc s-past {:synsem {:sem {:pred :fare
                                                                      :mod {:pred :bene}}}}))))]
    (is (successful result))))

(deftest fare-bene-vendere-casa
  (let [result (first (take 1 (generate (unifyc s-past {:synsem {:sem {:pred :fare
                                                                      :obj {:pred :vendere
                                                                            :obj {:pred :casa}}}}}))))]
    (is (successful result))))

(deftest io-sono-venuto-per-dormire
  (is (successful (over s-past "io"
                         (over vp-aux "essere" (over vp-past "venire" (over prep-plus-verb-inf "per" "dormire")))))))


;; Optimize me:
;(fo (take 1 (gen21 (take 1 (gen21 (shuffle lexicon) (shuffle lexicon)))
;                              (shuffle lexicon)))))


(def np cc10)
;; tests moved from (generate) namespace to here.
(deftest il-libro
  (let [il-libro (finalize (first (over np "il" "libro")))]
    (is (not (fail? il-libro)))
    (is (= "il libro"
           (get-in il-libro '(:italian))))
    (is (= "the book"
           (get-in il-libro '(:english))))))

(deftest il-cane
  (let [il-cane (finalize (first (over np "il" "cane")))]
    (is (not (fail? il-cane)))
    (is (= "il cane"
           (get-in il-cane '(:italian))))
    (is (= "the dog"
           (get-in il-cane '(:english))))))

(deftest i-cani
  (let [i-cani (finalize (first (over np "i" "cane")))]
    (is (not (fail? i-cani)))
    (is (= "i cani"
           (get-in i-cani '(:italian))))
    (is (= "the dogs"
           (get-in i-cani '(:english))))))

(def nbar hc11)
(deftest il-cane-nero
  (let [il-cane-nero (finalize (first (over np "il" (over nbar "cane" "nero"))))]
    (is (not (fail? il-cane-nero)))
    (is (= "il cane nero"
           (get-in il-cane-nero '(:italian))))
    (is (= "the black dog"
           (get-in il-cane-nero '(:english))))))

(deftest i-cani-neri
  (let [i-cani-neri (finalize (first (over np "i" (over nbar "cane" "nero"))))]
    (is (not (fail? i-cani-neri)))
    (is (= "i cani neri"
           (get-in i-cani-neri '(:italian))))
    (is (= "the black dogs"
           (get-in i-cani-neri '(:english))))))

(deftest all-children-done-old-style-1
  (is (nil? (add-child-where (first (over nbar "studente" "brutto"))))))

(deftest all-children-done-old-style-2
  (is (nil? (add-child-where (first (over np "i" (over nbar "studente" "brutto")))))))

(deftest gli-studenti-brutti
  (is (= "gli studenti brutti"
         (get-in (finalize (first (over np "i" (over nbar "studente" "brutto"))))
                 '(:italian)))))

(deftest io-sogno
  (let [io-sogno (finalize (first (over s-present "io" "sognare")))]
    (is (= "io sogno"
           (get-in io-sogno '(:italian))))
    (is (= "I dream"
           (get-in io-sogno '(:english))))))

(def vp-pron ch21)
(deftest lei-ci-vede
  (let [lei-ci-vede (finalize (first (over s-present "lei" (over vp-pron "ci" "vedere"))))]
    (is (= "lei ci vede"
           (get-in lei-ci-vede '(:italian))))
    (is (= "she sees us"
           (get-in lei-ci-vede '(:english))))))

(def vp hh21)
(deftest io-parlo-la-parola
  (let [parlare-la-parola (first (over vp "parlare" (over np "la" "parola")))
        io-parlo-la-parola (first
                            (over s-present "io"
                                  (over vp "parlare" (over np "la" "parola"))))]

    (is (nil? (add-child-where parlare-la-parola)))

    (is (nil? (add-child-where io-parlo-la-parola)))

    (is (= "io parlo la parola"
           (get-in (finalize io-parlo-la-parola) '(:italian))))
    (is (= "I speak the word"
           (get-in (finalize io-parlo-la-parola) '(:english))))
  ))

(deftest loro-hanno-il-pane
  (let [loro-hanno-il-pane (first (over s-present "loro"
                                        (over vp "avere" (over np "il" "pane"))))
        hanno-il-pane (first (over vp "avere" (over np "il" "pane")))]
    (is (nil? (add-child-where hanno-il-pane)))
    (is (nil? (add-child-where loro-hanno-il-pane)))
    (is (= "loro hanno il pane"
           (get-in (finalize loro-hanno-il-pane) '(:italian))))
;    (is (= "they have the bread"
;           (get-in loro-hanno-il-pane '(:english)))
  ))

(deftest generate-nbar
  (let [nbar (take 1 (generate nbar))]
    (is (not (fail? nbar)))))

(deftest generate-np
  (let [np (take 1 (generate np))]
    (is (not (fail? np)))))

(def vp-present hh21)
(def s-present cc10)

(deftest generate-vp
  (let [vp (take 1 (generate vp-present))]
    (is (not (fail? vp)))))

(deftest generate-s-present
  (let [sentence (take 1 (generate s-present))]
    (is (not (fail? sentence)))))

(deftest add-child-where-1
  (let [cane (first (over nbar "cane"))
        cane-rosso (first (over nbar "cane" "rosso"))]
    (is (= (add-child-where cane) :comp))
    (is (nil? (add-child-where cane-rosso)))))
