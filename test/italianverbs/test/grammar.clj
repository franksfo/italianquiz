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

(def s-past cc10)

(def s-past cc10)
(def s-present cc10)
(def adj-phrase-test hh21)

(deftest fare-bene
  (let [result (first (take 1 (generate (unifyc s-past {:synsem {:sem {:pred :fare
                                                                      :mod {:pred :bene}}}}))))]
    (is (successful result))))

(deftest fare-bene-vendere-casa
  (let [result (first (take 1 (generate (unifyc s-past {:synsem {:sem {:pred :fare
                                                                      :obj {:pred :vendere
                                                                            :obj {:pred :casa}}}}}))))]
    (is (successful result))))

(def np-test cc10)
;; tests moved from (generate) namespace to here.
(deftest il-libro
  (let [il-libro (finalize (first (over np-test "il" "libro")))]
    (is (not (fail? il-libro)))
    (is (= "il libro"
           (get-in il-libro '(:italian))))
    (is (= "the book"
           (get-in il-libro '(:english))))))

(deftest il-cane
  (let [il-cane (finalize (first (over np-test "il" "cane")))]
    (is (not (fail? il-cane)))
    (is (= "il cane"
           (get-in il-cane '(:italian))))
    (is (= "the dog"
           (get-in il-cane '(:english))))))

(deftest i-cani
  (let [i-cani (finalize (first (over np-test "i" "cane")))]
    (is (not (fail? i-cani)))
    (is (= "i cani"
           (get-in i-cani '(:italian))))
    (is (= "the dogs"
           (get-in i-cani '(:english))))))

(def nbar-test hc11)
(deftest il-cane-nero
  (let [il-cane-nero (finalize (first (over np-test "il" (over nbar-test "cane" "nero"))))]
    (is (not (fail? il-cane-nero)))
    (is (= "il cane nero"
           (get-in il-cane-nero '(:italian))))
    (is (= "the black dog"
           (get-in il-cane-nero '(:english))))))

(deftest i-cani-neri
  (let [i-cani-neri (finalize (first (over np-test "i" (over nbar "cane" "nero"))))]
    (is (not (fail? i-cani-neri)))
    (is (= "i cani neri"
           (get-in i-cani-neri '(:italian))))
    (is (= "the black dogs"
           (get-in i-cani-neri '(:english))))))

(deftest gli-studenti-brutti
  (is (= "gli studenti brutti"
         (get-in (finalize (first (over np-test "i" (over nbar "studente" "brutto"))))
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

(def vp-test hh21)
(deftest io-parlo-la-parola
  (let [parlare-la-parola (first (over vp-test "parlare" (over np-test "la" "parola")))
        io-parlo-la-parola (first
                            (over s-present "io"
                                  (over vp "parlare" (over np-test "la" "parola"))))]

    (is (= "io parlo la parola"
           (get-in (finalize io-parlo-la-parola) '(:italian))))
    (is (= "I speak the word"
           (get-in (finalize io-parlo-la-parola) '(:english))))
  ))

(deftest loro-hanno-il-pane
  (let [loro-hanno-il-pane (first (over s-present "loro"
                                        (over vp "avere" (over np-test "il" "pane"))))
        hanno-il-pane (first (over vp "avere" (over np-test "il" "pane")))]
    (is (= "loro hanno il pane"
           (get-in (finalize loro-hanno-il-pane) '(:italian))))
;    (is (= "they have the bread"
;           (get-in loro-hanno-il-pane '(:english)))
  ))

(deftest generate-nbar
  (let [nbar-test (take 1 (generate nbar))]
    (is (not (fail? nbar-test)))))

(deftest generate-np-test
  (let [np-test-generate (take 1 (generate np-test))]
    (is (not (fail? np-test-generate)))))

(def vp-present hh21)
(def s-present cc10)

(deftest generate-vp
  (let [vp (take 1 (generate vp-present))]
    (is (not (fail? vp)))))

(deftest generate-s-present
  (let [sentence (take 1 (generate s-present))]
    (is (not (fail? sentence)))))
