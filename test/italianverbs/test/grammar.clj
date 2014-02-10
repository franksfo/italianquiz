(ns italianverbs.test.grammar
  (:refer-clojure :exclude [get-in resolve merge])
  (:require
   [clojure.core :exclude (get-in resolve merge)]
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [italianverbs.generate :refer :all]
   [italianverbs.grammar :refer :all]
   [italianverbs.lexicon :refer (it)]
   [italianverbs.lexiconfn :refer :all]
   [italianverbs.morphology :refer :all]
   [italianverbs.over :refer :all]
   [italianverbs.ug :refer :all]
   [italianverbs.unify :refer :all :exclude [unify]]))

(defn successful [result]
  (or (not (map? result))
      (not (fail? result)))
  (or (not (seq? result))
      (not (nil? (seq result)))))

;; TODO: use grammar-defined phrases, not these test-internal ones.
(def vp-plus-adverb hh32)
;; TODO: "fare" or "bene" or both are not working: commented out next two expressions until fixed.
(def fare-bene (over vp-plus-adverb "fare" "bene"))
(deftest fare-bene-test
  (is (successful fare-bene)))

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

(def nbar-test hc11-comp-subcat-1)
(deftest il-cane-nero
  (let [il-cane-nero (finalize (first (over np-test "il" (over nbar-test "cane" "nero"))))]
    (is (not (fail? il-cane-nero)))
    (is (= "il cane nero"
           (get-in il-cane-nero '(:italian))))
    (is (= "the black dog"
           (get-in il-cane-nero '(:english))))))


(def agreement-test-1
  (let [result (first (over s-aux (over noun-phrase "la" "melanzana") (over vp-aux "essere" (over vp-past "essere" "nero"))))]
    (is (= (get-italian-1 (get-in result '(:italian)))
           "la melanzana è stata nera"))))

(def agreement-test-2
  (let [result (first (over s-aux (over noun-phrase "le" "melanzana") (over vp-aux "essere" (over vp-past "essere" "nero"))))]
    (is (= (get-italian-1 (get-in result '(:italian)))
           "le melanzane sono state nere"))))

(def agreement-test-3
  (let [result (first (over s-present (over noun-phrase "le mie" "pizza") (over vp-present "essere" (over intensifier-phrase "meno" "bello"))))]
    (is (= (get-italian-1 (get-in result '(:italian)))
           "le mie pizze sono meno belle"))))

(def aux-with-agreement-1
  (let [result (first (over s-aux "io" (over vp-pronoun-aux "lo" (over vp-aux-22 "avere" "ricordare"))))]
    (is (= (get-italian-1 (get-in result '(:italian)))
           "io l'ho ricordato"))))

(def aux-with-agreement-2
  (let [result (first (over s-aux "io" (over vp-pronoun-aux "la" (over vp-aux-22 "avere" "ricordare"))))]
    (is (= (or true (get-italian-1 (get-in result '(:italian))))
           "io l'ho ricordata"))))


