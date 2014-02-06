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

(def nbar-test hc11)
(deftest il-cane-nero
  (let [il-cane-nero (finalize (first (over np-test "il" (over nbar-test "cane" "nero"))))]
    (is (not (fail? il-cane-nero)))
    (is (= "il cane nero"
           (get-in il-cane-nero '(:italian))))
    (is (= "the black dog"
           (get-in il-cane-nero '(:english))))))

