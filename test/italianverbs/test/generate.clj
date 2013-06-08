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
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.grammar :as gram]
   [italianverbs.search :as search]))

(deftest il-libro
  (let [il-libro (first (over gram/np "il" "libro"))]
    (is (not (fail? il-libro)))
    (is (= "il libro"
           (get-in il-libro '(:italian))))
    (is (= "the book"
           (get-in il-libro '(:english))))))

(deftest io-sogno
  (let [io-sogno (first (over gram/s-present "io" "sognare"))]
    (is (= "io sogno"
           (get-in io-sogno '(:italian))))
    (is (= "i dream"
           (get-in io-sogno '(:english))))))

(deftest io-parlo-la-parola
  (let [io-parlo-la-parola (first
                            (over gram/s-present "io"
                                  (over gram/vp "parlare" (over gram/np "la" "parola"))))]
    (is (= "io parlo la parola"
           (get-in io-parlo-la-parola '(:italian))))
    (is (= "i speak the word"
           (get-in io-parlo-la-parola '(:english))))))


(deftest loro-hanno-il-pane
  (let [loro-hanno-il-pane (first (over gram/s-present "loro"
                                        (over gram/vp "avere" (over gram/np "il" "pane"))))]
    (is (= "loro hanno il pane"
           (get-in loro-hanno-il-pane '(:italian))))
    (is (= "they have the bread"
           (get-in loro-hanno-il-pane '(:english))))))


(deftest gli-studenti-brutti
  (is (= "gli studenti brutti"
         (get-in (first (over gram/np "i" (over gram/nbar "studente" "brutto")))
                    '(:italian)))))

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
