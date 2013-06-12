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

(deftest il-cane
  (let [il-cane (first (over gram/np-new "il" "cane"))]
    (is (not (fail? il-cane)))
    (is (= "il cane"
           (get-in il-cane '(:italian))))
    (is (= "the dog"
           (get-in il-cane '(:english))))))

(deftest i-cani
  (let [i-cani (first (over gram/np-new "i" "cane"))]
    (is (not (fail? i-cani)))
    (is (= "i cani"
           (get-in i-cani '(:italian))))
    (is (= "the dogs"
           (get-in i-cani '(:english))))))

(deftest il-cane-nero
  (let [il-cane-nero (first (over gram/np-new "il" (over gram/nbar-new "cane" "nero")))]
    (is (not (fail? il-cane-nero)))
    (is (= "il cane nero"
           (get-in il-cane-nero '(:italian))))
    (is (= "the black dog"
           (get-in il-cane-nero '(:english))))))

(deftest i-cani-neri
  (let [i-cani-neri (first (over gram/np-new "i" (over gram/nbar-new "cane" "nero")))]
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
         (get-in (first (over gram/np "i" (over gram/nbar "studente" "brutto")))
                 '(:italian)))))

(deftest io-sogno
  (let [io-sogno (first (over gram/s-present "io" "sognare"))]
    (is (= "io sogno"
           (get-in io-sogno '(:italian))))
    (is (= "i dream"
           (get-in io-sogno '(:english))))))

(deftest io-parlo-la-parola
  (let [parlare-la-parola (first (over gram/vp "parlare" (over gram/np "la" "parola")))
        io-parlo-la-parola (first
                            (over gram/s-present "io"
                                  (over gram/vp "parlare" (over gram/np "la" "parola"))))]

    (is (nil? (add-child-where parlare-la-parola)))

    (is (nil? (add-child-where io-parlo-la-parola)))

    (is (= "io parlo la parola"
           (get-in io-parlo-la-parola '(:italian))))
    (is (= "i speak the word"
           (get-in io-parlo-la-parola '(:english))))
  ))

(deftest loro-hanno-il-pane
  (let [loro-hanno-il-pane (first (over gram/s-present "loro"
                                        (over gram/vp "avere" (over gram/np "il" "pane"))))
        hanno-il-pane (first (over gram/vp "avere" (over gram/np "il" "pane")))]
    (is (nil? (add-child-where hanno-il-pane)))
    (is (nil? (add-child-where loro-hanno-il-pane)))
    (is (= "loro hanno il pane"
           (get-in loro-hanno-il-pane '(:italian))))
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
  (let [cane (first (over gram/nbar-new "cane"))
        cane-rosso (first (over gram/nbar-new "cane" "rosso"))]
    (is (= (add-child-where cane) :comp))
    (is (nil? (add-child-where cane-rosso)))))

