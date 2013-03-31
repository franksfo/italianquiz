(ns italianverbs.test.generate
  (:use [clojure.test]
        [italianverbs.generate])
  (:require
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [clojure.set :as set]
   [somnium.congomongo :as mongo]
   ;; TODO: graduate italianverbs.fs to :use.
   [italianverbs.fs :as fs]
   [italianverbs.html :as html]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.grammar :as gram]
   [italianverbs.search :as search]))

(deftest il-libro
  (let [il-libro (over gram/np "il" "libro")]
    (is (not (fs/fail? il-libro)))
    (is (= "il libro"
           (fs/get-in il-libro '(:italian))))
    (is (= "the book"
           (fs/get-in il-libro '(:english))))))

(deftest io-sogno
  (let [io-sogno (over gram/s-present "io" "sognare")]
    (is (= "io sogno"
           (fs/get-in io-sogno '(:italian))))
    (is (= "i dream"
           (fs/get-in io-sogno '(:english))))))



