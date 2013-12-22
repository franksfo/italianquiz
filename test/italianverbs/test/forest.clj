(ns italianverbs.test.forest
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:use [clojure.test])
  (:require
   [italianverbs.generate :as generate]
   [clojure.tools.logging :as log]
   [italianverbs.forest :refer :all]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.over :refer :all]
   [italianverbs.unify :refer :all]))

(deftest lightning1
  (let [bolts (filter (fn [x] (= "cc10" (get-in x '(:comment)))) (overh parents (overh parents lex)))]
    (is (not (empty? bolts)))))


(deftest lightning2
  (let [bolt (take 1 (lightningb :top parents))]
    (is (not (nil? bolt)))))

(deftest lightning3
  (= 6 (.size (lightningb :top parents))))



