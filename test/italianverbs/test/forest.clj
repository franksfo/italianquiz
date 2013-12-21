(ns italianverbs.test.forest
  (:refer-clojure :exclude [get-in merge resolve find])
  (:use [clojure.test])
  (:require
   [italianverbs.generate :as generate]
   [clojure.tools.logging :as log]
   [italianverbs.forest :refer :all]
   [italianverbs.over :refer :all]
   [italianverbs.unify :refer :all]))

(deftest lightning1
  (let [bolt (filter (fn [x] (= "cc10" (get-in x '(:comment)))) (overh parents (overh parents lex)))]
    (is (not (empty? bolt)))))




