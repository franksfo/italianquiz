(ns italianverbs.test.fs
  (:use [italianverbs.fs])
  (:use [clojure.test]))

(deftest simple-merge-test
  (let [result (merge {:foo 99} {:bar 42})]
    (is (= (:foo result) 99))
    (is (= (:bar result) 42))))

(deftest simple-unify-test
  (let [result (unify {:foo 99} {:bar 42})]
    (is (= (:foo result) 99))
    (is (= (:bar result) 42))))

(deftest recursive-merge-of-3-maps
  (let [result
        (let [map1 {:foo {:bar 99}}
              map2 {:foo {:baz 42}}
              map3 {:biff 12}]
          (merge map1 map2 map3))]
    ;; test that result looks like:
    ;; {:foo {:bar 99
    ;;        :baz 42}
    ;;  :biff 12}}
    (is (= (:bar (:foo result)) 99))
    (is (= (:baz (:foo result)) 42))
    (is (= (:biff result) 12))
    (is (= (get-in result '(:foo :bar)) 99))
    (is (= (get-in result '(:foo :baz)) 42))
    (is (= (get-in result '(:biff)) 12))))

(deftest unify-unequal-atomic-values
  "Testing that unify(v1,v2)=fail if v1 != v2."
  (let [result (unify {:foo 42} {:foo 43})]
    (is (= (:foo result) :fail))))

(deftest merge-unequal-atomic-values
  "Testing that merge(v1,v2)=v2 (overriding)."
  (let [result (merge {:foo 42} {:foo 43})]
    (is (= (:foo result) 43))))

(deftest ignore-nils-in-values
  "Ignore nils in values (true,nil)."
  (let [result (merge {:foo true} {:foo nil})]
    (is (= (:foo result) true))))

