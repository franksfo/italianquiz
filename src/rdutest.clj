(ns rdutest
  (:require
   [clojure.contrib.string :as stringc]))

;; to include in your namespace (e.g. "user")
;; user=> (load "rdutest")
;; user=> (ns user (:use [rdutest]))

;; simplest passing test:
;;
;;  (rdutest "trivial pass" true (fn [result] (= result true)) :trivial-pass)
;;
;; simplest failing test:
;;
;;  (rdutest "trivial fail" true (fn [result] (= result false)) :trivial-fail)
;;
;
;; a bit less simple usage:
;; (rdutest "addition" (+ 1 3) (fn [result] (= result 4)) :addition)
;;
;; 
;; arguments:
;;  1. string to be printed for humans
;;  2. test to be run
;;  3. function to apply to result of 2.
;;  4. key (redundant with 1; needs to be removed.)

;; running tests repeatedly:
;; user=> (repeatedly 3 #(rdutest "trivial pass" true (fn [result] (= result true)) :trivial-pass))
;; 
;; TODO: figure out namespaces so I can just do: (defmacro test).
;; 
(defmacro rdutest [testcomment test assert sym & [supress-output]]
  "takes a test function and an assert function (should return boolean). test function will be evaluated and applied to the assert function."
  (let [test-text (str test)
        assert-text (str assert)
        sym-text (str sym)
        sym-text-no-colon (stringc/tail (- (.length sym-text) 1) sym-text)
        test-result `~test
        ;; TODO: this is ugly to be generating strings and then evaluating (with load-string below).
        ;; TODO: figure out how to defn within a namespace qualifier (e.g. "test/foo").
        defn (str "(defn rdu-" sym-text-no-colon " " [] " " test-text ")")
        supress-output `~supress-output
        testcomment (str testcomment)]
    `(let [assert# (apply ~assert (list ~test-result))]
       (if (not (= ~supress-output true))
         (println ~(str "Test: " sym-text " '" testcomment "' started.")))
       (if (not (= ~supress-output true))
         (println (str "  Result: " assert# (if (= assert# false) " (FAILED)."))))
       (load-string ~defn)
       {:test-text ~test-text
        :assert-text ~assert-text
        :test-result ~test-result
        :assert-result assert#
        :comment ~testcomment}
       )))

;; TODO: lookup tests in its native namespace so we don't need the 2nd arg.
;; Not using this as of now.
(defn wtf [test-name tests]
  "answers the question: 'wtf went wrong with that test??': (test-result of test with label test-name)"
  (:test-result (get tests test-name)))

(def rdutests
  (list
   (rdutest
    "Just a simple example showing how to write rdutests." ; comment
    (+ 2 3)  ; expression to evaluate.
    #(= % 5) ; function to be applied to the evaluated expression.
    :simple-test-example)
   (rdutest
    "Just a simple example showing how to write failing rdutests:" ; comment
    (let [failing-test
          (rdutest
           "failing test..."
           (+ 2 3)
           #(= % 4)
           :failing-test
           true)]
      (:assert-result failing-test))
    #(= % false) ;; test that the test failed: that the return value is false.
    :simple-failing-example)))
    