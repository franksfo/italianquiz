(ns italianverbs.rdutest
  (:require
   [clojure.contrib.string :as stringc]))

;; to include in your namespace (e.g. "user")
;; user=> (load "italianverbs/rdutest")
;; user=> (ns user (:use [italianverbs.rdutest]))
;; WARNING: test already refers to: #'clojure.core/test in namespace: italianverbs.rdutest, being replaced by: #'italianverbs.rdutest/test
;; WARNING: test already refers to: #'clojure.core/test in namespace: user, being replaced by: #'italianverbs.rdutest/test
;; nil

;; simple usage:
;; user=> (test (+ 1 3) (fn [result] (= result 4)))
;; {:test-text "(+ 1 3)", :assert-text "(fn [result] (= result 4))", :assert-result true, :test-result 4}
;;
;; running tests repeatedly:
;; 
;; user=> (rdutest
;;    "Just a simple example showing how to write rdutests."
;;    (+ 2 3)  ; expression to evaluate.
;;    #(= % 5) ; function to be applied to the evaluated expression.
;;    :simple-test-example)}) ; repeat of label (unfortunately)
;; user=> (rdu-simple-test-example)
;; 5
;; user=> (repeatedly 3 #(rdu-simple-test-example))
;; (5 5 5)
;; 

;; can't use just 'test' because I'm too stupid to understand Clojure namespaces.
(defmacro rdutest [testcomment test assert sym]
  "takes a test function and an assert function (should return boolean). test function will be evaluated and applied to the assert function."
  (let [test-text (str test)
        assert-text (str assert)
        sym-text (str sym)
        sym-text-no-colon (stringc/tail (- (.length sym-text) 1) sym-text)
        test-result `~test
        ;; TODO: this is ugly to be generating strings and then evaluating (with load-string below).
        ;; TODO: figure out how to defn within a namespace qualifier (e.g. "test/foo").
        defn (str "(defn rdu-" sym-text-no-colon " " [] " " test-text ")")
        testcomment (str testcomment)]
    `(let [assert# (apply ~assert (list ~test-result))]
       (println ~(str "Test: " sym-text " '" testcomment "' started."))
       (println (str "  Result: " assert# (if (= assert# false) " (FAILED).")))
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

;; TODO: remove test label.
(def rdutests
  {:simple-test-example ; test label
   (rdutest
    "Just a simple example showing how to write rdutests." ; comment
    (+ 2 3)  ; expression to evaluate.
    #(= % 5) ; function to be applied to the evaluated expression.
    :simple-test-example)}) ; repeat of label (unfortunately)


    