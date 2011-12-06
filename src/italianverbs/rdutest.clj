(ns italianverbs.rdutest)

;; usage:
;; user=> (load "italianverbs/rdutest")
;; user=> (ns user (:use [italianverbs.rdutest]))
;; WARNING: test already refers to: #'clojure.core/test in namespace: italianverbs.rdutest, being replaced by: #'italianverbs.rdutest/test
;; WARNING: test already refers to: #'clojure.core/test in namespace: user, being replaced by: #'italianverbs.rdutest/test
;; nil
;; user=> (test (+ 1 3) (fn [result] (= result 4)))
;; {:test-text "(+ 1 3)", :assert-text "(fn [result] (= result 4))", :assert-result true, :test-result 4}
;;

;; can't use just 'test' because I'm too stupid to understand Clojure namespaces.
(defmacro rdutest [testcomment test assert sym]
  "takes a test function and an assert function (should return boolean). test function will be evaluated and applied to the assert function."
  (let [test-text (str test)
        assert-text (str assert)
        sym-text (str sym)
        test-result `~test
        testcomment (str testcomment)]
    `(let [assert# (apply ~assert (list ~test-result))]
       (println ~(str "Test: " sym-text " '" testcomment "' started."))
       (println (str "  Result: " assert# (if (= assert# false) " (FAILED).")))
       {:test-text ~test-text
        :assert-text ~assert-text
        :test-result ~test-result
        :assert-result assert#
        :comment ~testcomment}
       )))

;; TODO: lookup tests in its native namespace so we don't need the 2nd arg.
(defn wtf [test-name tests]
  "answers the question: 'wtf went wrong with that test??': (test-result of test with label test-name)"
  (:test-result (get tests test-name)))


(def rdutests
  {:simple-test-example ; test label
   (rdutest
    "Just a simple example showing how to write rdutests." ; comment
    (+ 2 3)  ; expression to evaluate.
    #(= % 5) ; function to be applied to the evaluated expression.
    :simple-test-example)}) ; repeat of label (unfortunately)


    