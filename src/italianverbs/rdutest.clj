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
(defmacro rdutest [testcomment test assert]
  "takes a test function and an assert function (should return boolean). test function will be evaluated and applied to the assert function."
  (let [test-text (str test)
        assert-text (str assert)
        test-result `~test
        testcomment (str testcomment)]
    `(let [assert# (apply ~assert (list ~test-result))]
       (println ~(str "Test: '" testcomment "' started."))
       {:test-text ~test-text
        :assert-text ~assert-text
        :test-result ~test-result
        :assert-result assert#
        :comment ~testcomment}
       (println (str "  Result: " assert#))
       (println)
       )))


  


