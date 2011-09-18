 (ns italianverbs.test
    (:use 
     [hiccup core page-helpers]
     [somnium.congomongo])
    (:require
     [italianverbs.generate :as gen]
     [italianverbs.grammar :as gram]
     [italianverbs.lev :as lev]
     [italianverbs.html :as html]
     ))

(defn run-test [test-fn-label-and-test]
  "run a single test and wrap an HTML string in a list.
   acts according to the type of test-fn which can be:
    -list (evaluate each recursively)
    -function (apply with no args)
    -(TODO): map of function => arg"
  (let [label (first test-fn-label-and-test)
        test-fn (second test-fn-label-and-test)]
    (str
     "<div><h1>" label "</h1>"
     (cond
      (= (type test-fn) clojure.lang.LazySeq)
      (clojure.string/join ""
                           (map run-test test-fn))
      (= (type test-fn) clojure.lang.PersistentList)
      (clojure.string/join ""
                           (map run-test test-fn))
      true
      (html/tablize
       (apply test-fn [])))
     "</div>")))

; list of all packages to test (for now you must suffixize with "/test")
;(def alltests (list html/test quiz/test gen/test lev/test))
                                        ;(def alltests (list html/test gen/test))
                                        ;(def alltests (list html/test))
(def alltests
  {:html html/test
   :generate gen/test
   :grammar gram/test})

;; these tests run at load-time:
(def tests
  (map run-test alltests))

;; these tests run at each invocation of (test/run-tests):
(defn run-tests []
  (mongo! :db "mydb")
  (clojure.string/join ""
                       (map run-test alltests)))

