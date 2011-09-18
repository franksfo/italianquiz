 (ns italianverbs.test
    (:use 
     [hiccup core page-helpers]
     [italianverbs.generate]
     [somnium.congomongo])
    (:require
     [italianverbs.generate :as gen]
     [italianverbs.lev :as lev]
     [italianverbs.html :as html]
     ))

(defn run-test [test-fn]
  "run a single test and wrap an HTML string in a list.
   acts according to the type of test-fn which can be:
    -list (evaluate each recursively)
    -function (apply with no args)
    -(TODO): map of function => arg"
  (cond
   (= (type test-fn) clojure.lang.LazySeq)
   "lazyseq"
   (= (type test-fn) clojure.lang.PersistentList)
   "persistent-list"
   true
   (html/tablize
    (apply test-fn []))))

; list of all packages to test (for now you must suffixize with "/test")
;(def alltests (list html/test quiz/test gen/test lev/test))
                                        ;(def alltests (list html/test gen/test))
                                        ;(def alltests (list html/test))
(def alltests (list html/test gen/test))

;; these tests run at load-time:
(def tests
  (map run-test alltests))

;; these tests run at each invocation of (test/run-tests):
(defn run-tests []
  (clojure.string/join ""
                       (map run-test alltests)))

