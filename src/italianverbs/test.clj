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

(defn run-test [test-fn]
  "run a single test and wrap an HTML string in a list.
   acts according to the type of test-fn which can be:
    -list (evaluate each recursively)
    -function (apply with no args)
    -(TODO): map of function => arg"
  (cond
   (= (type test-fn) clojure.lang.LazySeq)
   (clojure.string/join ""
                        (map run-test test-fn))
   (= (type test-fn) clojure.lang.PersistentList)
   (clojure.string/join ""
                        (map run-test test-fn))
   true
   (html/tablize
    (apply test-fn []))))

(defn run-tests-in-package [package-tests-map]
  "run all tests in package."
  (clojure.string/join ""
                       (map (fn [package-tests]
                              (let [package (first package-tests)
                                    tests (second package-tests)]
                                (str "<div class='package'><h1>" package "</h1>"
                                     (map (fn [test]
                                            (run-test test))
                                          tests)
                                     "</div>")))
                            package-tests-map)))
           


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


(def improved-alltests
  (map
   (fn [package]
     {:package (first package)
      :tests
      (apply (second package) [])})
   alltests))

;; these tests run at each invocation of (test/run-tests):
(defn run-tests []
  (mongo! :db "mydb")
  (clojure.string/join ""
                       (map (fn [package]
                              (str
                               "<div class='package'>"
                               "<h1>" (get package :package) "</h1>"
                               (html/tablize (get package :tests))
                               "</div>"))
                            (map
                             (fn [package]
                               {:package (first package)
                                :tests
                                (apply (second package) [])})
                             alltests))))
