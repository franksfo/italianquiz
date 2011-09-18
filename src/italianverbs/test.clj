 (ns italianverbs.test
    (:use 
     [hiccup core page-helpers]
     [somnium.congomongo])
    (:require
     [italianverbs.generate :as gen]
     [italianverbs.grammar :as gram]
     [italianverbs.lev :as lev]
     [italianverbs.html :as html]
     [italianverbs.quiz :as quiz]
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
(def alltests
  {:html html/test
   :grammar gram/test
   :generate gen/test
   :lev lev/test
   :quiz quiz/test
   })

;; these tests run at load-time:
(def tests
  (map run-test alltests))

;; these tests run at each invocation of (test/run-tests):
(defn run-tests []
  (mongo! :db "mydb")
  (clojure.string/join ""
                       (flatten
                        (list
                         (str "<div class='legend'>"
                              (clojure.string/join ""
                                                   (map (fn [package]
                                                          (str
                                                           "<h1><a href='#" (get package :name) "'>" (get package :name) "</a></h1>"
                                                           ))
                                                        (map
                                                         (fn [package]
                                                           {:name (first package)})
                                                         alltests)))
                              "</div>")
                         (map (fn [package]
                                (str
                                 "<div class='package'>"
                                 "<h1><a name='" (get package :package) "'>" (get package :package) "</a></h1>"
                                 (html/tablize (get package :tests))
                                 "</div>"))
                              (map
                               (fn [package]
                                 {:package (first package)
                                  :tests
                                  (apply (second package) [])})
                               alltests))))))
  