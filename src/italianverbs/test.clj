 (ns italianverbs.test
    (:use 
     [hiccup core page-helpers]
     [somnium.congomongo])
    (:require
     [italianverbs.generate :as gen]
     [italianverbs.grammar :as gram]
     [italianverbs.lev :as lev]
     [italianverbs.morphology :as morph]
     [italianverbs.html :as html]
     [italianverbs.xml :as xml]
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
; shown in order from more basic to more complex, but can be in any order.
(def alltests
  {:xml xml/test
   :html html/test
   :morph morph/test
   :grammar gram/test
   :generate gen/test
   :lev lev/test
   :quiz quiz/test
   })

;; these tests run at load-time:
(def tests
  (map run-test alltests))

;; these tests run at each invocation of (test/run-tests) (which is run by core.clj when the "/test/" URL is GETted.)
(defn run-tests []
  (mongo! :db "mydb")
  (let [test-results
        (map
         (fn [package]
           {:package (first package)
            :tests
            (apply (second package) [])})
         alltests)]
    (clojure.string/join ""
                         (flatten
                          (list
                           (str "<div class='legend'><h2>Contents</h2>"
                                (clojure.string/join
                                 ""
                                 (map (fn [package]
                                        (let [package-name (get package :name)]
                                          (str
                                           "<div class='package-toc'><h1><a href='#" package-name "'>" package-name "</a></h1>"
                                           (clojure.string/join ""
                                                                (map (fn [test-result]
                                                                       (str
                                                                        (if (= (get test-result :package) package-name)
                                                                          (clojure.string/join ""
                                                                                               (map (fn [test]
                                                                                                      (let [anchor (html/create-anchor
                                                                                                                    package-name
                                                                                                                    (if (get test :anchor)
                                                                                                                      (get test :anchor)
                                                                                                                      (get test :comment)))]
                                                                                                        (str "<h3><a href='#"
                                                                                                             ;; use colon(:) as a separator between
                                                                                                             ;; package and test.
                                                                                                             anchor
                                                                                                             "'>"
                                                                                                             (get test :comment)
                                                                                                             "</a></h3>")))
                                                                                                    (get test-result :tests))))))
                                                                     test-results))
                                           
                                           "</div>"
                                           )))
                                      (map
                                       (fn [package]
                                         {:name (first package)})
                                       alltests)))
                                "</div>")
                           (map (fn [package]
                                  (let [package-name (get package :package)]
                                    (str
                                     "<div class='package'>"
                                     "<h1><a name='" (get package :package) "'>" (get package :package) "</a></h1>"
                                     (clojure.string/join ""
                                                          (map (fn [test]
                                                                 (let [anchor (html/create-anchor
                                                                               package-name
                                                                               (if (get test :anchor)
                                                                                 (get test :anchor)
                                                                                 (get test :comment)))]
                                                                   (str "<div class='test'>"
                                                                        (html/tablize test)
                                                                        "<a name='" anchor "'> </a>"
                                                                        "</div>")))
                                                               (get package :tests)))
                                     "</div>")))
                                test-results))))))
