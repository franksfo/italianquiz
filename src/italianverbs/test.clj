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

(defn tablize [arg]
  (cond
   (= (type arg) clojure.lang.LazySeq)
   (str
    (clojure.string/join ""
                         (map tablize arg)))
   (= (type arg) clojure.lang.PersistentList)
   (str
    (clojure.string/join ""
                         (map tablize arg)))
   (= (type arg) clojure.lang.PersistentArrayMap)
   (str
    "<table class='map'>"
    (clojure.string/join ""
                         (map (fn [tr]
                                (str "<tr><th>"
                                     (str (first tr))
                                     "</th>"
                                     "<td>"
                                     (tablize (second tr))
                                     "</td></tr>"))
                              arg))
    "</table>")
   true
   (str "<div class='atom'>" arg "</div>")))

(defn run-test [test-fn]
  "run a single test and wrap an HTML string in a list.
   acts according to the type of test-fn which can be:
    -list (evaluate each recursively)
    -function (apply with no args)
    -(TODO): map of function => arg"
  (tablize
   (cond
    (= (type test-fn) clojure.lang.LazySeq)
    "lazyseq"
    (= (type test-fn) clojure.lang.PersistentList)
    "persistent-list"
    true
    (apply test-fn []))))

;; these tests run at load-time:
(def tests
  (map run-test alltests))

;; these tests run at each invocation of (test/run-tests):
(defn run-tests []
  (clojure.string/join ""
                       (map run-test alltests)))

; list of all packages to test (for now you must suffixize with "/test")
;(def alltests (list html/test quiz/test generate/test lev/test))
(def alltests (list html/test))
