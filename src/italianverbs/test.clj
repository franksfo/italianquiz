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
  (list (html/tablize
         (apply
          (eval test-fn) []))))

(def tests '(lev/test
             gen/random-present
             gen/random-passato-prossimo
             gen/mobili
             ))

(defn wrap-div [string]
  (str "<div class='test'>" string "</div>"))

(def tests
  (mapcat run-test tests))
