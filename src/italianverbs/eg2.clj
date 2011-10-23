(ns italianverbs.search
  (:use [hiccup core page-helpers]
        [clojure.test]
        [delimc.core]))

;; Ported from the Scala version presented at :
;; http://dcsobral.blogspot.com/2009/07/delimited-continuations-explained-in.html
;;I believe that once I understand why (baz) => 70; i will understand delimited continuations.

(defn baz []
  (do
    (println "start of the baz2 function..")
    (reset
     (* 
      (do
        (println "start of the bar part.")
        (let [result (+ 1 
                        (do
                          (println "start of the foo part.")
                          (let [result 
                                (shift k
                                       (k (k (k 7))))]
                            (println "calculated shift part: " result)
                            result)))]
          (println "bar result: " + result)
          result))
      2)))) ;; output of (baz) : 70





  
