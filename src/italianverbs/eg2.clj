(ns italianverbs.search
  (:use [hiccup core page-helpers]
        [clojure.test]
        [delimc.core]))

;; Ported from the Scala version presented at :
;; http://dcsobral.blogspot.com/2009/07/delimited-continuations-explained-in.html
;;I believe that once I understand why (foo) => 70; i will understand delimited continuations.

(defn foo []
  (do
    (println "start of foo.")
    (let [result
          (reset
           (* 
            (do
              (println "start of bar.")
              (let
                  [result (+ 1 
                             (do
                               (println "start of baz.")
                               (let [result 
                                     (shift k
                                            ;; controls how many "end of baz"s are printed.
                                            ;; (k (k 7))     : 2 'end of baz'
                                            ;; (k (k (k 7))) : 3 'end of baz'
                                            ;; .. etc.
                                            (k (k (k 7))))]
                                 (println "end of baz: " result)
                                 result)))]
                (println "end of bar:" result)
                result))
            2))]
      (println "end of foo:" result)
      result)))
;; output of (foo) :
;; 1 k:  16
;; 2 ks: 34
;; 3 ks: 70
;; 4 ks: 142





  
