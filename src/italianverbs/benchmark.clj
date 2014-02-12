(ns italianverbs.benchmark
  (:require
   [italianverbs.cache :refer (over)]
   [italianverbs.grammar :refer (grammar)]
   [italianverbs.lexicon :refer (lexicon)]))

;;
;; Usage:
;;
;; user> (in-ns 'italianverbs.benchmark)
;; #<Namespace italianverbs.benchmark>
;; italianverbs.benchmark> (run-small 5)
;; "Elapsed time: 3215.335 msecs"
;; "Elapsed time: 3240.738 msecs"
;; "Elapsed time: 3246.048 msecs"
;; "Elapsed time: 3219.936 msecs"
;; "Elapsed time: 3226.173 msecs"
;; 5
;; italianverbs.benchmark> 
;;

(def benchmark-small-fn (fn [] (time (take 1 (over grammar
                                                   "io"
                                                   "dormire")))))

(defn print-language-stats []
  (print (str "grammar size:" (.size grammar)))
  (print (str "; lexicon size:" (.size lexicon))))

(defn run-small [n]
  (let [result (take n (repeatedly #(benchmark-small-fn)))]
    (.size result)
    (print-language-stats)))

(def benchmark-medium-fn (fn [] (time (take 1 (over grammar
                                                    "io"
                                                    (over grammar
                                                          "essere"
                                                          (over grammar
                                                                "andare"
                                                                (over grammar
                                                                      "a"
                                                                      (over grammar
                                                                            "il"
                                                                            "mercato")))))))))

(defn run-medium [n]
  (let [result
        (take n (repeatedly #(benchmark-medium-fn)))]
    (print (str "grammar size:" (.size grammar)))
    (print (str "lexicon size:" (.size lexicon)))
    (.size result)
    (print-language-stats)))


