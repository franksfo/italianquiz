(ns italianverbs.benchmark
  (:require
   [italianverbs.grammar :refer (grammar)]
   [italianverbs.over :refer (over)]))

(def benchmark-small-fn (fn [] (time (take 1 (over grammar
                                                 "io"
                                                 "dormire")))))

(defn run-small [n]
  (let [result (take n (repeatedly #(results-small-fn)))]
    (.size result)))

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
        (take n (repeatedly #(results-medium-fn)))]
    (.size result)))


