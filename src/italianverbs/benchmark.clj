(ns italianverbs.benchmark
  (:require
   [italianverbs.grammar :refer (grammar)]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.over :refer :all]))

(def results-small-fn (fn [] (time (fo (take 1 (over grammar 
                                                     "io" 
                                                     "dormire"))))))

(defn run-small [n]
  (take n (repeatedly #(results-small-fn))))

(def results-medium-fn (fn [] (time (fo (take 1 (over grammar 
                                                      "io" 
                                                      (over grammar 
                                                            "essere" 
                                                            (over grammar 
                                                                  "andare" 
                                                                  (over grammar 
                                                                        "a" 
                                                                        (over grammar 
                                                                              "il" 
                                                                              "mercato"))))))))))

(defn run-medium [n]
  (take n (repeatedly #(results-medium-fn))))


