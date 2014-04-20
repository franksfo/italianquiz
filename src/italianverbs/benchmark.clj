(ns italianverbs.benchmark
  (:require
   [clojure.math.numeric-tower :as math]
   [clojure.core.async :as async :exclude [partition-by]]
   [clojure.set :refer (union)]
   [clojure.tools.logging :as log]
   [italianverbs.cache :refer (build-lex-sch-cache over)]
   [italianverbs.forest :exclude [lightning-bolt]]
   [italianverbs.forest :as forest] ;; this allows us to use newly-defined functions from the forest namespace.
   [italianverbs.generate :refer :all]
   [italianverbs.grammar :refer :all]
   [italianverbs.lexicon :refer :all]
   [italianverbs.morphology :refer (fo fo-ps)]
   [italianverbs.over :refer (overc overh)]
   [italianverbs.ug :refer (head-principle)]
   [italianverbs.unify :refer (fail? lazy-shuffle unifyc)]))

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

(log/info "building cache..")
(def cache nil)
(def cache (conj (build-lex-sch-cache grammar
                                      (map (fn [lexeme]
                                             (unifyc lexeme
                                                     {:phrasal false}))
                                           lexicon)
                                      grammar)
                 {:phrase-constraints head-principle})) ;; for now, only one constraint: ug/head-principle.

(log/info "done building cache: " (keys cache))

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
;    (print (str "lexicon size:" (.size lexicon)))
    (.size result)
    (print-language-stats)))

(defn benchmark-1 [n]
  (let [grammar (list s-present)
        lexicon (seq (union (it "io")
                            (it "dormire")))]
    (fo (take n (repeatedly (fn []
                              (time (sentence {:synsem {:infl :present
                                                        :sem {:pred :dormire
                                                              :subj {:pred :io}
                                                              :tense :present}}}
                                              lexicon
                                              grammar
                                              cache))))))))

(defn benchmark-2 [n]
  (let [grammar (list noun-phrase nbar)]
    (fo (take n (repeatedly #(time (nounphrase {:synsem {:sem {:pred :cane}}}
                                               lexicon 
                                               grammar
                                               cache)))))))
 
(defn benchmark-3 [n]
  (let [grammar (list noun-phrase nbar)]
    (fo (take n (repeatedly #(time (nounphrase :top
                                    lexicon
                                    grammar
                                    cache)))))))


(defn benchmark-4 [n]
  "currently too slow ~ 1-3 seconds"
  "try find the slow parts by constraining the spec."
  (fo (take n (repeatedly #(time (sentence {:head {:comp {:head {:phrasal true}}}
                                            :synsem {:sem {:obj {:pred :pasta}
                                                           :pred :mangiare
                                                           :subj {:pred :io}
                                                           :tense :futuro}
                                                     :subcat '()}}
                                           lexicon
                                           grammar
                                           cache))))))

(defn benchmark-5 [n]
  "currently too slow ~ .5~.8 seconds"
  "like benchmark-4, but trying to find the slow parts by constraining the spec."
  (fo (take n (repeatedly #(time (sentence {:head {:comp {:head {:phrasal false}}} ;; don't generate a noun+adj, just a noun.
                                            :synsem {:sem {:obj {:pred :pasta}
                                                           :pred :mangiare
                                                           :subj {:pred :io}
                                                           :tense :futuro}
                                                     :subcat '()}}
                                           lexicon
                                           grammar
                                           cache))))))


(defn benchmark-5a [n]
  "currently too slow ~.6 seconds"
  "like benchmark-4, but trying to find the slow parts by constraining the spec."
  (fo (take n (repeatedly #(time (sentence {:comp {:phrasal false} ;; don't try to generate a phrasal form of 'io'
                                            :head {:comp {:head {:phrasal false}}} ;; don't generate a noun+adj, just a noun.
                                            :synsem {:sem {:obj {:pred :pasta}
                                                           :pred :mangiare
                                                           :subj {:pred :io}
                                                           :tense :futuro}
                                                     :subcat '()}}
                                           lexicon
                                           grammar
                                           cache))))))

(defn benchmark-6 [n]
  (fo (take n (repeatedly #(time (nounphrase {:head {:phrasal false}}
                                             lexicon
                                             grammar
                                             cache))))))

;; these are currently take way too long - either 12-20 seconds or 37ms (latter is a bug: (sentence) returned nil)
(defn benchmark-7 [n]
  (fo (take n (repeatedly #(time (sentence {:synsem {:cat :sent-modifier}}
                                           lexicon
                                           grammar
                                           cache))))))

(defn async-test [n]
  (let [cs (repeatedly n async/chan)
        begin (System/currentTimeMillis)]
    (doseq [c cs] (async/go (>! c (nounphrase {:head {:phrasal false}} lexicon grammar cache))))
    (dotimes [i n]
      (let [[v c] (async/alts!! cs)]
        (log/info (str "core async nounphrase: " (fo v)))))
    (println "Generated " n " noun phrases in" (- (System/currentTimeMillis) begin) "ms")))

(defn async-test-do-all []
  (let [n 1
        begin (System/currentTimeMillis)]
    (let [nounphrase (nounphrase {:head {:phrasal true}} lexicon grammar cache)]
      (log/info (str "core async generated noun phrase: " (fo nounphrase)))
      (log/info "Generated " n " noun phrases in" (- (System/currentTimeMillis) begin) "ms"))))

(defn percentile [percent runtimes]
  ;; for now, ignore percent and do 95%.
  (let [sorted-runtimes (sort runtimes)
        trials (.size runtimes)

        increment (/ (* trials 1.0) 100)

        index-of-ninety-fifther 
        (- (* increment 95) 1)

        value-of-ninety-fifther 
        (nth sorted-runtimes index-of-ninety-fifther)]

    {;:incr increment
     :min (nth sorted-runtimes 0)
     :max (nth sorted-runtimes (- trials 1))
;     :ninety-fifther-index index-of-ninety-fifther
     :95% value-of-ninety-fifther
     }))

(defn run-benchmark [function-to-evaluate trials]
  (if (> trials 0)
    (let [runtimes
          (map (fn [x] 
                 (let [begin (System/currentTimeMillis)
                       result (function-to-evaluate)
                       end (System/currentTimeMillis)
                       runtime (- end begin)]
                        (println "'" result "' took: " runtime " msec.")
                        runtime))
               (range 0 trials))]
      ;; note cast of _trials_ to float by multiplying it times a float.
      (let [average (/ (reduce + runtimes) (* trials 1.0))
            avg-sum-of-differences-squared
            (/ (reduce + (map #(let [diff (- average %)]
                              (* diff diff))
                           runtimes))
               average)]
        ;; TODO: calculate median and 95% pct
        (println "average: " average "msec.")
        (println "percentiles:" (percentile 95 runtimes))
        (println "stddev: " (math/ceil (math/sqrt avg-sum-of-differences-squared)))))))

(defn spresent [trials]
  (run-benchmark
   #(fo (first (take 1 (lightning-bolt (list s-present)
                            cache
                            {:comp {:phrasal false}
                             :head {:phrasal false}}))))
   trials))

(defn saux [trials]
  (run-benchmark 
   #(fo (first (take 1 (lightning-bolt (list s-aux vp-aux) cache {:synsem {:subcat '()}}))))
   trials))

(defn run-hlcp [trials]
  (run-benchmark
   #(fo (short-sentence))
   trials))

(defn run-hlcp2 [trials]
  (run-benchmark
   #(fo (short-sentence {:synsem {:sem {:pred :impazzire}}}))
   trials))

(defn run-hlcl [trials]
  (run-benchmark
   #(fo (first (take 1 (forest/hlcl cache grammar {:synsem {:cat :verb :subcat '()}}))))
   trials))

(defn run-hlcp [trials]
  (run-benchmark
   #(fo (first (take 1 (forest/hlcp cache grammar {:synsem {:cat :verb :subcat '()}}))))
   trials))

(defn run-hpcl [trials]
  (run-benchmark
   #(fo (first (take 1 (forest/hpcl cache (list vp-pronoun) {:synsem {:cat :verb :subcat '()}
                                                             :head {:synsem {:subcat {:1 :top}}
                                                                    :head {:subcat {:1 :top
                                                                                    :2 :top}
                                                                           :aux true}}}))))

                                                                            
                                                            
   trials))

(defn run-test [trials]
  (run-benchmark
   #(fo (first (take 1 (forest/hlcl cache (list vp-aux) {:synsem {:cat :verb 
                                                                  :subcat {:1 :top}}
                                                         :head {:subcat {:1 :top
                                                                         :2 :top}
                                                                :aux true}}))))

                                                                            
                                                            
   trials))

(defn run-hlcl-test [trials]
  (run-benchmark
   #(fo (first (take 1 (forest/hlcl cache grammar :top))))
   trials))


(defn run-hpcp [trials]
  (run-benchmark
   #(fo (first (take 1 (forest/hpcp cache grammar {:synsem {:infl :futuro :cat :verb :subcat '()}
                                                   :head {:synsem {:subcat {:1 :top
                                                                            :2 '()}}}}))))
   trials))

(defn run-hxcx [trials]
  (run-benchmark
   #(fo (first (take 1 (forest/hxcx cache grammar {:synsem {:infl :futuro :cat :verb :subcat '()}
                                                   :head {:synsem {:subcat {:1 :top
                                                                            :2 '()}}}}))))
   trials))

(defn benchmark []
  (println "-- hlcl --")
  (run-hlcl 10)
  (println)
  (println "-- hpcl --")
  (run-hpcl 10)
  (println)
  (println "-- hlcp --")
  (run-hlcp 10)
  (println)
  (println "-- hpcp --")
  (run-hpcp 10))
