(ns italianverbs.benchmark
  (:refer-clojure :exclude [get-in])
  (:require
   [clojure.math.numeric-tower :as math]
   [clojure.core.async :as async :exclude [partition-by]]
   [clojure.set :refer (union)]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [italianverbs.cache :refer (build-lex-sch-cache over spec-to-phrases)]
   [italianverbs.forest :exclude [lightning-bolt]]
   [italianverbs.forest :as forest] ;; this allows us to use newly-defined functions from the forest namespace.
   [italianverbs.generate :refer :all]
   [italianverbs.grammar :refer :all]
   [italianverbs.lexicon :refer :all]
   [italianverbs.morphology :refer (fo fo-ps)]
   [italianverbs.over :refer (overc overh)]
   [italianverbs.ug :refer (head-principle)]
   [italianverbs.unify :as unify]
   [italianverbs.unify :refer (fail? get-in lazy-shuffle unifyc)]))

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
;; TODO: trying to print cache takes forever and blows up emacs buffer:
;; figure out how to change printable version to (keys cache).
(def cache (conj (build-lex-sch-cache grammar
                                      (map (fn [lexeme]
                                             (unifyc lexeme
                                                     {:phrasal false}))
                                           lexicon)
                                      grammar)
                 {:phrase-constraints head-principle ;; for now, only one constraint: ug/head-principle.
                  :phrases-for-spec
                  (spec-to-phrases
                   ;; TODO: make this list derivable from the grammar and /or lexicon.
                   (list {:synsem {}, :head {:synsem {}}, :phrasal true}
                         {:synsem {:cat :verb, :aux false}, :head {:synsem {:subcat {:2 {}, :1 {}}, :infl :present, :cat :verb, :sem {:tense :present}}, :phrasal false}, :phrasal true}
                         {:synsem {:cat :verb}, :head {:synsem {:cat :verb, :infl {:not :past}, :subcat {:2 {:cat :noun, :subcat (), :pronoun true}, :1 {}}}, :phrasal false}, :phrasal true}
                         {:synsem {:cat :verb, :aux false}, :head {:synsem {:cat :verb, :infl :infinitive, :subcat {:2 {}, :1 {}}}, :phrasal false}, :phrasal true}
                         )
                   grammar)}))

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

(def benchmark-1-spec
  {:synsem {:infl :present
            :sem {:pred :dormire
                  :subj {:pred :io}
                  :tense :present}}})

(defn benchmark-1-sentence []
  (sentence benchmark-1-spec
            lexicon
            grammar
            cache))

(defn benchmark-1 [n]
  (let [grammar (list s-present)
        lexicon (seq (union (it "io")
                            (it "dormire")))]
    (fo (take n (repeatedly (fn []
                              (time (benchmark-1-sentence))))))))

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
  (let [sorted-runtimes (sort runtimes)
        trials (.size runtimes)

        increment (/ (* trials 1.0) 100)

        index-of-chosen-percent
        (- (* increment percent) 1)

        value-of-chosen-percent
        (nth sorted-runtimes index-of-chosen-percent)

        mean (/ (reduce + runtimes) (* trials 1.0))

        avg-sum-of-differences-squared
        (/ (reduce + (map #(let [diff (- mean %)]
                             (* diff diff))
                          runtimes))
           mean)
        stddev (math/ceil (math/sqrt avg-sum-of-differences-squared))]

    {:mean mean
     :stddev stddev
     :min (nth sorted-runtimes 0)
     :max (nth sorted-runtimes (- trials 1))
     (keyword (str percent "%")) value-of-chosen-percent
     }))

(defn run-benchmark [function-to-evaluate trials & [name]]
  (if (> trials 0)
    (let [name (if name name "(unnamed)")
          runtimes
          (map (fn [x] 
                 (let [begin (System/currentTimeMillis)
                       result (function-to-evaluate)
                       end (System/currentTimeMillis)
                       runtime (- end begin)]
                        (println "'" result "' took: " runtime " msec.")
                        runtime))
               (range 0 trials))]
      (println (str "stats for '" (string/trim (string/join "" name)) "' " (percentile 95 runtimes))))))

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
   trials
   "hlcp"))

(defn run-hlcp2 [trials]
  (run-benchmark
   #(fo (short-sentence {:synsem {:sem {:pred :impazzire}}}))
   trials
   "hlcp with 'impazzire'"))

(defn run-hlcl [trials]
  (run-benchmark
   #(fo (first (take 1 (forest/hlcl cache grammar {:synsem {:cat :verb :subcat '()}}))))
   trials
   "hlcl"))

(defn run-hlcp [trials]
  (run-benchmark
   #(fo-ps (first (take 1 (forest/hlcp cache grammar {:synsem {:cat :verb :subcat '()}}))))
   trials
   "hlcp with empty subcat"))

(defn run-hpcl [trials]
  (run-benchmark
   #(fo (first (take 1 (forest/hpcl cache grammar {:synsem {:sem {:pred :volere} :cat :verb :subcat '()}}))))
   trials
   "hpcl with empty subcat and pred=volere"))

(defn run-hpcp [trials]
  (run-benchmark
   #(fo (first (take 1 (forest/hpcp cache grammar {:synsem {:cat :verb :subcat '()}}))))
   trials
   "hpcp with empty subcat"))

(defn run-test [trials]
  (run-benchmark
   #(fo (first (take 1 (forest/hlcl cache (list vp-aux) {:synsem {:cat :verb 
                                                                  :subcat {:1 :top}}
                                                         :head {:subcat {:1 :top
                                                                         :2 :top}
                                                                :aux true}}))))
   trials
   "vp-aux"))

(defn run-hlcl-test [trials]
  (run-benchmark
   #(fo-ps (first (take 1 (forest/hlcl cache grammar :top))))
   trials
   "hlcl"))

(defn run-hlcl-with-subcat-nil-test [trials]
  (run-benchmark
   #(fo-ps (first (take 1 (forest/hlcl cache grammar {:synsem {:subcat '()}}))))
   trials
   "hlcl with empty subcat"))

(defn run-hp-with-subcat-nil-test [trials]
  (run-benchmark
   #(fo-ps (first (take 1 (forest/hp cache grammar {:synsem {:subcat '()}}))))
   trials
   "hp with empty subcat"))

(defn run-hpcl-with-subcat-nil-test [trials]
  (run-benchmark
   #(fo-ps (first (take 1 (forest/hpcl cache grammar {:synsem {:subcat '()}}))))
   trials
   "hpcl with empty subcat"))

(defn run-hpcp-with-subcat-nil-test [trials]
  (run-benchmark
   #(fo-ps (first (take 1 (forest/hpcp cache grammar {:synsem {:subcat '()}}))))
   trials
   "hpcp with empty subcat"))

(defn run-suoceri [trials]
  (run-benchmark
   #(fo (first (take 1 (forest/hpcp cache grammar {:synsem {:cat :verb
                                                            :aux false
                                                            :infl :imperfetto
                                                            :sem {:pred :amare
                                                                  :obj {:pred :giorgio}
                                                                  :subj {:pred :suoceri}}
                                                            :subcat '()}}))))
   trials))

(def catspec-s
  {:synsem {:cat :verb
            :aux false
            :infl :imperfetto
            :sem {:pred :amare
                  :obj {:pred :gatto}}
            :subcat '()}})

(def catspec-grammar-0
  (filter (fn [rule]
            (not (fail? rule)))
          (map #(unifyc % catspec-s)
               grammar)))

(def grammar-0
  (filter (fn [rule]
            (not (fail? rule)))
          (map #(unifyc % {:synsem {:subcat '()}})
               grammar)))

(def grammar-1
  (filter (fn [rule]
            (not (fail? rule)))
          (mapcat (fn [grammar-0-rule]
                    (map (fn [grammar-rule]
                           (unifyc grammar-0-rule {:head grammar-rule}))
                         grammar))
                  grammar-0)))

(def grammar-2
  (filter (fn [rule]
            (not (fail? rule)))
          (mapcat (fn [grammar-1-rule]
                    (map (fn [grammar-rule]
                           (unifyc grammar-1-rule {:head {:head grammar-rule}}))
                         grammar))
                  grammar-1)))

(def grammar-3
  (filter (fn [rule]
            (not (fail? rule)))
          (mapcat (fn [grammar-2-rule]
                    (map (fn [grammar-rule]
                           (unifyc grammar-2-rule {:head {:head {:head grammar-rule}}}))
                         grammar))
                  grammar-2)))

(def grammar-full
  (union grammar-1 grammar-2 grammar-3))

;; all possible expansions of above subgrammar's heads:
(def catspec-grammar-1-head
  (filter (fn [rule]
            (not (fail? rule)))
          (mapcat (fn [grammar-rule]
                    (map (fn [catspec-grammar-0-rule-head]
                           (unifyc grammar-rule catspec-grammar-0-rule-head))
                         (map #(unify/get-in % [:head])
                              catspec-grammar-0)))
                  grammar)))

(def hl-over-cg1h
  (forest/hl cache catspec-grammar-1-head))

(defn cp-over-hl [subgrammar]
  (forest/cp subgrammar cache grammar))

;; (type cp-over-hl) => lazyseq
;; (fo-ps (take 1 (cp-over-hl hl-over-cg1h)))
;; "[vp-imperfetto amare (were loving) [noun-phrase il vostro (your (pl) ) gatto (cat)]]"
(defn catlove []
  (forest/hlcp cache grammar {:synsem {:cat :verb
                                       :aux false
                                       :infl :imperfetto
                                       :sem {:pred :amare
                                             :obj {:pred :gatto}}}}))

(defn run-gatto [trials]
  (run-benchmark
   #(fo-ps (first (take 1 (catlove))))
   trials))

(defn run-gatto2 [trials]
  (run-benchmark
   #(fo-ps (first (take 1 (cp-over-hl (forest/hl cache catspec-grammar-1-head)))))
   trials))

(defn run-gatto3 [trials]
  (run-benchmark
   #(fo-ps (first (take 1 (cp-over-hl hl-over-cg1h))))
   trials))

(defn run-gatto4 [trials]
  (run-benchmark
   #(fo-ps (first 
            (take 1 (cp-over-hl 
                     (forest/hl cache 
                                (filter (fn [rule]
                                          (not (fail? rule)))
                                        (mapcat (fn [grammar-rule]
                                                  (map (fn [catspec-grammar-0-rule-head]
                                                         (unifyc grammar-rule catspec-grammar-0-rule-head))
                                                       (map (fn [x] 
                                                              (unify/get-in x [:head]))
                                                            (let [catspec-s
                                                                  {:synsem {:cat :verb
                                                                            :aux false
                                                                            :infl :imperfetto
                                                                            :sem {:pred :amare
                                                                                  :obj {:pred :gatto}}
                                                                            :subcat '()}}

                                                                  catspec-grammar-0
                                                                  (filter (fn [rule]
                                                                            (not (fail? rule)))
                                                                          (map 
                                                                           (fn [rule]
                                                                             (unifyc rule catspec-s))
                                                                           grammar))]
                                                              catspec-grammar-0))))
                                                grammar)))))))
   
   trials))

(defn run-hlcp-with-subcat-nil-test [trials]
  (run-benchmark
   #(fo-ps (first (take 1 (forest/hlcp cache grammar {:synsem {:subcat '()}}))))
   trials))

(defn run-hlcl-test-aux [trials]
"this one seems to be an outlier for hlcl - try to make it faster."
(let [essere true
      grammar grammar]
  (run-benchmark
   
   #(fo (first (take 1 (forest/hlcl cache grammar {:synsem {:sem {:pred :venire}
                                                            :essere essere
                                                            :aux true}}))))
   trials)))

(defn run-hl-test-aux [trials]
"this one seems to be an outlier for hlcl - try to make it faster."
(let [essere true
      grammar grammar]
  (run-benchmark
   
   #(fo-ps (first (take 1 (forest/hl cache grammar {:synsem {:sem {:pred :venire}
                                                            :essere essere
                                                            :aux true}}))))
   trials)))

(defn run-hpcp2 [trials]
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
  (println "run-hlcl-test 10")
  (run-hlcl-test 10)

  (println "run-hlcl-test-aux 10")
  (run-hlcl-test-aux 10)

  (println "run-hlcp-with-subcat-nil-test 10")
  (run-hlcp-with-subcat-nil-test 10)

  (println "run-hp-with-subcat-nil-test 10")
  (run-hp-with-subcat-nil-test 10)

  (println "run-hpcl-with-subcat-nil-test 10")
  (run-hpcl-with-subcat-nil-test 10)

  (println "run-hpcp-with-subcat-nil-test 10")
  (run-hpcp-with-subcat-nil-test 10))




