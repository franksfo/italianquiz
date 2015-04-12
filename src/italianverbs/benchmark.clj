(ns italianverbs.benchmark
  (:refer-clojure :exclude [get-in])
  (:require
   [clojure.math.numeric-tower :as math]
   [clojure.core.async :as async :exclude [partition-by]]
   [clojure.set :refer (union)]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [italianverbs.cache :as cache]
   [italianverbs.forest :as forest] ;; this allows us to use newly-defined functions from the forest namespace.
   [italianverbs.english :as en]
   [italianverbs.italiano :as it]
   [italianverbs.morphology :refer (fo fo-ps)]
   [italianverbs.over :refer (overc overh)]
   [italianverbs.parse :refer (parse)]
   [italianverbs.ug :refer (head-principle)]
   [italianverbs.unify :as unify]
   [italianverbs.unify :refer (fail? get-in lazy-shuffle unifyc)]))

;;
;; Usage:
;;
;; user> (in-ns 'italianverbs.benchmark)
;; #<Namespace italianverbs.benchmark>
;; italianverbs.benchmark> (standard-benchmark 5)
;;

;; italianverbs.benchmark> (run-benchmark #(fo (it/sentence)) 1)
;; prints:
;;   ' (Il tuo ragazzo contento sarà Giorgio (This guy will be Giorgio).) ' took:  12041  msec.
;;   stats for '(unnamed)' {:trials 1, :mean 12041.0, :median 12041, :stddev 0.0, :min 12041, :max 12041, :95% 12041, :99% 12041}
;;

;; TODO: move stats-related stuff somewhere else.
(defn percentile [percent runtimes]
  (let [sorted-runtimes (sort runtimes)
        trials (.size runtimes)

        increment (/ (* trials 1.0) 100)

        index-of-95-percent
        (- (* increment 95) 1)

        value-of-95-percent
        (nth sorted-runtimes index-of-95-percent)

        index-of-99-percent
        (- (* increment 99) 1)

        value-of-99-percent
        (nth sorted-runtimes index-of-99-percent)

        index-of-median
        (/ (.size sorted-runtimes) 2)
;; (let [arr [10 11 12 13 14]] (nth arr (/ (.size arr) 2)))
;; => 12
;; (let [arr [10 11 12 13 14 15]] (nth arr (/ (.size arr) 2)))
;; => 13

        median
        (nth sorted-runtimes (/ (.size sorted-runtimes) 2))

        mean (/ (reduce + runtimes) (* trials 1.0))

        avg-sum-of-differences-squared
        (/ (reduce + (map #(let [diff (- mean %)]
                             (* diff diff))
                          runtimes))
           mean)
        stddev (math/ceil (math/sqrt avg-sum-of-differences-squared))]

    {:trials trials
     :mean mean
     :median median
     :stddev stddev
     :min (nth sorted-runtimes 0)
     :max (nth sorted-runtimes (- trials 1))
     (keyword (str "95%")) value-of-95-percent
     (keyword (str "99%")) value-of-99-percent
     
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

(defn past-perfect [trials]
  (run-benchmark
   #(fo-ps (it/sentence {:synsem {:sem {:aspect :perfect 
                                        :tense :past 
                                        :pred :perdere
                                        :subj {:pred :io}
                                        :obj {:pred :roma}}}}))
   trials
   "past-perfect-tense generation is needful of optimization."))

(defn sentence-subject-verb [trials]
  (run-benchmark
   #(fo (it/generate {:comp {:phrasal false}
                      :head {:phrasal false}}))
   trials
   "sentence which is simply a subject plus a verb"))

(defn saux [trials]
  (run-benchmark 
   #(fo (it/generate
         {:synsem {:subcat '()}}))))

(defn run-sentence [trials]
  (run-benchmark
   #(fo (it/sentence {:synsem {:sem {:pred :impazzire}}}))
   trials
   "sentence with 'impazzire'"))

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
               it/grammar)))

(def grammar-0
  (filter (fn [rule]
            (not (fail? rule)))
          (map #(unifyc % {:synsem {:subcat '()}})
               it/grammar)))

(def grammar-1
  (filter (fn [rule]
            (not (fail? rule)))
          (mapcat (fn [grammar-0-rule]
                    (map (fn [grammar-rule]
                           (unifyc grammar-0-rule {:head grammar-rule}))
                         it/grammar))
                  grammar-0)))

(def grammar-2
  (filter (fn [rule]
            (not (fail? rule)))
          (mapcat (fn [grammar-1-rule]
                    (map (fn [grammar-rule]
                           (unifyc grammar-1-rule {:head {:head grammar-rule}}))
                         it/grammar))
                  grammar-1)))

(def grammar-3
  (filter (fn [rule]
            (not (fail? rule)))
          (mapcat (fn [grammar-2-rule]
                    (map (fn [grammar-rule]
                           (unifyc grammar-2-rule {:head {:head {:head grammar-rule}}}))
                         it/grammar))
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
                  it/grammar)))

;; (type cp-over-hl) => lazyseq
;; (fo-ps (take 1 (cp-over-hl hl-over-cg1h)))
;; "[vp-imperfetto amare (were loving) [noun-phrase il vostro (your (pl) ) gatto (cat)]]"
(defn catlove []
  (it/generate
   {:synsem {:cat :verb
                     :aux false
             :infl :imperfetto
             :sem {:pred :amare
                   :obj {:pred :gatto}}}}))

(defn run-gatto [trials]
  (run-benchmark
   #(fo-ps (first (take 1 (catlove))))
   trials))

;; the {:subj {:animate true}} is a workaround for rathole prevention - 
;;  subject must be animate, because no verbs (or hardly any) 
;; will work with inanimate subjects (only exception currently is ‘essere’ (e.g. ‘la cipolle è’ ..)
;;
;; To reproduce problem, use: {:sem {:subj {:animate false}}}
(defn bolt-benchmark [trials grammar cache name  &  [spec ]]
  (let [spec 
        (if spec spec
            {:synsem {:cat :verb
                      :aux false
                      :sem {:subj {:animate true}}
                      :subcat '()}})]
    (run-benchmark #(fo (it/generate spec))
                   trials
                   name)))

(defn standard-benchmark-it2en [trials]
  (let [spec 
        {:synsem {:cat :verb
                  :aux false
                  :sem {:subj {:animate true}}
                  :subcat '()}}]
    (run-benchmark #(let [sentence-spec {:synsem {:subcat '()
                                                  :cat :verb
                                                  :subj {:animate true}}};; TODO: why :animate:true? - consider eliminating this.
                          spec (if spec spec :top)
                          unified-spec (unifyc sentence-spec spec)]
                      (fo (en/generate unified-spec)))
                   trials
                   "italian2english")))

(declare standard-benchmark-en)
(declare standard-benchmark-it)

(defn standard-benchmark [ & [ trials ]]
  (let [trials
        (cond (string? trials)
              (Integer/parseInt trials)
              (nil? trials) 1
              true trials)]
    (do (standard-benchmark-it trials)
        (standard-benchmark-en trials)
        (standard-benchmark-it2en trials))))

(defn standard-benchmark-it [ & [ trials ]]
  (let [trials
        (if (nil? trials) 1 trials)]
    (bolt-benchmark trials it/grammar it/index "bolt-benchmark-it")))

(defn standard-benchmark-it-with [ trials spec ]
  (let [trials
        (if (nil? trials) 1 trials)]
    (bolt-benchmark trials it/grammar it/index "bolt-benchmark-it" spec)))

(defn standard-benchmark-en-with [ trials spec ]
  (let [trials
        (if (nil? trials) 1 trials)]
    (bolt-benchmark trials en/grammar en/index "bolt-benchmark-en" spec)))

(defn standard-benchmark-en [ & [ trials ]]
  (let [trialsa
        (if (nil? trials) 1 trials)]
    (bolt-benchmark trials en/grammar en/index "bolt-benchmark-en")))

(defn standard-benchmark-with [ trials spec ]
  (let [trials
        (if (nil? trials) 1 trials)]
    (do (standard-benchmark-it-with trials spec)
        (standard-benchmark-en-with trials spec))))

(defn word-speaker [trials]
  (run-benchmark
   #(fo (it/sentence {:synsem {:subcat '() :cat :verb
                            :sem {:pred :parlare
                                  :subj {:pred :lei}
                                  :obj {:pred :parola}}}}))
   trials))

(defn word-spoken [trials]
  (run-benchmark
   #(fo-ps (take 1 (it/generate)))
   trials))

(defn word-spoken2 [trials]
  ;; TODO: allow it/generate to accept a lexicon as a parameter to (generate)
  (let [lexicon (seq (union (set (it/lookup "parola")) (set (it/lookup "bianco")) (set (it/lookup "la"))))]
    (run-benchmark
     #(fo-ps (take 1 (it/generate)))
     trials)))

(defn parsing [trials]
  (run-benchmark
   #(time (fo-ps (take 1 (parse "il gatto nero dorme"))))
   trials))

