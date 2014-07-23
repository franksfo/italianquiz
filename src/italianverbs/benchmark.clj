(ns italianverbs.benchmark
  (:refer-clojure :exclude [get-in])
  (:require
   [clojure.math.numeric-tower :as math]
   [clojure.core.async :as async :exclude [partition-by]]
   [clojure.set :refer (union)]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [italianverbs.cache :refer (build-lex-sch-cache over spec-to-phrases get-comp-phrases-of)]
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

(defn sentence-subject-verb [trials]
  (run-benchmark
   #(fo (generate {:comp {:phrasal false}
                   :head {:phrasal false}}
                  lexicon
                  (list s-present)
                  cache))
   trials
   "sentence which is simply a subject plus a verb"))

(defn saux [trials]
  (run-benchmark 
   #(fo (generate
         {:synsem {:subcat '()}}
         lexicon
         (list s-aux vp-aux) cache))
   trials
   "saux"))

(defn run-hlcp2 [trials]
  (run-benchmark
   #(fo (sentence {:synsem {:sem {:pred :impazzire}}}))
   trials
   "sentence with 'impazzire'"))

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
   #(fo (first (take 1 (forest/hpcl cache grammar {:synsem {:cat :verb :subcat '()}}))))
   trials
   "hpcl"))

(defn run-hpcl-v [trials]
  (run-benchmark
   #(fo (first (take 1 (forest/hpcl cache grammar {:synsem {:cat :verb
                                                            :infl :futuro
                                                            :sem {:obj {:pred :ridere} 
                                                                  :pred :volere
;                                                                  :tense :futuro
                                                                  :subj {:pred :paola}}
                                                            :subcat '()}}))))
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

;; the {:subj {:animate true}} is a workaround for rathole prevention - 
;;  subject must be animate, because no verbs (or hardly any) 
;; will work with inanimate subjects (only exception currently is ‘essere’ (e.g. ‘la cipolle è’ ..)
;;
;; To reproduce problem, use: {:sem {:subj {:animate false}}}
(defn bolt-benchmark [trials]
  (let [spec {:synsem {:cat :verb
                       :aux false
                       :sem {:subj {:animate true}}
                       :subcat '()}}]
    (run-benchmark #(fo (first (take 1 (forest/gen2 grammar lexicon spec cache))))
                   trials
                   "bolt-benchmark")))

(defn run-hlcp-with-subcat-nil-test [trials]
  (run-benchmark
   #(fo-ps (first (take 1 (forest/hlcp cache grammar {:synsem {:subcat '()}}))))
   trials
   "hlcp with empty subcat"))

(defn run-hlcl-test-aux [trials]
  ;; ?? this comment says hlcl: TODO resolve if this is also an outlier for its respective generic
  ;; generation rule (hl).
  "this one seems to be an outlier for hlcl - try to make it faster."
  (let [essere true
        grammar grammar]
    (run-benchmark
   
     #(fo (first (take 1 (forest/hlcl cache grammar {:synsem {:sem {:pred :venire}
                                                              :essere essere
                                                              :aux true}}))))
     trials
     "hlcl with aux = true and pred=venire")))

(defn run-hpcp2 [trials]
  (run-benchmark
   #(fo (first (take 1 (forest/hpcp cache grammar lexicon {:synsem {:infl :futuro :cat :verb :subcat '()}
                                                           :head {:synsem {:subcat {:1 :top
                                                                                    :2 '()}}}}))))
   trials))

(defn run-hxcx [trials]
  (run-benchmark
   #(fo (first (take 1 (forest/gen2 cache grammar lexicon
                                    {:synsem {:infl :futuro :cat :verb :subcat '()}
                                     :head {:synsem {:subcat {:1 :top
                                                              :2 '()}}}}))))
   trials))

(defn word-speaker [trials]
  (run-benchmark
   #(fo (sentence {:synsem {:subcat '() :cat :verb
                            :sem {:pred :parlare
                                  :subj {:pred :lei}
                                  :obj {:pred :parola}}}}
                  lexicon grammar))
   trials))

(defn word-spoken [trials]
  (run-benchmark
   #(fo-ps (generate {:synsem {:subcat '() :cat :noun
                               :sem {:pred :parola}}}
                     lexicon grammar))
   trials))

(defn word-spoken2 [trials]
  (let [lexicon (seq (union (it "parola") (it "bianco") (it "la")))]
    (run-benchmark
     #(fo-ps (generate {:synsem {:subcat '() :cat :noun
                                 :sem {:pred :parola}}}
                       lexicon (list np nbar)))
     trials)))

(defn word-speaker-lb [trials]
  (run-benchmark
   #(fo (take 1 (forest/lb grammar lexicon {:synsem {:subcat '() :cat :verb
                                                     :sem {:pred :parlare
                                                           :subj {:pred :lei}
                                                           :obj {:pred :parola}}}})))
   trials))

(defn benchmark []
  (println "run-hlcl-test 10")
  (run-hlcl-test 10)

  (println "run-hlcl-test-aux 10")
  (run-hlcl-test-aux 10)

  (println "run-hlcp-with-subcat-nil-test 10")
  (run-hlcp-with-subcat-nil-test 10)

  (println "run-hpcl-with-subcat-nil-test 10")
  (run-hpcl-with-subcat-nil-test 10)

  (println "run-hpcp-with-subcat-nil-test 10")
  (run-hpcp-with-subcat-nil-test 10))




