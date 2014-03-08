(ns italianverbs.benchmark
  (:require
   [clojure.set :refer (union)]

   [clojure.tools.logging :as log]

   [italianverbs.cache :refer (build-lex-sch-cache over)]
   [italianverbs.generate :refer :all]
   [italianverbs.grammar :refer :all]
   [italianverbs.lexicon :refer :all]
   [italianverbs.morphology :refer (fo fo-ps)]
   [italianverbs.unify :refer (unifyc)]))

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
(def cache (build-lex-sch-cache grammar
                                (map (fn [lexeme]
                                       (unifyc lexeme
                                               {:phrasal false}))
                                     lexicon)
                                grammar))
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
    (print (str "lexicon size:" (.size lexicon)))
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
  "currently too slow ~ 3-23 seconds"
  "try find the slow parts by constraining the spec."
  (fo (take n (repeatedly #(time (sentence {:head {:comp {:head {:phrasal true}}}
                                            :synsem {:sem {:obj {:pred :pasta}
                                                           :pred :mangiare
                                                           :subj {:pred :io}
                                                           :tense :future}
                                                     :subcat '()}}
                                           lexicon
                                           grammar
                                           cache))))))

(defn benchmark-5 [n]
  "currently too slow ~ 3-5 seconds"
  "like benchmark-4, but trying to find the slow parts by constraining the spec."
  (fo (take n (repeatedly #(time (sentence {:head {:comp {:head {:phrasal false}}} ;; don't generate a noun+adj, just a noun.
                                            :synsem {:sem {:obj {:pred :pasta}
                                                           :pred :mangiare
                                                           :subj {:pred :io}
                                                           :tense :future}
                                                     :subcat '()}}
                                           lexicon
                                           grammar
                                           cache))))))


(defn benchmark-5a [n]
  "currently too slow ~ 1-5 seconds"
  "like benchmark-4, but trying to find the slow parts by constraining the spec."
  (fo (take n (repeatedly #(time (sentence {:comp {:phrasal false} ;; don't try to generate a phrasal form of 'io'
                                            :head {:comp {:head {:phrasal false}}} ;; don't generate a noun+adj, just a noun.
                                            :synsem {:sem {:obj {:pred :pasta}
                                                           :pred :mangiare
                                                           :subj {:pred :io}
                                                           :tense :future}
                                                     :subcat '()}}
                                           lexicon
                                           grammar
                                           cache))))))

(defn benchmark-6 [n]
  (fo (take n (repeatedly #(time (nounphrase {:head {:phrasal false}}
                                             lexicon
                                             grammar
                                             cache))))))



