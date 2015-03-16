(ns italianverbs.pos.english
  (:refer-clojure :exclude [get-in]))

(require '[clojure.tools.logging :as log])
(require '[italianverbs.morphology :refer (fo)])
(require '[italianverbs.pos :as pos])
(require '[italianverbs.unify :as unify :refer (dissoc-paths get-in serialize unifyc)])
(require '[italianverbs.lexiconfn :as lexiconfn :refer (map-function-on-map-vals)])

(def agreement-noun
  (let [agr (ref :top)]
    {:english {:agr agr}
     :synsem {:agr agr}}))

;; A generalization of intransitive and transitive:
;; they both have a subject, thus "subjective".
(def verb-subjective
  (unifyc pos/verb-subjective
          (let [infl (ref :top)
                agr (ref :top)]
            {:english {:agr agr
                       :infl infl}
             :synsem {:infl infl
                      :subcat {:1 {:agr agr}}}})))

(def transitive
  (unifyc verb-subjective
          pos/transitive))

(def intransitive-unspecified-obj
  (unifyc verb-subjective
          pos/intransitive-unspecified-obj))

(def intransitive
  (unifyc verb-subjective
          pos/intransitive))

(defn intransitivize [lexicon]
  (lexiconfn/intransitivize lexicon intransitive transitive))

(defn transitivize [lexicon]
  (map-function-on-map-vals
   lexicon
   (fn [k vals]
     (map (fn [val]
            (cond (and (= (get-in val [:synsem :cat])
                          :verb)
                       (not (nil? (get-in val [:synsem :sem :obj] nil))))
                  (unify/unifyc val
                                transitive)
                  
                  (= (get-in val [:synsem :cat]) :verb)
                  (unify/unifyc val
                                verb-subjective)
                  true
                  val))
          vals))))
