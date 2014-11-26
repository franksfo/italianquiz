(ns italianverbs.pos.italiano)

(require '[italianverbs.pos :as pos])
(require '[italianverbs.unify :refer (unifyc)])

;; A generalization of intransitive and transitive:
;; they both have a subject, thus "subjective".
(def verb-subjective
  (unifyc pos/verb-subjective
          (let [subj-sem (ref :top)
                subject-agreement (ref :nom)
                infl (ref :top)
                agr (ref :top)
                essere-type (ref :top)]
            {:italiano {:agr agr
                        :essere essere-type
                        :infl infl}
             :synsem {:infl infl
                      :essere essere-type
                      :subcat {:1 {:sem subj-sem}}}})))

(def transitive
  (unifyc verb-subjective
          pos/transitive))

(def intransitive-unspecified-obj
  (unifyc verb-subjective
          pos/intransitive-unspecified-obj))

(def intransitive
  (unifyc verb-subjective
          pos/intransitive))

