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
            {:italiano {:agr agr}
             :synsem {:subcat {:1 {:sem subj-sem}}}})))
