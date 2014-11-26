(ns italianverbs.pos.english)

(require '[italianverbs.pos :as pos])
(require '[italianverbs.unify :refer (unifyc)])

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
             :synsem {:infl infl}})))

(def transitive
  (unifyc verb-subjective
          pos/transitive))

(def intransitive-unspecified-obj
  (unifyc verb-subjective
          pos/intransitive-unspecified-obj))

(def intransitive
  (unifyc verb-subjective
          pos/intransitive))

