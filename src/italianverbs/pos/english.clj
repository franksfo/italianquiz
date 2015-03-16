(ns italianverbs.pos.english
  (:refer-clojure :exclude [get-in]))

(require '[clojure.tools.logging :as log])
(require '[italianverbs.morphology :refer (fo)])
(require '[italianverbs.pos :as pos])
(require '[italianverbs.unify :as unify :refer (dissoc-paths get-in serialize unifyc)])
(require '[italianverbs.lexiconfn :refer (map-function-on-map-vals)])

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
  (map-function-on-map-vals
   lexicon
   (fn [k vals]
     (mapcat (fn [val]
               (cond (and (= (get-in val [:synsem :cat])
                             :verb)
                          (not (nil? (get-in val
                                             [:synsem :sem :obj]
                                             nil))))
                     
                     (list (unifyc val ;; Make a 2-member list. member 1 is the transitive version..
                                   transitive)
                           
                           ;; and the other member of the list being the intransitive version.
                           ;; Turn the singular, transitive form into an intranstive form by
                           ;; doing some surgery on it: (remove the object) and intransitivize it
                           (let [without-object  ;; intransitive version
                                 (unifyc intransitive
                                         (dissoc-paths (first vals)
                                                       (list [:serialized]
                                                             [:synsem :sem :obj]
                                                             [:synsem :subcat :2])))]
                             (merge without-object
                                    {:serialized (serialize without-object)})))
                     
                     (= (get-in val [:synsem :cat])
                        :verb)
                     (do (log/trace (str "val:" (fo val)))
                         (log/trace (str "map-unified:" (fo (map #(unifyc % intransitive)
                                                                 val))))
                         (list (unifyc val intransitive)))
                              
                     ;; else just return vals:
                     true
                     (do (log/trace (str "no modifications apply for val: " (fo val) " ; cat: " (get-in val [:synsem :cat])))
                         (list val))))
             vals))))

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
