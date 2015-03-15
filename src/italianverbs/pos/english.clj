(ns italianverbs.pos.english)

(require '[italianverbs.pos :as pos])
(require '[italianverbs.unify :as unify :refer (dissoc-paths serialize unifyc)])
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
     (cond (and (= (.size vals) ;; TODO: currently assumes only a single transitive verb per lexical entry: allow more than one.
                   1)
                (not (nil? (get-in (first vals)
                                   [:synsem :sem :obj]
                                   nil))))

           (list (unifyc (first vals) ;; Make a 2-member list. member 1 is the transitive version..
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
           ;; else just return vals.
           true
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
