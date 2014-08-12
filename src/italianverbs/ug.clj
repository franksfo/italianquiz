(ns italianverbs.ug
  (:refer-clojure :exclude [get-in merge resolve])
  (:require [clojure.tools.logging :as log]
            [italianverbs.lexicon :as lex]
            [italianverbs.lexiconfn :refer (sem-impl)]
            [italianverbs.morphology :refer (fo fo-ps)]
            [italianverbs.unify :refer (fail? get-in merge unifyc)]
            [clojure.string :as string]))

;; ^^ true: pre-compute cross product of phrases X lexicon (slow startup, fast runtime)
;;    false: don't pre-compute product (fast startup, slow runtime)

(def phrasal {:phrasal true})

;;    [1]
;;   /   \
;;  /     \
;; H[1]    C
(def head-principle-no-infl
  (let [head-cat (ref :top)
        head-essere (ref :top)
        head-is-pronoun (ref :top)
        head-sem (ref :top)
        sem-mod (ref :top)]
    (unifyc phrasal
            {:synsem {:cat head-cat
                      :essere head-essere
                      :pronoun head-is-pronoun
                      :sem head-sem
                      :sem-mod sem-mod}
             :head {:synsem {:cat head-cat
                             :essere head-essere
                             :pronoun head-is-pronoun
                             :sem head-sem
                             :sem-mod sem-mod}}})))

;;    [1]
;;   /   \
;;  /     \
;; H[1]    C
(def head-principle
  (unifyc head-principle-no-infl
          phrasal
          (let [head-infl (ref :top)
                agr (ref :top)]
            {:synsem {:infl head-infl
                      :agr agr}
             :head {:synsem {:infl head-infl
                             :agr agr}}})))

;;     subcat<>
;;     /      \
;;    /        \
;; H subcat<1>  C[1]
(def subcat-1-principle
  (let [comp-synsem (ref :top)]
    {:synsem {:subcat '()}
     :head {:synsem {:subcat {:1 comp-synsem
                              :2 '()}}}
     :comp {:synsem comp-synsem}}))

;;     subcat<>
;;     /      \
;;    /        \
;; H subcat<1>  C[1]
(def subcat-1-principle-no-complement-subcat-restrictions
  (let [comp-synsem (ref {:subcat :top})]
    {:synsem {:subcat '()}
     :head {:synsem {:subcat {:1 comp-synsem
                              :2 '()}}}
     :comp {:synsem comp-synsem}}))

;;     subcat<1>
;;     /      \
;;    /        \
;; H subcat<1>  C<>
(def subcat-1-1-principle
  (let [subcat (ref :top)]
    {:synsem {:subcat {:1 subcat
                       :2 '()}}
     :comp {:synsem {:subcat '()}}
     :head {:synsem {:subcat {:1 subcat
                              :2 '()}}}}))


;;     subcat<1,2>
;;     /         \
;;    /           \
;; H subcat<1,3>  3:C<1,2>
(def subcat-2-2-principle
  (let [subcat1 (ref :top)
        subcat2 (ref :top)
        subcat3 (ref {:subcat {:1 subcat1
                               :2 subcat2
                               :3 '()}})]
    {:synsem {:subcat {:1 subcat1
                       :2 subcat2
                       :3 '()}}
     :comp {:synsem subcat3}
     :head {:synsem {:subcat {:1 subcat1
                              :2 subcat3
                              :3 '()}}}}))

;;     subcat<1>
;;     /      \
;;    /        \
;; H subcat<1>  C<>
(def subcat-1-1-principle-comp-subcat-1
  (let [subcat (ref :top)]
    {:synsem {:subcat {:1 subcat
                       :2 '()}}
     :comp {:synsem {:subcat {:1 :top
                              :2 '()}}}
     :head {:synsem {:subcat {:1 subcat
                              :2 '()}}}}))


;;     subcat<1>
;;     /      \
;;    /        \
;; H subcat<1,2>  C[2]
(def subcat-2-principle
  (let [comp-synsem (ref {:cat :top})
        parent-subcat (ref {:cat :top})]
    {:synsem {:subcat {:1 parent-subcat
                       :2 '()}}
     :head {:synsem {:subcat {:1 parent-subcat
                              :2 comp-synsem
                              :3 '()}}}
     :comp {:synsem comp-synsem}}))

;;     subcat<1,3>
;;     /      \
;;    /        \
;; H subcat<1,2>  C[2]<1,3>
(def subcat-3-principle
  (let [subcat-1 (ref :top)
        subcat-3 (ref :top)
        subcat-2 (ref {:subcat {:1 subcat-1
                                :2 subcat-3}})]
    {:synsem {:subcat {:1 subcat-1
                       :2 subcat-3
                       :3 '()}}
     :head {:synsem {:subcat {:1 subcat-1
                              :2 subcat-2
                              :3 '()}}}
     :comp {:synsem subcat-2}}))

;;     subcat<1>
;;     /      \
;;    /        \
;; H subcat<2>  C[2]<1>
(def subcat-4-principle
  (let [subcat-1 (ref :top)
        subcat-2 (ref {:subcat {:1 subcat-1}})]
    {:synsem {:subcat {:1 subcat-1
                       :2 '()}}
     :head {:synsem {:subcat {:1 subcat-2}}}
     :comp {:synsem subcat-2}}))

;;       subcat<1,2>
;;      /          \
;;     /            \
;; H subcat<1,2,3>  C[3]
(def subcat-5-principle
  ;; we specify {:cat :top} rather than simply :top
  ;; because we want to prevent matching with '()
  ;; that is, a verb which only subcats for :1 and 2: (transitive)
  ;; would match :3 because (unify '() :top) => :top,
  ;; and would fit in here erroneously.
  ;; This is prevented by {:cat :top},
  ;; because (unify '() {:cat :top}) => :fail.
  (let [subcat-1 (ref {:cat :top})
        subcat-2 (ref {:cat :top})
        subcat-3 (ref {:cat :top})]
    {:head {:synsem {:subcat {:1 subcat-1
                              :2 subcat-2
                              :3 subcat-3}}}
     :comp {:synsem subcat-3}
     :synsem {:subcat {:1 subcat-1
                       :2 subcat-2}}}))

(def standard-filter-fn
  (fn [additional-phrase-with-head]
    (fn [phrase-with-head]
      (let [phrase-with-head (unifyc additional-phrase-with-head
                                    phrase-with-head)]
        (do
          (if (fail? phrase-with-head)
            (throw (Exception. (str "phrase-with-head is fail: " phrase-with-head))))
          (fn [comp]
            (let [result
                  {:essere
                   (unifyc (get-in phrase-with-head '(:comp :synsem :essere) :top)
                           (get-in comp '(:synsem :essere) :top))
                   :agr
                   (unifyc (get-in phrase-with-head '(:comp :synsem :agr) :top)
                           (get-in comp '(:synsem :agr) :top))
                   :sem
                   (unifyc (sem-impl (get-in phrase-with-head '(:comp :synsem :sem) :top))
                           (sem-impl (get-in comp '(:synsem :sem) :top)))}]
              (if (not (fail? result))
                ;; complement was compatible with the filter: not filtered out.
                true
                ;; complement was incompatible with the filter and thus filtered out:
                false))))))))

(def comp-modifies-head
  (let [human (ref :top)
        animate (ref :top)
        comp-semantics (ref {:animate animate :human human})
        head-semantics (ref {:animate animate :human human :mod comp-semantics})]
    {:head {:synsem {:sem head-semantics}}
     :comp {:synsem {:sem comp-semantics}}}))

;; -- END SCHEMA DEFINITIONS

(defn sentence-impl [input]
  "do things necessary before something can be a sentence. e.g. if infl is still :top, set to
:present (later, set to a randomly selected member of {:finite, :futuro, ..}."
  (do
    (if (not (fail? input))
      (do
        (log/debug (str "grammar: sentence-impl input: " (fo input)))
        (log/debug (str "grammar: sentence-impl type: " (type input)))))
    (cond
     (seq? input)
     (map (fn [each]
            (sentence-impl each))
          input)
     (= input :top) input
     true
     (let [finitize
           (cond (or (= (get-in input '(:synsem :infl))
                        :top)
                     (= (get-in input '(:synsem :infl))
                        :infinitive))

                 (first (take 1 (shuffle
                                 (list
                                  {:synsem {:infl :futuro}}
                                  {:synsem {:infl :imperfetto}}
                                  {:synsem {:infl :present}}
                                  ))))
                 ;; special additional case for 'potere' exclude :imperfetto.
                 (= (get-in input '(:synsem :infl :not))
                    :imperfetto)
                 (first (take 1 (shuffle
                                 (list
                                  {:synsem {:infl :futuro}}
                                  {:synsem {:infl :present}})))))]
       (let [merged
             (if (= input :fail) :fail
                 (merge input finitize))]
         (do
           (if (not (fail? merged))
             (log/debug (str "sentence-impl: merged result IS:::" (fo merged))))
           merged)))))) ;; for now, no recursive call.

(defn sent-impl [input]
  "shortcut"
  (sentence-impl input))

(log/info "Universal Grammar Immediate Dominance schemata are defined in our environment.")

