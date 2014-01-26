(ns italianverbs.ug
  (:refer-clojure :exclude [get-in resolve])
  (:use [clojure.set :only (union intersection)]
        [clojure.core :exclude (get-in resolve merge)]
        [italianverbs.lexicon :only (it)]
        [italianverbs.lexiconfn :only (unify sem-impl)]
        [italianverbs.morphology :only (finalize fo italian-article get-italian-1 get-italian)]
        [italianverbs.over :only (moreover-head moreover-comp)]
        [italianverbs.unify :only (copy fail? serialize get-in fail-path lazy-shuffle)])

  (:require [clojure.tools.logging :as log]
            [italianverbs.lexicon :as lex]
            [italianverbs.unify :as unify]
            [clojure.string :as string]))

(def phrase-times-lexicon-cache false)
;; ^^ true: pre-compute cross product of phrases X lexicon (slow startup, fast runtime)
;;    false: don't pre-compute product (fast startup, slow runtime)

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
    {:synsem {:cat head-cat
              :essere head-essere
              :pronoun head-is-pronoun
              :sem head-sem
              :sem-mod sem-mod}
     :head {:synsem {:cat head-cat
                     :essere head-essere
                     :pronoun head-is-pronoun
                     :sem head-sem
                     :sem-mod sem-mod}}}))

;;    [1]
;;   /   \
;;  /     \
;; H[1]    C
(def head-principle
  (unify head-principle-no-infl
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

;; a language's morphological inflection is
;; identical to its head's SYNSEM|INFL value.
(def verb-inflection-morphology
  (let [essere (ref :top)
        infl (ref :top)
        cat (ref :verb)]
    {:italian {:a {:infl infl
                   :cat cat}}
     :english {:a {:infl infl
                   :cat cat}}
     :synsem {:infl infl
              :essere essere}
     :head {:italian {:infl infl
                      :cat cat}
            :english {:infl infl
                      :cat cat}
            :synsem {:cat cat
                     :essere essere
                     :infl infl}}}))

(def italian-head-first
  (let [head-italian (ref :top)
        comp-italian (ref :top)]
    (unify
     {:comp {:italian {:initial false}}
      :head {:italian {:initial true}}}
     {:head {:italian head-italian}
      :comp {:italian comp-italian}
      :italian {:a head-italian
                :b comp-italian}})))

(def italian-head-last
  (let [head-italian (ref :top)
        comp-italian (ref :top)]
    (unify
     {:comp {:italian {:initial true}}
      :head {:italian {:initial false}}}
     {:head {:italian head-italian}
      :comp {:italian comp-italian}
      :italian {:a comp-italian
                :b head-italian}})))

(def english-head-first
  (let [head-english (ref :top)
        comp-english (ref :top)]
    {:head {:english head-english}
     :comp {:english comp-english}
     :english {:a head-english
               :b comp-english}}))

(def english-head-last
  (let [head-english (ref :top)
        comp-english (ref :top)]
    {:head {:english head-english}
     :comp {:english comp-english}
     :english {:a comp-english
               :b head-english}}))

(def standard-filter-fn
  (fn [additional-phrase-with-head]
    (fn [phrase-with-head]
      (let [phrase-with-head (unify additional-phrase-with-head
                                    phrase-with-head)]
        (do
          (if (fail? phrase-with-head)
            (throw (Exception. (str "phrase-with-head is fail: " phrase-with-head))))
          (fn [comp]
            (let [result
                  {:essere
                   (unify (unify/get-in phrase-with-head '(:comp :synsem :essere) :top)
                          (unify/get-in comp '(:synsem :essere) :top))
                   :agr
                   (unify (unify/get-in phrase-with-head '(:comp :synsem :agr) :top)
                          (unify/get-in comp '(:synsem :agr) :top))
                   :sem
                   (unify (sem-impl (unify/get-in phrase-with-head '(:comp :synsem :sem) :top))
                          (sem-impl (unify/get-in comp '(:synsem :sem) :top)))}]
              (if (not (fail? result))
                ;; complement was compatible with the filter: not filtered out.
                true
                ;; complement was incompatible with the filter and thus filtered out:
                false))))))))

(def hc-agreement
  (let [agr (ref :top)]
    {:synsem {:agr agr}
     :head {:synsem {:agr agr}}
     :comp {:italian {:agr agr}
            :english {:agr agr}
            :synsem {:agr agr}}}))

(def comp-modifies-head
  (let [head-semantics (ref :top)
        pred-of-mod (ref :top)
        mod-semantics {:pred pred-of-mod
                       :mod head-semantics}]
    {:synsem {:sem-mod {:pred pred-of-mod
                        :obj head-semantics}}
     :head {:synsem {:sem head-semantics}}
     :comp {:synsem {:sem mod-semantics}}}))

;; -- BEGIN SCHEMA DEFINITIONS

(def cc10
  (unify
   subcat-1-principle
   head-principle
   italian-head-last
   english-head-last
   {:comment "cc10"

    ;; TODO: using :schema-symbol below - cannot use :schema for some reason; need to figure out why.
    ;; if you try to use :schema, I get:
    ;; java.util.concurrent.ExecutionException: java.lang.RuntimeException:
    ;; Can't embed object in code, maybe print-dup not defined: clojure.lang.Ref@11819f3c

    :schema-symbol 'cc10 ;; used by over-each-parent to know where to put children.
    :first :comp
    :comp {:synsem {:subcat '()}}
    :comp-filter-fn standard-filter-fn}))

(def ch21
  (unify
   subcat-2-principle
   head-principle
   italian-head-last
   english-head-first
   {:comp {:synsem {:subcat '()
                    :pronoun true}}
    :schema-symbol 'ch21 ;; used by over-each-parent to know where to put children.
    :first :comp
    :comment "ch21"
    :comp-filter-fn standard-filter-fn}))

(def hc11
  (unify
   subcat-1-1-principle
   hc-agreement
   head-principle
   comp-modifies-head
   italian-head-first
   english-head-last
   {
    :schema-symbol 'hc11 ;; used by over-each-parent to know where to put children.
    :first :head
    :comment "hc11"
    :comp-filter-fn standard-filter-fn}))

(def hh10
  (unify
   subcat-1-principle
   head-principle
   italian-head-first
   english-head-first
   {:comment "hh10"
    :schema-symbol 'hh10 ;; used by over-each-parent to know where to put children.
    :first :head
    :comp-filter-fn standard-filter-fn}))

(def hh21
  (unify
   subcat-2-principle
   head-principle
   italian-head-first
   english-head-first
   {:comment "hh21"
    :schema-symbol 'hh21 ;; used by over-each-parent to know where to put children.
    :first :head
    :comp-filter-fn standard-filter-fn}))

(def hh32
  (unify
   subcat-5-principle
   head-principle
   italian-head-first
   english-head-first
   {:comment "hh32"
    :schema-symbol 'hh32 ;; used by over-each-parent to know where to put children.
    :first :head
    :comp-filter-fn standard-filter-fn}))

;; -- END SCHEMA DEFINITIONS

;; standard rule-caching disclaimer:
;; "this is computed when it's needed. first usage is very expensive. TODO: make first usage less expensive."
(def ch21-heads
  (if phrase-times-lexicon-cache
    (lazy-seq (filter (fn [lex]
                        (not (fail? (unify ch21 {:head lex}))))
                      lex/lexicon)
              lex/lexicon)))

(def hc11-comps
  (if phrase-times-lexicon-cache
    (lazy-seq (filter (fn [lex]
                        (not (fail? (unify hc11 {:comp lex}))))
                      lex/lexicon)
              lex/lexicon)
    lex/lexicon))

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
           (cond (or (= (unify/get-in input '(:synsem :infl))
                        :top)
                     (= (unify/get-in input '(:synsem :infl))
                        :infinitive))

                 (first (take 1 (shuffle
                                 (list
                                  {:synsem {:infl :futuro}}
                                  {:synsem {:infl :imperfetto}}
                                  {:synsem {:infl :present}}
                                  ))))
                 ;; special additional case for 'potere' exclude :imperfetto.
                 (= (unify/get-in input '(:synsem :infl :not))
                    :imperfetto)
                 (first (take 1 (shuffle
                                 (list
                                  {:synsem {:infl :futuro}}
                                  {:synsem {:infl :present}})))))]
       (let [merged
             (if (= input :fail) :fail
                 (unify/merge input finitize))]
         (do
           (if (not (fail? merged))
             (log/debug (str "sentence-impl: merged result IS:::" (fo merged))))
           merged)))))) ;; for now, no recursive call.

(defn sent-impl [input]
  "shortcut"
  (sentence-impl input))

(defn find-some-head-for [parent heads candidate-comp]
  "returns true iff there is some head H such that parent => H candidate-comp succeeds."
  (if (not (empty? heads))
    (or
     (not (fail? (moreover-comp (moreover-head parent
                                               (first heads)
                                               sem-impl)
                                candidate-comp
                                sem-impl)))
     (find-some-head-for parent (rest heads) candidate-comp))))

;; standard rule-caching disclaimer:
;; "this is computed when it's needed. first usage is very expensive. TODO: make first usage less expensive."
(def ch21-comps
  (if phrase-times-lexicon-cache
    (lazy-seq
     (filter (fn [lex]
               (find-some-head-for ch21 ch21-heads lex))
             (filter (fn [lex]
                       (not (fail? (unify ch21 {:comp lex}))))
                     lex/lexicon))
     lex/lexicon)))

;; standard rule-caching disclaimer:
;; "this is computed when it's needed. first usage is very expensive. TODO: make first usage less expensive."
(def hh21-heads
  (lazy-seq
   (filter (fn [lex]
             (not (fail? (unify hh21 {:head lex}))))
           lex/lexicon)
   lex/lexicon))

(def all-in-lexicon lex/lexicon)

(def hh10-heads
  (lazy-seq
    (filter (fn [lex]
              (not (fail? (unify hh10 {:head lex}))))
            lex/lexicon)
    lex/lexicon))

(def subject-verb-agreement
  (let [infl (ref :top)
        agr (ref {:case :nom})]
    {:comp {:synsem {:agr agr}}
     :head {:synsem {:subcat {:1 {:agr agr}}
                     :infl infl}
            :italian {:agr agr
                      :infl infl}
            :english {:agr agr
                      :infl infl}}}))

;; standard rule-caching disclaimer:
;; "this is computed when it's needed. first usage is very expensive. TODO: make first usage less expensive."
(def cc10-heads
  (if phrase-times-lexicon-cache
    (filter (fn [lex]
              (and true ;(= (unify/get-in lex '(:italian :italian)) "acqua")
                   (not (fail? (unify cc10 {:head lex})))))
            lex/lexicon)
    lex/lexicon))

;; standard rule-caching disclaimer:
;; "this is computed when it's needed. first usage is very expensive. TODO: make first usage less expensive."
(def cc10-comps
  (if phrase-times-lexicon-cache
    (filter (fn [lex]
              (find-some-head-for cc10 cc10-heads lex))
            (filter (fn [lex]
                      (not (fail? (unify cc10 {:comp lex}))))
                    lex/lexicon))
    lex/lexicon))

(if phrase-times-lexicon-cache
  (do
    (log/info "pre-compiling phrase-lex caches because phrase-times-lexicon-cache is true..")
    (log/info (str "ch21-heads.."))
    (log/info (str "ch21-heads: " (.size ch21-heads)))
    (log/info (str "ch21-comps.."))
    (log/info (str "ch21-comps: " (.size ch21-comps)))
    (log/info (str "ch10-heads.."))
    (log/info (str "cc10-heads: " (.size cc10-heads)))
    (log/info (str "ch10-comps.."))
    (log/info (str "cc10-comps: " (.size cc10-comps)))
    (log/info "done pre-compiling phrase-lex caches.")))

(log/info "done.")
