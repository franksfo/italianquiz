(ns italianverbs.ug
  (:refer-clojure :exclude [get-in resolve])
  (:use [clojure.set :only (union intersection)]
        [clojure.core :exclude (get-in resolve merge)]
        [italianverbs.generate :only (generate moreover-head moreover-comp gen14 gen15 gen17 lazy-shuffle)]
        [italianverbs.lexicon :only (it1)]
        [italianverbs.lexiconfn :only (unify sem-impl)]
        [italianverbs.morphology :only (finalize fo italian-article get-italian-1 get-italian)]
        [italianverbs.unify :only (copy fail? serialize get-in)]
        )

  (:require [clojure.tools.logging :as log]
            [italianverbs.lexicon :as lex]
            [italianverbs.unify :as unify]
            [clojure.string :as string])
)

(def phrase-times-lexicon-cache true)
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
        head-sem (ref :top)]
    {:synsem {:cat head-cat
              :essere head-essere
              :pronoun head-is-pronoun
              :sem head-sem}
     :head {:synsem {:cat head-cat
                     :essere head-essere
                     :pronoun head-is-pronoun
                     :sem head-sem}}}))

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
  (let [comp-synsem (ref {:subcat '()})]
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
    {:synsem {:subcat {:1 parent-subcat}}
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
                       :2 subcat-3}}
     :head {:synsem {:subcat {:1 subcat-1
                              :2 subcat-2}}}
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
  (fn [phrase-with-head]
    (let [debug (log/debug (str "standard-filter-fn IS BEING CREATED NOW WITH phrase-with-head: " (fo phrase-with-head)))
          complement-synsem (unify/get-in phrase-with-head '(:comp :synsem) :top)
          complement-category (unify/get-in complement-synsem '(:cat) :top)
          complement-sem (sem-impl (unify/get-in complement-synsem '(:sem) :top))
          complement-essere-type (unify/get-in complement-synsem '(:essere) :top)
          complement-italian-initial (unify/get-in phrase-with-head '(:comp :italian :initial) :top)


          debug (log/debug (str "cond1: " (not (fail? (unify (unify/get-in comp '(:synsem :cat) :top)
                                                            complement-category)))))
          debug (log/debug (str "cond2: " (not (fail? (unify (unify/get-in comp '(:synsem :sem) :top)
                                                            complement-sem)))))
          debug (log/debug (str "cond3: " (not (fail? (unify (unify/get-in comp '(:synsem :essere) :top)
                                                            complement-essere-type)))))
          debug (log/debug (str "cond4: " (not (fail? (unify (unify/get-in comp '(:italian :initial) :top)
                                                            complement-italian-initial)))))]


      (fn [comp]

        (let [debug1 (log/debug (str "cond1: " (not (fail? (unify (unify/get-in comp '(:synsem :cat) :top)
                                                                complement-category)))))
              debug2 (log/debug (str "cond2: " (not (fail? (unify (unify/get-in comp '(:synsem :sem) :top)
                                                                 complement-sem)))))
              debug2p5 (log/debug (str "complement-sem: " (unify/get-in comp '(:synsem :sem) :top)))
              debug2p5 (log/debug (str "complement-sem (expected): " complement-sem))
              debug3 (log/debug (str "cond3: " (not (fail? (unify (unify/get-in comp '(:synsem :essere) :top)
                                                                 complement-essere-type)))))
              debug4 (log/debug (str "cond4: " (not (fail? (unify (unify/get-in comp '(:italian :initial) :top)
                                                                 complement-italian-initial)))))
              result
              (and
               (not (fail? (unify (unify/get-in comp '(:synsem :cat) :top)
                                  complement-category)))
               (not (fail? (unify (sem-impl (unify/get-in comp '(:synsem :sem) :top))
                                  (sem-impl complement-sem))))
               (not (fail? (unify (unify/get-in comp '(:synsem :essere) :top)
                                  complement-essere-type)))
               (not (fail? (unify (unify/get-in comp '(:italian :initial) :top)
                                  complement-italian-initial))))]

          (log/debug (str "comp-filter-fn:phrase-with-head:" (fo phrase-with-head)))
          (log/debug (str "comp-filter-fn:phrase-with-head's first arg" (unify/get-in phrase-with-head '(:head :synsem :subcat :1) :wtf)))
          (log/debug (str "comp-filter-fn:type(phrase-with-head):" (type phrase-with-head)))
          (log/debug (str "comp-filter-fn:complement:" (fo comp)))
          (log/debug (str "comp-filter-fn:complement-synsem (from head): " complement-synsem))
          (log/debug (str "comp-filter-fn:complement-category (from head): " complement-category))
          (log/debug (str "comp-filter-fn:complement-sem: " complement-sem))
          (log/debug (str "comp-filter-fn:complement's italian initial must be: " complement-italian-initial))


          (let [fail-path-result ""] ;; TODO: show fail diagnostics.
            (if (= \c (nth (get-in phrase-with-head '(:comment)) 0))
              (log/debug (str "comp-filter-fn:RESULT OF FILTER: " (fo comp) " + " (fo phrase-with-head) " = " result
                             fail-path-result))
              ;; else, head is first
              (log/debug (str "comp-filter-fn:RESULT OF FILTER: "  (fo phrase-with-head) " + " (fo comp) " = " result
                             fail-path-result))))

          (if result
            ;; complement was compatible with the filter: not filtered out.
            (do ;(log/debug (str "FILTER IN: standard-filter-fn: complement-synsem category:" complement-category))
                ;(log/debug (str "FILTER IN: standard-filter-fn: complement-synsem sem:" complement-sem))
                (log/debug (str "FILTER IN: head: " (fo phrase-with-head) " filtering comp: " (fo comp) " => "
                               "TRUE" ;; emphasize for ease of readability in logs.
                               ))
                result)

            ;; complement was incompatible with the filter and thus filtered out:
            (do
              (log/debug (str "FILTER OUT: " (:comment comp) " : " (fo comp) " against phrase-with-head: " (:comment phrase-with-head) " :" (fo phrase-with-head)))
              result)))))))

(def hc-agreement
  (let [agr (ref :top)]
    {:synsem {:agr agr}
     :head {:synsem {:agr agr}}
     :comp {:italian {:agr agr}
            :english {:agr agr}
            :synsem {:agr agr}}}))

(def comp-modifies-head
  (let [head-semantics (ref :top)]
    {:head {:synsem {:sem head-semantics}}
     :comp {:synsem {:sem {:mod head-semantics}}}}))

;; -- BEGIN SCHEMA DEFINITIONS

(def cc10
  (unify
   subcat-1-principle
   head-principle
   italian-head-last
   english-head-last
   {:comment "cc10"
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
   {:comment "hc11"}))

(def hh10
  (unify
   subcat-1-principle
   head-principle
   italian-head-first
   english-head-first
   {:comment "hh10"
    :comp {:synsem {:subcat '()}}
    :comp-filter-fn standard-filter-fn}))

(def hh21
  (unify
   subcat-2-principle
   head-principle
   italian-head-first
   english-head-first
   {:comment "hh21"
    :comp-filter-fn standard-filter-fn}))

;; -- END SCHEMA DEFINITIONS

;; standard rule-caching disclaimer:
;; "this is computed when it's needed. first usage is very expensive. TODO: make first usage less expensive."
(def ch21-heads
  (if phrase-times-lexicon-cache
    (filter (fn [lex]
              (not (fail? (unify ch21 {:head lex}))))
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
                                               (first heads))
                                candidate-comp)))
     (find-some-head-for parent (rest heads) candidate-comp))))

;; standard rule-caching disclaimer:
;; "this is computed when it's needed. first usage is very expensive. TODO: make first usage less expensive."
(def ch21-comps
  (if phrase-times-lexicon-cache
    (filter (fn [lex]
              (find-some-head-for ch21 ch21-heads lex))
            (filter (fn [lex]
                      (not (fail? (unify ch21 {:comp lex}))))
                    lex/lexicon))
    lex/lexicon))

;; standard rule-caching disclaimer:
;; "this is computed when it's needed. first usage is very expensive. TODO: make first usage less expensive."
(def hh21-heads
  (if true
    (filter (fn [lex]
              (not (fail? (unify hh21 {:head lex}))))
            lex/lexicon)
    lex/lexicon))

(def hh10-heads
  (if true
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

(defn base-ch21 []
  (gen15 ch21 ch21-heads ch21-comps))

(defn base-cc10 [use-filter]
  (gen15 cc10
         (filter use-filter
                 cc10-heads)
         cc10-comps))

(defn base-cc10-random-nofilter []
  (gen15 cc10
         (shuffle cc10-heads)
         (shuffle cc10-comps)))

(defn base-cc10-random [use-filter]
  (do
    (log/debug "base-cc10-random: start: filtering cc10 heads.")
    (gen15 cc10
           (filter use-filter
                   (lazy-shuffle cc10-heads))
           (lazy-shuffle cc10-comps))))

(log/info "done.")
