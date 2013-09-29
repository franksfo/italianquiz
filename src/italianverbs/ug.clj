(ns italianverbs.ug
  (:refer-clojure :exclude [get-in resolve])
  (:use [clojure.set :only (union intersection)]
        [clojure.core :exclude (get-in resolve merge)]
        [italianverbs.generate :only (generate moreover-head moreover-comp gen14 lazy-shuffle)]
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
    (let [complement-synsem (unify/get-in phrase-with-head '(:comp :synsem) :top)
          complement-category (unify/get-in complement-synsem '(:cat) :top)
          complement-sem (sem-impl (unify/get-in complement-synsem '(:sem) :top))]

      (fn [comp]
        (let [result
              (and
               (not (fail? (unify (unify/get-in comp '(:synsem :cat) :top)
                                  complement-category)))
               (not (fail? (unify (unify/get-in comp '(:synsem :sem) :top)
                                  complement-sem))))]
          (log/debug (str "comp-filter-fn:phrase-with-head:" (fo phrase-with-head)))
          (log/debug (str "comp-filter-fn:phrase-with-head's first arg" (unify/get-in phrase-with-head '(:head :synsem :subcat :1) :wtf)))
          (log/debug (str "comp-filter-fn:type(phrase-with-head):" (type phrase-with-head)))
          (log/debug (str "comp-filter-fn:complement:" (fo comp)))
          (log/debug (str "comp-filter-fn:complement-synsem (from head): " complement-synsem))
          (log/debug (str "comp-filter-fn:complement-category (from head): " complement-category))
          (log/debug (str "comp-filter-fn:complement-sem: " complement-sem))
          (log/debug (str "comp-filter-fn:result of filter: " (fo phrase-with-head) " + " (fo comp) " = " result))

          (if result
            (log/debug (str "head: " (fo phrase-with-head) " filtering comp: " (fo comp) " => "
                            (if result
                              "TRUE" ;; emphasize for ease of readability in logs.
                              result))))
          result)))))

(def cc10
  (unify
   subcat-1-principle
   head-principle
   italian-head-last
   english-head-last
   {:comment "cc10"
    :comp-filter-fn standard-filter-fn}))

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
     (let [finitize (if (or (= (unify/get-in input '(:synsem :infl))
                               :top)
                            (= (unify/get-in input '(:synsem :infl))
                               :infinitive))
                      (first (take 1 (shuffle
                                      (list
                                            {:synsem {:infl :futuro}}
                                            {:synsem {:infl :imperfetto}}
                                            {:synsem {:infl :present}}
                                            )))))]
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
     (not (fail? (sent-impl (moreover-comp (moreover-head parent
                                                                     (first heads))
                                                  candidate-comp))))
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
   italian-head-last
   english-head-last
   {:comment "hh10"}))

(def hh21
  (unify
   subcat-2-principle
   head-principle
   italian-head-first
   english-head-first
   {:comment "hh21"
    :comp-filter-fn standard-filter-fn}))

;; standard rule-caching disclaimer:
;; "this is computed when it's needed. first usage is very expensive. TODO: make first usage less expensive."
(def hh21-heads
  (if false
    (filter (fn [lex]
              (not (fail? (unify hh21 {:head lex}))))
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
                      (and (or true (= (unify/get-in lex '(:italian)) "la")
                               (= (unify/get-in lex '(:italian)) "il"))
                           (not (fail? (unify cc10 {:comp lex})))))
                    lex/lexicon))
    lex/lexicon))

(if phrase-times-lexicon-cache
  (do
    (log/debug (str "ch21-heads: " (.size ch21-heads)))
    (log/debug (str "ch21-comps: " (.size ch21-comps)))
    (log/debug (str "cc10-heads:" (.size cc10-heads)))
    (log/debug (str "cc10-comps:" (.size cc10-comps)))))

(defn gen15 [phrase heads comps]
  (do
    (log/debug (str "gen15 start: " (get-in phrase '(:comment)) "," (type heads) "," (type comps)))
    (gen14 phrase heads comps sent-impl 0)))

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

(log/info "compiling gen-ch21..")
(defmacro gen-ch21 [head comp]
  `(do ~(log/info "gen-ch21 macro compile-time.")
       (gen15 ch21
              ~head
              ~comp)))

(log/info "compiling gen-hh21..")
(defmacro gen-hh21 [head comp]
  `(do ~(log/info "gen-hh21 macro compile-time.")
       (gen15 hh21
              ~head
              ~comp)))

(log/info "compiling gen-cc10..")
(defmacro gen-cc10 [head comp]
  `(do ~(log/info "gen-cc10 macro compile-time.")
       (gen15 cc10
              ~head
              ~comp)))

(defn gen-all [label alternatives & [filter-against filter-fn]]
  (if (first alternatives)
    (let [first-alt (first alternatives)]
      (log/info (str "gen-all: " label "; candidate: "
                     (cond (map? first-alt)
                           (fo first-alt)
                           true
                           first-alt)))
      (let [filter-fn (if filter-fn
                        filter-fn
                        (if filter-against
                          ;; create a function using the filter-against we were given.
                          (fn [x]
                            (let [debug (log/debug (str "filtering: " x))
                                  debug (log/debug (str "against: " filter-against))
                                  result (unify x filter-against)
                                  debug (log/debug (str "result: " result))]
                              (not (unify/fail? result))))

                          ;; no filter was desired by the caller: just use the pass-through filter.
                          (fn [x] true)))]
        (lazy-cat
         (let [lazy-returned-sequence
               (cond (symbol? first-alt)
                     (lazy-shuffle
                      (filter filter-fn (eval first-alt)))

                     (and (map? first-alt)
                          (not (nil? (:schema first-alt))))
                     (let [schema (:schema first-alt)
                           head (:head first-alt)
                           comp (:comp first-alt)]
                       (log/info (str "schema: " schema))
                       (gen15 (eval schema)
                              (filter filter-fn
                                      (gen-all (str schema " -> " head " (H)")
                                               (lazy-shuffle (eval head))))
                              (gen-all (str schema " -> " comp " (C)")
                                       (if (symbol? comp) (lazy-shuffle (eval comp)) (lazy-shuffle comp)))))

                     (map? first-alt)
                     (list first-alt)

                     true (throw (Exception. "don't know what to do with this; type=" (type first-alt))))]
           lazy-returned-sequence)
         (gen-all label (rest alternatives) filter-against filter-fn))))))

(log/info "done.")
