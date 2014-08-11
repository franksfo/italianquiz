(ns italianverbs.grammar.italiano
  (:refer-clojure :exclude [get-in merge resolve])
  (:require 
   [clojure.set :only (union intersection)]
   [clojure.tools.logging :as log]
   [italianverbs.cache :refer (build-lex-sch-cache over spec-to-phrases get-comp-phrases-of)]
   [italianverbs.forest :as forest :exclude [generate]]
   [italianverbs.lexicon :refer :all]
   [italianverbs.morphology :refer :all]
   [italianverbs.ug :refer :all]
   [italianverbs.unify :refer :all]))

(def head-first
  (let [head-italian (ref :top)
        comp-italian (ref :top)]
    (unify
     {:comp {:italian {:initial false}}
      :head {:italian {:initial true}}}
     {:head {:italian head-italian}
      :comp {:italian comp-italian}
      :italian {:a head-italian
                :b comp-italian}})))

(def head-last
  (let [head-italian (ref :top)
        comp-italian (ref :top)]
    (unify
     {:comp {:italian {:initial true}}
      :head {:italian {:initial false}}}
     {:head {:italian head-italian}
      :comp {:italian comp-italian}
      :italian {:a comp-italian
                :b head-italian}})))

;; -- BEGIN SCHEMA DEFINITIONS
(def schema-10
  (unify
   subcat-1-principle
   head-principle
   {:comment "cc10"
    :first :comp
    :comp {:synsem {:subcat '()}}}))

(def cc10
  (unify
   schema-10
   head-last
   {:comment "cc10"
    ;; TODO: using :schema-symbol below - cannot use :schema for some reason; need to figure out why.
    ;; if you try to use :schema, I get:
    ;; java.util.concurrent.ExecutionException: java.lang.RuntimeException:
    ;; Can't embed object in code, maybe print-dup not defined: clojure.lang.Ref@11819f3c
    :schema-symbol 'cc10 ;; used by over-each-parent to know where to put children.
    :first :comp
    :comp {:synsem {:subcat '()}}}))

(def ch21
  (unify
   subcat-2-principle
   head-principle
   head-last
   {:comp {:synsem {:subcat '()
                    :pronoun true}}
    :schema-symbol 'ch21 ;; used by over-each-parent to know where to put children.
    :first :comp
    :comment "ch21"}))

(def hc11
  (unify
   subcat-1-1-principle
   hc-agreement
   head-principle
   comp-modifies-head
   head-first
   {
    :schema-symbol 'hc11 ;; used by over-each-parent to know where to put children.
    :first :head
    :comment "hc11"}))


(def hc11-comp-subcat-1
  (let [subcat (ref :top)]
    (unify
     {:head {:synsem {:subcat {:1 subcat}}}
      :comp {:synsem {:subcat {:1 subcat}}}}
     subcat-1-1-principle-comp-subcat-1
     hc-agreement
     head-principle
     comp-modifies-head
     head-first
     {:schema-symbol 'hc11-comp-subcat-1
      :first :head
      :comment "hc11-comp-subcat-1"})))

(def hh10
  (unify
   subcat-1-principle
   head-principle
   head-first
   {:comment "hh10"
    :schema-symbol 'hh10 ;; used by over-each-parent to know where to put children.
    :first :head}))

(def hh21
  (unify
   subcat-2-principle
   head-principle
   head-first
   {:comment "hh21"
    :schema-symbol 'hh21 ;; used by over-each-parent to know where to put children.
    :first :head}))

(def hh22
  (unify
   subcat-2-2-principle
   head-principle
   head-first
   {:comment "hh22"
    :schema-symbol 'hh22 ;; used by over-each-parent to know where to put children.
    :first :head}))

(def hh32
  (unify
   subcat-5-principle
   head-principle
   head-first
   {:comment "hh32"
    :schema-symbol 'hh32 ;; used by over-each-parent to know where to put children.
    :first :head}))

;; -- END SCHEMA DEFINITIONS

(def grammar (list (unifyc hh21
                           {:rule "adjective-phrase"
                            :synsem {:cat :adjective}})

                   (unifyc hh21
                           (let [head-synsem {:cat :intensifier
                                              :modified true}] ;; TODO: document what purpose :modified serves (if any: if none, remove).
                             {:rule "intensifier-phrase"
                              :synsem head-synsem}))

                   (unifyc hc11-comp-subcat-1
                           (let [head-synsem {:cat :noun
                                              :modified true}]
                             {:comp {:phrasal false ;; rathole prevention ;; TODO: see if this can be removed.
                                     :synsem {:cat :adjective
                                              :mod head-synsem}}
                              :head {:phrasal false
                                     :synsem {:modified false}} ;; TODO: document what purpose :modified serves (if any: if none, remove).
                              :rule "nbar"
                              :synsem head-synsem}))

                   (unifyc cc10
                           (let [number-agreement (ref :top)]
                             {:rule "noun-phrase"
                              :aliases (list "np")
                              :synsem {:agr {:number number-agreement}
                                       :cat :noun
                                       :sem {:number number-agreement}}
                              :comp {:phrasal false}})) ;; rathole prevention ;; TODO: see if this can be removed.

                   (unifyc hh10
                           {:rule "prepositional-phrase"
                            :synsem {:cat :prep}})

                   (unifyc cc10
                           {:head {:synsem {:aux true}}
                            :rule "s-aux"
                            :synsem {:infl :present
                                     :cat :verb
                                     :sem {:tense :past}}})

                   (unifyc cc10
                          {:rule "s-future"
                           :synsem {:aux false
                                    :infl :futuro
                                    :cat :verb
                                    :sem {:tense :futuro}}})


                   (unifyc cc10
                           {:rule "s-imperfetto"
                            :synsem {:aux false
                                     :infl :imperfetto
                                     :cat :verb
                                     :sem {:tense :past}}})

                   (unifyc cc10
                           {:rule "s-present"
                            :synsem {:aux false
                                     :infl :present
                                     :cat :verb
                                     :sem {:tense :present}}})

                   (unifyc hh21
                           {:rule "vp-infinitive"
                            :synsem {:aux false
                                     :infl :infinitive
                                     :cat :verb}})

                   (unifyc hh21
                           {:rule "vp-aux"
                            :head {:phrasal false}
                            :synsem {:aux true
                                     :infl :present
                                     :sem {:tense :past}
                                     :cat :verb}})

                   ;; this rule is kind of complicated and made more so by
                   ;; dependence on auxilary sense of "avere" which supplies the
                   ;; obj-agr agreement between the object and the main (non-auxilary) verb.
                   (unifyc hh22
                           (let [obj-agr (ref :top)]
                             {:head {:phrasal false}
                              :rule "vp-aux-22"
                              :synsem {:aux true
                                       :cat :verb
                                       :infl :present
                                       :sem {:tense :past}
                                       :subcat {:2 {:agr obj-agr}}}
                              :italian {:b {:obj-agr obj-agr}}}))

                   (unifyc hh21
                           {:rule "vp-future"
                            :synsem {:aux false
                                     :infl :futuro
                                     :cat :verb}})

                   (unifyc hh21
                          {:rule "vp-imperfetto"
                           :synsem {:aux false
                                    :infl :imperfetto
                                    :cat :verb}})

                   (unifyc hh21
                           {:rule "vp-past"
                            :synsem {:aux false
                                     :infl :past
                                     :cat :verb}})

                   (unifyc hh21
                           {:rule "vp-present"
                            :synsem {:aux false
                                     :infl :present
                                     :sem {:tense :present}
                                     :cat :verb}})

                   (unifyc ch21
                           {:comp {:phrasal false
                                   :synsem {:cat :noun
                                            :pronoun true}}
                            :rule "vp-pronoun"
                            :synsem {:cat :verb
                                     :infl {:not :past}}})

                   (unifyc hh10
                           {:head {:phrasal false
                                   :synsem {:cat :sent-modifier}}
                            :rule "s-modifier"})

))


(defn aux-is-head-feature [phrase]
  (cond (= :verb (get-in phrase '(:synsem :cat)))
        (unifyc phrase
                (let [ref (ref :top)]
                  {:synsem {:aux ref}
                   :head {:synsem {:aux ref}}}))
        true phrase))

(defn modal-is-head-feature [phrase]
  (cond (= :verb (get-in phrase '(:synsem :cat)))
        (unifyc phrase
                (let [ref (ref :top)]
                  {:synsem {:modal ref}
                   :head {:synsem {:modal ref}}}))
        true phrase))


(def grammar
  (map (fn [phrase]
         (modal-is-head-feature
          (aux-is-head-feature phrase)))
       grammar))

;; These two internings allows us to refer to individual grammar rules within grammar
;; by symbols like "vp-present" (e.g. (over vp-present lexicon)).
;; TODO: not sure if aliases are working yet.
(.size (map (fn [rule]
              (do
                (log/debug (str "Looking for aliases for rule: " (fo-ps rule)))
                (.size (map (fn [alias]
                              (do
                                (log/debug (str "rule alias: " alias " -> " (fo-ps rule)))
                                (intern *ns* (symbol alias) rule)))
                            (:aliases rule)))))
            grammar))

;; TODO: calling (.size) because (map) is lazy, and I want to realize
;; the sequence - must be a better way to loop over the grammar and realize the result.
(.size (map (fn [rule]
       (intern *ns* (symbol (:rule rule)) rule))
     grammar))

(log/info "Italian grammar defined.")

(def begin (System/currentTimeMillis))
(log/debug "building grammatical and lexical cache..")
(def cache nil)
;; TODO: trying to print cache takes forever and blows up emacs buffer:
;; figure out how to change printable version to (keys cache).
(def cache (conj (build-lex-sch-cache grammar
                                      (map (fn [lexeme]
                                             (unifyc lexeme
                                                     {:phrasal false}))
                                           lexicon)
                                      grammar)
                 {:phrase-constraints head-principle ;; for now, only one constraint: ug/head-principle.
                  :phrases-for-spec
                  (spec-to-phrases
                   ;; TODO: make this list derivable from the grammar and /or lexicon.
                   (list {:synsem {}, :head {:synsem {}}, :phrasal true}
                         {:synsem {:cat :verb, :aux false}, :head {:synsem {:subcat {:2 {}, :1 {}}, :infl :present, :cat :verb, :sem {:tense :present}}, :phrasal false}, :phrasal true}
                         {:synsem {:cat :verb}, :head {:synsem {:cat :verb, :infl {:not :past}, :subcat {:2 {:cat :noun, :subcat (), :pronoun true}, :1 {}}}, :phrasal false}, :phrasal true}
                         {:synsem {:cat :verb, :aux false}, :head {:synsem {:cat :verb, :infl :infinitive, :subcat {:2 {}, :1 {}}}, :phrasal false}, :phrasal true}
                         )
                   grammar)}))

(def end (System/currentTimeMillis))
(log/info "Built grammatical and lexical cache in " (- end begin) " msec.")
