(ns italianverbs.grammar.italiano
  (:refer-clojure :exclude [get-in merge resolve])
  (:require 
   [clojure.tools.logging :as log]
   [italianverbs.cache :refer (build-lex-sch-cache create-index over spec-to-phrases)]
   [italianverbs.generate :as gen]
   [italianverbs.lexicon :refer :all]
   [italianverbs.ug :refer :all]
   [italianverbs.unify :refer (get-in unifyc)]))

(declare cache)
(defn generate [spec grammar]
  (gen/generate spec
                grammar
                lexicon
                cache))

(def hc-agreement
  (let [agr (ref :top)]
    {:synsem {:agr agr}
     :head {:synsem {:agr agr}}
     :comp {:italiano {:agr agr}
            :synsem {:agr agr}}}))

(def head-first
  (let [head-italian (ref :top)
        comp-italian (ref :top)]
    (unifyc
     {:comp {:italiano {:initial false}}
      :head {:italiano {:initial true}}}
     {:head {:italiano head-italian}
      :comp {:italiano comp-italian}
      :italiano {:a head-italian
                :b comp-italian}})))

(def head-last
  (let [head-italian (ref :top)
        comp-italian (ref :top)]
    (unifyc
     {:comp {:italiano {:initial true}}
      :head {:italiano {:initial false}}}
     {:head {:italiano head-italian}
      :comp {:italiano comp-italian}
      :italiano {:a comp-italian
                :b head-italian}})))

;; -- BEGIN SCHEMA DEFINITIONS
(def schema-10
  (unifyc
   subcat-1-principle
   head-principle
   {:first :comp
    :comp {:synsem {:subcat '()}}}))

(def c10
  (unifyc
   schema-10
   head-last
   {:comment "c10"
    ;; TODO: using :schema-symbol below - cannot use :schema for some reason; need to figure out why.
    ;; if you try to use :schema, I get:
    ;; java.util.concurrent.ExecutionException: java.lang.RuntimeException:
    ;; Can't embed object in code, maybe print-dup not defined: clojure.lang.Ref@11819f3c
    :schema-symbol 'c10 ;; used by over-each-parent to know where to put children.
    :first :comp
    :comp {:synsem {:subcat '()}}}))

(def c21
  (unifyc
   subcat-2-principle
   head-principle
   head-last
   {:comp {:synsem {:subcat '()
                    :pronoun true}}
    :schema-symbol 'c21 ;; used by over-each-parent to know where to put children.
    :first :comp
    :comment "c21"}))

(def h11
  (unifyc
   subcat-1-1-principle
   hc-agreement
   head-principle
   comp-modifies-head
   head-first
   {
    :schema-symbol 'h11 ;; used by over-each-parent to know where to put children.
    :first :head
    :comment "h11"}))


(def h11-comp-subcat-1
  (let [subcat (ref :top)]
    (unifyc
     {:head {:synsem {:subcat {:1 subcat}}}
      :comp {:synsem {:subcat {:1 subcat}}}}
     subcat-1-1-principle-comp-subcat-1
     hc-agreement
     head-principle
     comp-modifies-head
     head-first
     {:schema-symbol 'h11-comp-subcat-1
      :first :head
      :comment "h11-comp-subcat-1"})))

(def h10
  (unifyc
   subcat-1-principle
   head-principle
   head-first
   {:comment "h10"
    :schema-symbol 'h10 ;; used by over-each-parent to know where to put children.
    :first :head}))

(def h21
  (unifyc
   subcat-2-principle
   head-principle
   head-first
   {:comment "h21"
    :schema-symbol 'h21 ;; used by over-each-parent to know where to put children.
    :first :head}))

(def h22
  (unifyc
   subcat-2-2-principle
   head-principle
   head-first
   {:comment "h22"
    :schema-symbol 'h22 ;; used by over-each-parent to know where to put children.
    :first :head}))

(def h32
  (unifyc
   subcat-5-principle
   head-principle
   head-first
   {:comment "h32"
    :schema-symbol 'h32 ;; used by over-each-parent to know where to put children.
    :first :head}))

;; -- END SCHEMA DEFINITIONS

(def grammar (list (unifyc h21
                           {:rule "adjective-phrase"
                            :synsem {:cat :adjective}})

                   (unifyc h21
                           (let [head-synsem {:cat :intensifier
                                              :modified true}] ;; TODO: document what purpose :modified serves (if any: if none, remove).
                             {:rule "intensifier-phrase"
                              :synsem head-synsem}))

                   (unifyc h11-comp-subcat-1
                           (let [head-synsem {:cat :noun
                                              :modified true}]
                             {:comp {:phrasal false ;; rathole prevention ;; TODO: see if this can be removed.
                                     :synsem {:cat :adjective
                                              :mod head-synsem}}
                              :head {:phrasal false
                                     :synsem {:modified false}} ;; TODO: document what purpose :modified serves (if any: if none, remove).
                              :rule "nbar"
                              :synsem head-synsem}))

                   (unifyc c10
                           comp-specs-head
                           (let [number-agreement (ref :top)]
                             {:rule "noun-phrase1"
                              :aliases (list "np1")
                              :synsem {:agr {:number number-agreement}
                                       :cat :noun
                                       :sem {:number number-agreement
                                             :mod '()}}
                              :head {:phrasal false}
                              :comp {:phrasal false}})) ;; rathole prevention ;; TODO: see if this can be removed.

                   (unifyc c10
                           comp-specs-head
                           (let [number-agreement (ref :top)]
                             {:rule "noun-phrase2"
                              :aliases (list "np2")
                              :synsem {:agr {:number number-agreement}
                                       :cat :noun
                                       :sem {:number number-agreement}}
                              :head {:phrasal true}
                              :comp {:phrasal false}})) ;; rathole prevention ;; TODO: see if this can be removed.

                   (unifyc h10
                           {:rule "prepositional-phrase"
                            :synsem {:cat :prep}})

                   (unifyc c10
                           {:head {:phrasal true ;; only a vp-aux may be the head child, not simply a lexical auxiliary verb.
                                   :synsem {:aux true}}
                            :rule "s-aux"
                            :synsem {:infl :present
                                     :cat :verb
                                     :sem {:aspect :perfect
                                           :tense :past}}})

                   (unifyc c10
                          {:rule "s-future"
                           :synsem {:aux false
                                    :infl :futuro
                                    :cat :verb
                                    :sem {:tense :futuro}}})

                   (unifyc c10
                          {:rule "s-conditional"
                           :synsem {:aux false
                                    :infl :conditional
                                    :cat :verb
                                    :sem {:tense :conditional}}})

                   (unifyc c10
                           {:rule "s-imperfetto"
                            :synsem {:aux false
                                     :infl :imperfetto
                                     :cat :verb
                                     :sem {:aspect :progressive
                                           :tense :past}}})

                   (unifyc c10
                           {:rule "s-present"
                            :synsem {:aux false
                                     :infl :present
                                     :cat :verb
                                     :sem {:aspect :progressive
                                           :tense :present}}})

                   (unifyc h21
                           {:rule "vp-infinitive"
                            :synsem {:aux false
                                     :infl :infinitive
                                     :cat :verb}})

                   (unifyc h21
                           {:rule "vp-aux"
                            :head {:phrasal false}
                            :synsem {:aux true
                                     :infl :present
                                     :sem {:tense :past}
                                     :cat :verb}})

                   ;; this rule is kind of complicated and made more so by
                   ;; dependence on auxilary sense of "avere" which supplies the
                   ;; obj-agr agreement between the object and the main (non-auxilary) verb.
                   (unifyc h22
                           (let [obj-agr (ref :top)]
                             {:head {:phrasal false}
                              :rule "vp-aux-22"
                              :synsem {:aux true
                                       :cat :verb
                                       :infl :present
                                       :sem {:tense :past}
                                       :subcat {:2 {:agr obj-agr}}}
                              :italiano {:b {:obj-agr obj-agr}}}))

                   (unifyc h21
                           {:rule "vp-future"
                            :synsem {:aux false
                                     :infl :futuro
                                     :cat :verb}})

                   (unifyc h21
                          {:rule "vp-imperfetto"
                           :synsem {:aux false
                                    :infl :imperfetto
                                    :cat :verb}})

                   (unifyc h21
                           {:rule "vp-past"
                            :synsem {:aux false
                                     :infl :past
                                     :cat :verb}})

                   (unifyc h21
                           {:rule "vp-present"
                            :synsem {:aux false
                                     :infl :present
                                     :sem {:tense :present}
                                     :cat :verb}})

                   (unifyc c21
                           {:comp {:phrasal false
                                   :synsem {:cat :noun
                                            :pronoun true}}
                            :rule "vp-pronoun"
                            :synsem {:cat :verb
                                     :infl {:not :past}}})

                   (unifyc h10
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
(def cache (future (create-index grammar lexicon head-principle)))

(def end (System/currentTimeMillis))
(log/info "Built grammatical and lexical cache in " (- end begin) " msec.")
