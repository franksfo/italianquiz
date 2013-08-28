(ns italianverbs.grammar
  (:use [italianverbs.lexiconfn :only (unify)])
  (:require
   [italianverbs.unify :as fs]
   [italianverbs.morphology :as morph]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.lexicon :as lex]
   [italianverbs.generate :as gen]
   [clojure.string :as string]))

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
    {:head {:italian head-italian}
     :comp {:italian comp-italian}
     :italian {:a head-italian
               :b comp-italian}}))

(def italian-head-last
  (let [head-italian (ref :top)
        comp-italian (ref :top)]
    {:head {:italian head-italian}
     :comp {:italian comp-italian}
     :italian {:a comp-italian
               :b head-italian}}))

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

(def cc0
  (unify
   {:comp {:italian {:initial true}}
    :head {:italian {:initial false}}}
   subcat-1-principle
   head-principle
   italian-head-last
   english-head-last))

(def vp-plus-adverb
  (unify subcat-5-principle
         head-principle-no-infl
         italian-head-first
         english-head-first
         {:extend {:a {:head (fn [] lex/verbs)
                       :comp (fn [] lex/adverbs)}}}))

;; TODO: make adjective the head (currently the complement)
;; and make noun the complement (currently the head)
(def nbar
  (let [head-semantics (ref :top)
        adjectival-predicate (ref :top)
        agr (ref :top)
        subcat (ref :top)]
    (unify head-principle
           ;; for Nbar, italian and english have opposite constituent order:
           italian-head-first
           english-head-last
           (let [def (ref :top)]
             {:head {:synsem {:def def}}
              :synsem {:def def}})
           {:synsem {:sem head-semantics}
            :comp {:synsem {:sem {:mod head-semantics}}}}
           ;; the following will rule out pronouns, since they don't subcat for a determiner;
           ;; (in fact, they don't subcat for anything)
           {:synsem {:subcat {:1 {:cat :det}}}}

           {:synsem {:agr agr
                     :subcat subcat}
            :head {:synsem {:agr agr
                            :cat :noun
                            :subcat subcat}}
            :comp {:synsem {:cat :adjective}
                   :italian {:agr agr}
                   :english {:agr agr}}}

           {:synsem {:sem {:mod adjectival-predicate}}
            :comp {:synsem {:sem {:mod head-semantics
                                  :comparative false
                                  :pred adjectival-predicate}}}}
           {:comment "n&#x0305; &#x2192; noun adj"
            :comment-plaintext "nbar -> noun adj"
            :extend {:a {:head (fn [] lex/nouns)
                         :comp (fn [] lex/adjs)}}})))


(def nbar2
  (let [head-semantics (ref :top)
        adjectival-predicate (ref :top)
        agr (ref :top)
        subcat (ref :top)]
    (unify head-principle
           ;; for Nbar2, italian and english have the same constituent order:
           ;; Adj then Noun,
           ;; e.g. "nuovo cellulare (new mobile phone)"
           italian-head-last
           english-head-last
           (let [def (ref :top)]
             {:head {:synsem {:def def}}
              :synsem {:def def}})
           {:synsem {:sem head-semantics}
            :comp {:synsem {:sem {:mod head-semantics}}}}
           ;; the following will rule out pronouns, since they don't subcat for a determiner;
           ;; (in fact, they don't subcat for anything)
           {:synsem {:subcat {:1 {:cat :det}}}}

           {:synsem {:agr agr
                     :subcat subcat}
            :head {:synsem {:agr agr
                            :cat :noun
                            :subcat subcat}}
            :comp {:synsem {:cat :adjective}
                   :italian {:agr agr}
                   :english {:agr agr}}}

           {:synsem {:sem {:mod adjectival-predicate}}
            :comp {:synsem {:sem {:mod head-semantics
                                  :comparative false
                                  :pred adjectival-predicate}}}}
           {:comment "n&#x0305; &#x2192; noun adj"
            :comment-plaintext "nbar -> noun adj"
            :extend {:a {:head (fn [] lex/adjs-initial-in-italian)
                         :comp (fn [] lex/nouns)}}})))

(def np-rules
  (let [head (ref :top)
        comp (ref :top)]
    (def np
      (let [head-english (ref :top)
            head-italian (ref :top)
            comp-english (ref :top)
            comp-italian (ref :top)]
        (fs/unifyc head-principle subcat-1-principle ;; NP -> Comp Head
                   (let [agr (ref :top)]
                     (fs/unifyc
                      (let [def (ref :top)]
                        {:head {:synsem {:def def}}
                         :synsem {:def def}
                         :comp {:synsem {:def def}}})
                      {:head {:italian head-italian
                              :english head-english
                              :synsem {:cat :noun
                                       :agr agr}}
                       :comp {:synsem {:cat :det}
                              :italian comp-italian
                              :english comp-english}
                       :synsem {:agr agr}
                       :comment "np &#x2192; det (noun or nbar)"
                       :comment-plaintext "np -> det (noun or nbar)"

                       ;; for NP, italian and english have same constituent order:
                       :italian {:a comp-italian
                                 :b head-italian}
                       :english {:a comp-english
                                 :b head-english}
                       :extend {
                                :a {:comp (fn [] lex/dets)
                                    :head (fn [] lex/nouns)}
                                :b {:comp (fn [] lex/dets)
                                    :head (fn [] nbar)}
                                :c {:comp (fn [] lex/dets)
                                    :head (fn [] nbar2)}

                                }
                   })))))
    (list np)))

(def vp-infinitive-transitive)
(def vp-pron-verb-first)

(def prep-phrase
  (merge
    (unify head-principle
           subcat-1-principle-no-complement-subcat-restrictions
           italian-head-first
           english-head-first)
    {:extend {:a {:head (fn [] lex/preps)
                  :comp (fn [] lex/propernouns-and-pronouns)}

              :b {:head (fn [] lex/preps)
                  :comp (fn [] np)}

              :c {:head (fn [] lex/preps)
                  :comp (fn [] lex/verbs)}

              :d {:head (fn [] lex/preps)
                  :comp (fn [] vp-infinitive-transitive)}

              :e {:head (fn [] lex/preps)
                  :comp (fn [] vp-pron-verb-first)}}}))

(def vp)

(def adj-phrase
  (unify head-principle
         subcat-2-principle
         italian-head-first
         english-head-first
         {:comment "adj-phrase&nbsp;&#x2192;&nbsp;adj&nbsp;+&nbsp;prep-phrase" ;; sorry that this is hard to read: trying to avoid the
          ;; linebreaking within comment: TODO: use CSS to accomplish this instead.
          :comment-plaintext "adj-phrase -> adj prep-phrase"}

         ;; TODO: prep-phrase should be {:cat {:not {:nom}}} to avoid "richer than he" (should be "richer than him")

         (let [comparative (ref true)]
           {:synsem {:sem {:comparative comparative}}
            :comp {:synsem {:sem {:comparative comparative}}}
            :head {:synsem {:sem {:comparative comparative}}}})

         (let [agr (ref :top)]
           {:synsem {:agr agr}
            :italian {:a {:agr agr}} ;; this enforces adjective-np agreement with subject.
            :head {:synsem {:agr agr}}})

         {:synsem {:cat :adjective}
          :extend {:a {:head (fn [] lex/adjs)
                       :comp (fn [] prep-phrase)}}}))

;; intensifier (e.g. "più") is the head, which subcategorizes
;; for an adjective.
;; the head is the adjective-phrase, not the intensifier,
;; while the intensifier is the complement.
(def intensifier-phrase
  (unify head-principle
         subcat-2-principle
         italian-head-first
         english-head-first ;; not sure about this e.g. "più ricca di Paolo (richer than Paolo)"

         (let [agr (ref :top)]
           {:synsem {:subcat {:1 {:agr agr}}}
            :comp {:synsem {:agr agr}}
            :italian {:b {:agr agr}}})

         ;; TODO: specify this in lexicon (subcat of head) rather than here in grammar..
         {:head {:synsem {:cat :intensifier}}}


         ;; ..but for now we use "more=rich" e.g. "più ricca di Paolo (more rich than Paolo)"
         {:comment "intensifier-phrase&nbsp;&#x2192;&nbsp;intensifier&nbsp;+&nbsp;adj-phrase"
          :comment-plaintext "intensifier-phrase -> intensifier adj-phrase"
          :extend {:a {:comp (fn [] adj-phrase)
                       :head (fn [] lex/intensifiers)}}}))

(let [head (ref :top)
      ;; commenting out a) in favor of b): heads should declare this constraint if they want.
      ;; a)
      ;; comp (ref {:subcat '()})
      ;; b) comp has no restrictions on its :subcat.
      comp (ref :top)
      infl (ref :top)
      agr (ref :top)]

  (def vp-infinitive-transitive
    (fs/unifyc head-principle
               subcat-2-principle
               verb-inflection-morphology
               {:head {:synsem {:cat :verb
                                :infl :infinitive
                                :subcat {:2 {:cat :noun
                                             :subcat '()}}}}}
               {:comment "vp[inf] &#x2192; head comp"
                :comment-plaintext "vp[inf] -> head comp"}
               italian-head-first
               english-head-first
               {:extend {
                         :a {:head (fn [] lex/verbs)
                             :comp (fn [] np)}}}))


  (def vp ;; TODO replace other vps with just vp.
    (fs/unifyc head-principle
               subcat-2-principle
               verb-inflection-morphology
               {:comment "vp &#x2192; head comp"
                :comment-plaintext "vp -> head comp"}
               italian-head-first
               english-head-first
               {:comp comp
                :head head}
               {:head {:synsem {:cat :verb}}}
               {:english {:a {:infl infl
                              :agr agr}
                          :infl infl
                          :agr agr}
                :italian {:a {:infl infl
                              :agr agr}
                          :b {:agr agr}
                          :infl infl}}

               ;; note that vp-pron does not
               ;; inherit from vp, so it does not have this constraint.
               ;; we must have this negative constraint to prevent
               ;; the accusative pronoun from appearing after the verb:
               ;; e.g. *"io vede ti (i see you)" (compare with correct:
               ;; "io ti vede (i see you)" generated by vp-pron.
               {:comp {:synsem {:pronoun {:not true}}}}
               {:extend {
                         :a {:head (fn [] lex/verbs)
                             :comp (fn [] np)}
                         :b {:head (fn [] lex/verbs)
                             :comp (fn [] prep-phrase)}
                         :c {:head (fn [] lex/verbs)
                             :comp (fn [] vp-infinitive-transitive)}
                         :d {:head (fn [] lex/verbs)
                             :comp (fn [] lex/propernouns-and-pronouns)}
                         :e {:head (fn [] lex/verbs)
                             :comp (fn [] intensifier-phrase)}
                         }}))

  ;; vp-pron is verb-last in Italian, so this is the opposite order in Italian.
  ;; e.g. "aiutare" "ti" (morphology turns it into "aiutarti").
  ;; TODO: as mentioned above, fold this into vp.
  (def vp-pron-verb-first
    (unify
     head-principle
     subcat-2-principle
     italian-head-first
     english-head-first
     {:head {:synsem {:cat :verb}}
      :comp {:synsem {:cat :noun
                      :pronoun true}}}
     {:comment-plaintext "vp[pron-vf]"
      :comment "vp[pron-vf]"
      :extend {:a {:head (fn [] lex/verbs)
                   :comp (fn [] lex/propernouns-and-pronouns)}}}))

  ;; "tu fai bene [a vendere la casa]"
  (def prep-plus-verb-inf
    (unify
     subcat-4-principle
     head-principle
     {:comment "pp &#x2192; prep vp[inf]"
      :comment-plaintext "pp -> prep vp[inf]"}

     italian-head-first
     english-head-first

     {:extend {:a {:head (fn [] lex/preps)
                   :comp (fn [] vp)}
               :b {:head (fn [] lex/preps)
                   :comp (fn [] vp-pron-verb-first)}
               :c {:head (fn [] lex/preps)
                   :comp (fn [] lex/verbs)}}}))

  (def vp-aux-3
    (let [aspect (ref :top)
          agr (ref :top)]
      (fs/merge
       (unify
        head-principle
        subcat-3-principle
        verb-inflection-morphology
        italian-head-first
        english-head-first
          {:comment "vp[aux3] &#x2192; head comp"
           :comment-plaintext "vp[aux3] -> head comp"
           ;; force the head (auxiliary verb (essere/avere)) to be present-tense:
           ;; non-present is possible too, but deferring that till later.
           :head {:synsem {:infl :present
                           :cat :verb
                           :aux true
                           :subcat {:2 {:cat :verb
                                        :infl :past}}}}}
          {:english {:a {:agr agr}
                     :b {:agr agr}
                     :agr agr}
           :italian {:a {:agr agr}
                     :b {:agr agr}
                     :agr agr}
           :head {:synsem {:agr agr}}
           :comp {:synsem {:agr agr}}}
          {:extend {:a {:head (fn [] lex/verbs)
                        :comp (fn [] lex/verbs)}}}))))

  (def vp-pron
    (unify
     head-principle
     subcat-2-principle
     italian-head-last
     english-head-first
     {:head {:synsem {:cat :verb}}
                                        ;                       :infl :present}} ;; TODO: allow other than :present. (:present-only for now for testing).
      :comp {:synsem {:cat :noun
                      :pronoun true}}}
     {:comment-plaintext "vp[pron]"
      :comment "vp[pron]"
      :extend {:a {:head (fn [] lex/verbs)
                   :comp (fn [] lex/propernouns-and-pronouns)}
               :b {:head (fn [] lex/verbs)
                   :comp (fn [] vp-aux-3)}}}))

  (def vp-past
    (fs/merge
     (unify
      vp
;      (dissoc vp :extend) ;; for debugging: allows elimination of vp's extend.
      (let [essere-boolean (ref :top)]
        {:head {:synsem {:essere essere-boolean}}
         :synsem {:infl :past
                  :essere essere-boolean
                  :sem {:aspect :passato}}}))
     {:comment "vp[past] &#x2192; head comp"
      :comment-plaintext "vp[past] -> head comp"}
     ;; TODO: promote to vp.
     {:extend {:f {:head (fn [] vp-plus-adverb)
                   :comp (fn [] prep-plus-verb-inf)}}}))

  (def vp-aux
    (let [aspect (ref :top)
          agr (ref :top)]
      (fs/merge
       (unify
              head-principle
              subcat-2-principle
              verb-inflection-morphology
              italian-head-first
              english-head-first
              {:comment "vp[aux] &#x2192; head comp"
               :comment-plaintext "vp[aux] -> head comp"
               ;; force the head (auxiliary verb (essere/avere)) to be present-tense:
               ;; non-present is possible too, but deferring that till later.
               :head {:synsem {:infl :present
                               :cat :verb
                               :aux true
                               :subcat {:2 {:cat :verb
                                            :infl :past}}}}
               :comp {:synsem {:subcat {:2 '()}}}}
              {:english {:a {:agr agr}
                         :b {:agr agr}
                         :agr agr}
               :italian {:a {:agr agr}
                         :b {:agr agr}
                         :agr agr}
               :head {:synsem {:agr agr}}
               :comp {:synsem {:agr agr}}})
       {:extend {:a {:head (fn [] lex/verbs)
                     :comp (fn [] vp-past)}
                 :b {:head (fn [] lex/verbs)
                     :comp (fn [] lex/verbs)}}})))

  (def vp-present
    (let [aspect (ref :top)]
    ;; add to vp some additional expansions for vp-present:
      (fs/merge (fs/copy vp)
                {:comment "vp[present] &#x2192; head comp"
                 :comment-plaintext "vp[present] -> head comp"
                 :head {:synsem {:infl :present
                                 :aux false}}
                 :extend {:f {:head (fn [] lex/verbs)
                              :comp (fn [] vp-past)}}})))


  (def vp-imperfetto
    (fs/merge (fs/copy vp)
              {:comment "vp[imperfetto] &#x2192; head comp"
               :comment-plaintext "vp[imperfetto] -> head comp"
               :comp {:synsem {:infl {:not :past}}}
               :head {:synsem {:infl :imperfetto
                               :sem {:activity true}}}
               ;; force the auxiliary verb (essere/avere) to be present-tense:
               ;; non-present is possible too, but deferring that till later.
               ;; add to vp some additional expansions for vp-imperfetto:
               :extend {:f {:head (fn [] lex/verbs)
                            :comp (fn [] lex/propernouns-and-pronouns)}
                        :g {:head (fn [] lex/verbs)
                            :comp (fn [] np)}}}))

)

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

(def sentence-rules
  (let [subj-sem (ref :top)
        subcatted (ref {:cat :noun
                        :subcat '()
                        :sem subj-sem})
        infl (ref :top)
        tense (ref :top)
        comp (ref {:synsem subcatted})
        agr (ref :top)
        head (ref {:synsem {:cat :verb
                            :sem {:subj subj-sem
                                  :tense tense}
                            :subcat {:1 subcatted
                                     :2 '()}}})

        rule-base-no-extend
        (fs/unifyc head-principle subcat-1-principle
                   subject-verb-agreement
                   {:synsem {:sem {:tense tense}}})

        rule-base
        (fs/unifyc rule-base-no-extend
                   {:extend {
                             :a {:comp (fn [] np)
                                 :head (fn [] vp)}
                             :b {:comp 'lexicon
                                 :head (fn [] vp)}
                             :c {:comp 'lexicon
                                 :head (fn [] vp-pron)}
                             :d {:comp (fn [] np)
                                 :head (fn [] vp-pron)}
                             :e {:comp (fn [] np)
                                 :head 'lexicon}
                             :f {:comp 'lexicon
                                 :head 'lexicon}
                             }})]

    (def s-present
      ;; unlike the case for future and imperfetto,
      ;; override the existing :extends in the case of s-present.
      (fs/merge
       (fs/unifyc rule-base
                  italian-head-last
                  english-head-last
                  {:comment "sentence[present]"
                   :comment-plaintext "s[present] -> .."
                   :synsem {:infl :present
                            :sem {:tense :present}}})
       {:extend {:g {:comp 'lexicon
                     :head (fn [] vp-present)}
                 :h {:comp (fn [] np)
                     :head (fn [] vp-present)}
                 }}))

    ;; if parent is subcat0, head must be subcat1 (i.e. intransitive).
    (def s-present2
      (fs/merge
       (fs/unifyc cc0
                  {:synsem {:infl :present}})
       {:extend {:g {:comp 'lexicon
                     :head (fn [] vp)}
                 :h {:comp (fn [] np)
                     :head (fn [] vp)}}}))


    ;; TODO: a) and b) should both be reducible to one rule.
    ;; the problem is that the :extend (generation rules) differs.
    ;; so if we encode the generation rules in the lexical entries,
    ;; we can collapse these 2 definitions into 1.
    ;; also we can do away with these long paths: (:head :synsem :subcat :1 :sem :tense :present).
    ;; a)
    ;; e.g. "qualche volte, <s-present>"
    (def s-present-modifier
      (fs/unifyc head-principle
                 subcat-1-principle
                 italian-head-first
                 english-head-first
                 {:comment "mod + s-present"
                  :comment-plaintext "mod + s-present"
                  :extend {:a {:head 'lexicon
                               :comp (fn [] s-present)}}
                  :synsem {:cat :sent-modifier}
                  :head {:synsem {:subcat {:1 {:sem {:tense :present}}}}}}))


    (def s-past
      (fs/merge
       (fs/unifyc rule-base-no-extend
                  italian-head-last
                  english-head-last
                  {:comment "sentence[past]"
                   :comment-plaintext "s[past] -> .."
                   :synsem {:infl :present
                            :sem {:aspect :passato
                                  :tense :past}}})
       {:extend {:g {:comp 'lexicon
                     :head (fn [] vp-aux)}
                 :h {:comp (fn [] np)
                     :head (fn [] vp-aux)}
                 }}))

    ;; b)
    ;; takes an past sentence as a complement: e.g. "ieri, <s-past>"
    (def s-past-modifier
      (fs/unifyc head-principle
                 subcat-1-principle
                 italian-head-first
                 english-head-first
                 {:comment "mod + s-past"
                  :comment-plaintext "mod + s-past"
                  :extend {:a {:head 'lexicon
                               :comp (fn [] s-past)}}
                  :synsem {:cat :sent-modifier}
                  :head {:synsem {:subcat {:1 {:sem {:tense :past}}}}}}))

    (def s-future
      (fs/unifyc rule-base-no-extend
                 italian-head-last
                 english-head-last
                 {:extend {:a {:comp (fn [] np)
                               :head (fn [] vp)}
                           :b {:comp 'lexicon
                               :head (fn [] vp)}}}
                 {:comment "sentence[future]"
                  :comment-plaintext "s[future] -> .."
                  :synsem {:infl :futuro}}))

    (def s-future-modifier
      (fs/unifyc head-principle
                 subcat-1-principle
                 italian-head-first
                 english-head-first
                 {:comment "mod + s-past"
                  :comment-plaintext "mod + s-future"
                  :extend {:a {:head 'lexicon
                               :comp (fn [] s-future)}}
                  :synsem {:cat :sent-modifier}
                  :head {:synsem {:subcat {:1 {:sem {:tense :future}}}}}}))

    (def s-imperfetto
      (fs/unifyc rule-base
                 italian-head-last
                 english-head-last
                 {:comment "sentence[imperfetto]"
                  :comment-plaintext "s[imperfetto] -> .."
                  :synsem {:infl :imperfetto
                           :sem {:activity true}}
                  :extend {:g {:comp 'lexicon
                               :head (fn [] vp-imperfetto)}
                           :h {:comp (fn [] np)
                               :head (fn [] vp-imperfetto)}
                           :i {:comp 'lexicon
                               :head (fn [] vp-pron)}}}))
    (def temporal-glue-phrase
      (fs/unifyc subcat-2-principle
                 italian-head-first
                 english-head-first
                 {:comment "temporal-glue-phrase"
                  :comment-plaintext "temporal-glue-phrase"
                  :extend {:a {:comp (fn [] s-past)
                               :head (fn [] lex/temporal-glue)}}}))


    (def s-temporal-glue
      (fs/unifyc subcat-1-principle
                 italian-head-last
                 english-head-last
                 {:command "temporal-sentence"
                  :comment-plaintext "temporal-sentence"
                  :extend {:a {:comp (fn [] s-imperfetto)
                               :head (fn [] temporal-glue-phrase)}}}))))


;; TODO: move to lexicon (maybe).
(defn italian-number [number]
  (cond
   (= number 1) "una"
   (= number 2) "due"
   (= number 3) "tre"
   (= number 4) "quattro"
   (= number 5) "cinque"
   (= number 6) "sei"
   (= number 7) "sette"
   (= number 8) "otto"
   (= number 9) "nove"
   (= number 10) "dieci"
   (= number 11) "undici"
   (= number 12) "dodici"
   (= number 13) "tredici"
   (= number 14) "quattordici"
   (= number 15) "quindici"
   (= number 16) "sedici"
   (= number 17) "diciassette"
   (= number 18) "diciotto"
   (= number 19) "diciannove"

   ;; ...
   (= number 20) "venti"
   (< number 30) (str (italian-number 20) (italian-number (- number 20)))
   (= number 30) "trenta"
   (< number 40) (str (italian-number 30) (italian-number (- number 30)))
   true "??"))

(defn italian-time [hour minute ampm]
  (let [print-hour
        (if (<= minute 30)
          (italian-number hour)
          (italian-number
           (if (= hour 12)
             1
             (+ hour 1))))]
    (str
     (cond
      (and (= print-hour 12)
           (= ampm "am"))
      "mezzogiorno"
      (and (= print-hour 12)
           (= ampm "pm"))
      "mezzonotte"
      true (morph/italian-article {:italian "le" :def :def} {:number :singular :italian print-hour :numerical true}))
     (cond
      (= minute 0) ""
      (= minute 15) " e un quarto"
      (= minute 30) " e mezzo"
      (= minute 45) " meno un quarto"
      (<= minute 30)
      (str " e " (italian-number minute))
      true (str " meno "(italian-number (- 60 minute)))))))

(defn english-time [hour minute ampm]
  (string/trim (str hour ":" (if (< minute 10) (str "0" minute) minute) " " (if (= hour 12) (if (= ampm "am") " after midnight" " after noon") ""))))

(defn minimal-grammatical-initialization []
  ;; TODO: replace with something less expensive. we want the minimum amount of work that
  ;; will prevent ExceptionInInitializerErrors.
  (first (take 1 (gen/generate nbar))))

(def np (conj {:serialized (fs/serialize np)}
              np))
(def nbar (conj {:serialized (fs/serialize nbar)}
              nbar))
(def s-present (conj {:serialized (fs/serialize s-present)}
                     s-present))

(def s-present-modifier (conj {:serialized (fs/serialize s-present-modifier)}
                              s-present-modifier))

(def s-future (conj {:serialized (fs/serialize s-future)}
                    s-future))

(def s-future-modifier (conj {:serialized (fs/serialize s-future-modifier)}
                             s-future-modifier))

(def s-past (conj {:serialized (fs/serialize s-past)}
                  s-past))

(def s-past-modifier (conj {:serialized (fs/serialize s-past-modifier)}
                           s-past-modifier))

(def s-imperfetto (conj {:serialized (fs/serialize s-imperfetto)}
                        s-imperfetto))

(def s-temporal-glue (conj {:serialized (fs/serialize s-temporal-glue)}
                           s-temporal-glue))

(def temporal-glue-phrase (conj {:serialized (fs/serialize temporal-glue-phrase)}
                                  temporal-glue-phrase))

(def vp-plus-adverb (conj {:serialized (fs/serialize vp-plus-adverb)}
                          vp-plus-adverb))

(def vp-aux (conj {:serialized (fs/serialize vp-aux)}
                  vp-aux))

(def vp-pron (conj {:serialized (fs/serialize vp-pron)}
                   vp-pron))

(def vp-past (conj {:serialized (fs/serialize vp-past)}
                   vp-past))

(def vp-present (conj {:serialized (fs/serialize vp-present)}
                      vp-present))

(def vp-pron-verb-first (conj {:serialized (fs/serialize vp-pron-verb-first)}
                              vp-pron-verb-first))

(def prep-phrase (conj {:serialized (fs/serialize prep-phrase)}
                       prep-phrase))

(def nbar (conj {:serialized (fs/serialize nbar)}
                nbar))

(def adj-phrase (conj {:serialized (fs/serialize adj-phrase)}
                      adj-phrase))


(def seed-phrases
  (list
   cc0))

(defn sentence-impl [input]
  "do things necessary before something can be a sentence. e.g. if infl is still :top, set to
:present (later, set to a randomly selected member of {:finite, :futuro, ..}."
  (cond
   (seq? input)
   (map (fn [each]
          (sentence-impl each))
        input)
   (= input :top) input
   true
   (let [finitize (if (= (fs/get-in input '(:synsem :infl))
                         :top)
                    (first (take 1 (shuffle
                                    (list {:synsem {:infl :present}}
                                          {:synsem {:infl :futuro}})))))]
     (let [merged
           (if (= input :fail) :fail
               (fs/merge input finitize))]
       merged)))) ;; for now, no recursive call.

(defn sent-impl [input]
  "shortcut"
  (sentence-impl input))

;; TODO: move to somewhere else that uses both grammar and lexicon (e.g. quiz or workbook): grammar itslelf should not depend on lexicon (lex/lexicon).
(defn random-sentence []
  (if false
  (morph/finalize (first (take 1 (gen/generate
                                  (first (take 1 (shuffle
                                                  (list s-present
                                                        s-present-modifier
                                                        s-future
                                                        s-future-modifier
                                                        s-past
                                                        s-past-modifier
                                                        s-imperfetto
                                                        s-temporal-glue
                                                        ))))))))
  (morph/finalize (first (take 1
                               (gen/gen14 seed-phrases
                                          (shuffle lex/lexicon) (shuffle lex/lexicon)
                                          sent-impl))))))

(defn random-sentences [n]
  (repeatedly n (fn [] (random-sentence))))

(defn speed-test [ & times]
  "TODO: show benchmark results and statistics (min,max,95%tile,stddev,etc)"
  (let [times (if (first times) (first times) 10)]
    (dotimes [n times] (time (random-sentence)))))

(defn gen21 [heads comps]
  (gen/gen14 seed-phrases
             heads
             comps
             sent-impl
             0))

