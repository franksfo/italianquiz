(ns italianverbs.grammar
  (:refer-clojure :exclude [get-in resolve])
  (:use [clojure.set :only (union intersection)]
        [clojure.core :exclude (get-in resolve merge)]
        [italianverbs.generate :only (moreover-head moreover-comp gen14 over3 lazy-shuffle)]
        [italianverbs.lexicon]
        [italianverbs.lexiconfn :only (unify sem-impl)]
        [italianverbs.morphology :only (finalize fo italian-article get-italian-1 get-italian)]
        [italianverbs.ug]
        [italianverbs.unify :only (copy fail? serialize get-in resolve)]
        )

  (:require [clojure.tools.logging :as log]
            [italianverbs.unify :as unify]
            [clojure.string :as string]))

(log/info "begin italian-english specific lexical categories..")

(def common-nouns
  (filter (fn [lexeme]
            (and (= (get-in lexeme '(:synsem :cat)) :noun)
                 (= (get-in lexeme '(:synsem :subcat :1 :cat)) :det)))
          cc10-heads))

(def dets
  (filter (fn [lexeme]
            (= (get-in lexeme '(:synsem :cat)) :det))
          cc10-comps))

(def propernouns-and-pronouns
  ;; TODO: more compile-time filtering
  (filter (fn [lexeme]
            (and (= (unify/get-in lexeme '(:synsem :cat)) :noun)
                 (= (unify/get-in lexeme '(:synsem :subcat)) '())))
          cc10-comps))

(def pronouns
  ;; TODO: more compile-time filtering
  (filter (fn [lexeme]
            (and (= (unify/get-in lexeme '(:synsem :cat)) :noun)
                 (not (= (unify/get-in lexeme '(:synsem :pronoun)) false))))
          propernouns-and-pronouns))

(def propernouns
  ;; TODO: more compile-time filtering
  (filter (fn [lexeme]
            (and (= (unify/get-in lexeme '(:synsem :cat)) :noun)
                 (not (= (unify/get-in lexeme '(:synsem :pronoun)) true))))
          propernouns-and-pronouns))

(def transitive-verbs
  (filter (fn [candidate]
            ;; filter Vs to reduce number of candidates we need to filter:
            ;; (only transitive verbs)
            (and (not (= :notfound (unify/get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                 (= (unify/get-in candidate '(:synsem :cat)) :verb)
                 (= (unify/get-in candidate '(:synsem :subcat :2 :cat)) :noun)))
          hh21-heads))

(def intransitive-verbs
  (filter (fn [lexeme]
            (and (= (get-in lexeme '(:synsem :cat)) :verb)
                 (= (get-in lexeme '(:synsem :subcat :2)) '())))
          all-in-lexicon))

(def aux-verbs
  (filter (fn [candidate]
            (and (not (= :notfound (unify/get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                 (= (unify/get-in candidate '(:synsem :cat)) :verb)
                 (= (unify/get-in candidate '(:synsem :aux)) true)))
          hh21-heads))

(def modal-verbs
  (filter (fn [candidate]
            (and (not (= :notfound (unify/get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                 (= (unify/get-in candidate '(:synsem :cat)) :verb)
                 (= (unify/get-in candidate '(:synsem :aux) false) false)
                 (= (unify/get-in candidate '(:synsem :subcat :2 :cat)) :verb)))
          hh21-heads))

(def sent-adverbs
  (filter (fn [candidate]
            (= (unify/get-in candidate '(:synsem :cat)) :sent-modifier))
          hh10-heads))

(log/info "done italian-english specific lexical categories.")
