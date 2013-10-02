(ns italianverbs.rules
  (:refer-clojure :exclude [get-in resolve])
  (:use [italianverbs.generate :only (rewrite-as)]
        [italianverbs.grammar]
        [italianverbs.lexicon :only (it1)]
        [italianverbs.ug]
        [italianverbs.morphology :only (fo)])
  (:require [clojure.tools.logging :as log]
            [italianverbs.unify :as unify])
)

(log/info "started loading rules.")

;; undefine any previous values: TODO: should be a one-liner.
(ns-unmap 'italianverbs.rules 'declarative-sentence)
(ns-unmap 'italianverbs.rules 'np)
(ns-unmap 'italianverbs.rules 'vp)
(ns-unmap 'italianverbs.rules 'transitive-vp)

(rewrite-as declarative-sentence {:schema 'cc10
                                  :label 'declarative-sentence
                                  :post-unify-fn sent-impl
                                  :comp 'np
                                  :head 'vp})
(rewrite-as np {:schema 'cc10
                :label 'np
                :comp 'dets
                :head 'common-nouns})
(rewrite-as np 'propernouns)
(rewrite-as np 'pronouns)

(rewrite-as vp 'intransitive-verbs)
(rewrite-as vp 'transitive-vp)
(rewrite-as transitive-vp {:schema 'hh21
                           :label 'vp
                           :comp 'np
                           :head 'transitive-verbs})
(rewrite-as vp {:schema 'ch21
                :label 'vp
                :comp 'pronouns
                :head 'transitive-verbs})
(rewrite-as vp {:schema 'hh21
                :label 'vp
                :head 'aux-verbs
                :comp 'intransitive-verbs})
(rewrite-as vp {:schema 'hh21
                :head 'aux-verbs
                :comp 'transitive-vp})

;; for testing.

(ns-unmap 'italianverbs.rules 'my-vp-transitive)
(ns-unmap 'italianverbs.rules 'myvp-past-trans)
(ns-unmap 'italianverbs.rules 'mynp)
(ns-unmap 'italianverbs.rules 'mydets)

(rewrite-as myds-sentence {:schema 'cc10
                           :label 'declarative-sentence
                           :post-unify-fn sent-impl
                           :comp 'mynp1
                           :head 'myvp})

(rewrite-as mynp1 {:schema 'cc10
                   :label 'mynp
                   :comp 'mydets
                   :head 'donna})
(def mydets (filter (fn [lexeme]
                     (= "la" (unify/get-in lexeme '(:italian))))
                   dets))

(def donna (filter (fn [lexeme]
                     (= "donna" (unify/get-in lexeme '(:italian :italian))))
                   common-nouns))

(rewrite-as myvp {:schema 'hh21
                  :comp 'mynp2
                  :head 'mytransitive-verbs})

(def mytransitive-verbs
  (filter (fn [candidate]
            ;; filter Vs to reduce number of candidates we need to filter:
            ;; (only transitive verbs)
            (= "mangiare" (unify/get-in candidate '(:italian :infinitive))))
          transitive-verbs))

(rewrite-as mynp2 {:schema 'cc10
                   :label 'mynp2
                   :comp 'mydets
                   :head 'pizza})

(def pizza (filter (fn [lexeme]
                     (= "pizza" (unify/get-in lexeme '(:italian :italian))))
                   common-nouns))

;; -- aliases --
(def ds declarative-sentence)

;; -- useful functions
(defn generate-sentences []
  (gen-all (shuffle declarative-sentence)))

(defn random-sentence []
  (log/info "STARTED random-sentence.")
  (take 1 (gen-all (shuffle declarative-sentence))))

(defn random-sentences [n]
  (repeatedly n (fn [] (random-sentence))))

(log/info "done loading rules.")
