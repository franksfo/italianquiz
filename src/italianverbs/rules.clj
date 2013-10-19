(ns italianverbs.rules
  (:refer-clojure :exclude [get-in resolve])
  (:use [italianverbs.generate :only (rewrite-as gen-all lazy-shuffle)]
        [italianverbs.grammar]
        [italianverbs.lexiconfn :only (unify)]
        [italianverbs.lexicon :only (it1)]
        [clojure.repl :only (source)]
        [italianverbs.unify :only (get-in)]
        [italianverbs.ug]
        [italianverbs.morphology :only (fo fof)])
  (:require [clojure.tools.logging :as log]
            [italianverbs.unify :as unify])
)

(log/info "started loading rules.")

;; possible expansions of sentence (for now, only declarative sentences):
(ns-unmap 'italianverbs.rules 'declarative-sentence)
(rewrite-as declarative-sentence {:schema 'cc10
                                  :label 'declarative-sentence
                                  :post-unify-fn sent-impl
                                  :comp 'np
                                  :head 'vp})

(ns-unmap 'italianverbs.rules 'sentence-with-modifier)
(rewrite-as sentence-with-modifier {:schema 'hh10
                                    :label 'sentence-with-modifier-left
                                    :head 'sent-adverbs
                                    :comp 'declarative-sentence})

;; possible expansions of np (noun phrase):
;;
(ns-unmap 'italianverbs.rules 'np)
(ns-unmap 'italianverbs.rules 'np-subj)
(ns-unmap 'italianverbs.rules 'np-obj)
(rewrite-as np {:schema 'cc10
                :comp 'dets
                :head 'common-nouns})
(rewrite-as np 'propernouns)
(rewrite-as np 'pronouns)
(rewrite-as np-subj 'pronouns)
(rewrite-as np-obj 'propernouns)

;; possible expansions of vp (verb phrase):
;;
(ns-unmap 'italianverbs.rules 'vp)

(rewrite-as vp 'intransitive-verbs)
(rewrite-as vp 'modal-vp)
(rewrite-as vp 'past-vp)
(rewrite-as vp 'transitive-vp)
(rewrite-as vp {:schema 'ch21
                :label 'vp
                :comp 'pronouns
                :head 'transitive-verbs})

(ns-unmap 'italianverbs.rules 'modal-vp)
(rewrite-as modal-vp {:schema 'hh21
                      :label 'modal-vp
                      :head 'modal-verbs
                      :comp 'intransitive-verbs})
(rewrite-as modal-vp {:schema 'hh21
                      :label 'modal-vp
                      :head 'modal-verbs
                      :comp 'transitive-vp})

;; possible expansions of transitive vp (verb phrase):
;;
;; undefine any previous values: TODO: should be a one-liner.
(ns-unmap 'italianverbs.rules 'transitive-vp)
(rewrite-as transitive-vp {:schema 'hh21
                           :comp 'np-obj
                           :head 'transitive-verbs})

(ns-unmap 'italianverbs.rules 'past-vp)
(rewrite-as past-vp {:schema 'hh21
                     :label 'past-vp
                     :head 'aux-verbs
                     :comp 'intransitive-verbs})
(rewrite-as past-vp {:schema 'hh21
                     :label 'past-vp
                     :head 'aux-verbs
                     :comp 'transitive-vp})
(rewrite-as past-vp {:schema 'hh21
                     :label 'past-vp
                     :head 'aux-verbs
                     :comp 'modal-vp})

;; -- aliases --
(def ds declarative-sentence)

(ns-unmap 'italianverbs.rules 'sents)
(rewrite-as sents 'ds)
(rewrite-as sents 'sentence-with-modifier)

;; -- useful functions
(defn generate-sentences []
  (gen-all (shuffle declarative-sentence)))

(defn random-sentence []
  (take 1 (gen-all (shuffle sents) "sents" :top)))

(defn gen-with [constraint]
  (take 1 (gen-all (shuffle sents) "sents" constraint)))

(defn random-sentences [ & n]
  (repeatedly (if (first n) (first n) 1000)
              #(random-sentence)))

(log/info "done loading rules.")
