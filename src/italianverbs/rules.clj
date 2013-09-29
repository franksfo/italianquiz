(ns italianverbs.rules
  (:refer-clojure :exclude [get-in resolve])
  (:use [italianverbs.generate :only (rewrite-as)]
        [italianverbs.grammar]
        [italianverbs.lexicon :only (it1)]
        [italianverbs.ug]
        [italianverbs.morphology :only (fo)])
  (:require [clojure.tools.logging :as log]
))

(log/info "started loading rules.")

;; undefine any previous values: TODO: should be a one-liner.
(ns-unmap 'italianverbs.rules 'declarative-sentence)
(ns-unmap 'italianverbs.rules 'np)
(ns-unmap 'italianverbs.rules 'vp)
(ns-unmap 'italianverbs.rules 'vp-transitive)
(ns-unmap 'italianverbs.rules 'supercool)

(rewrite-as declarative-sentence {:schema 'cc10
                                  :comp 'np
                                  :head 'vp})
(rewrite-as np {:schema 'cc10
                :comp 'dets
                :head 'common-nouns})

(rewrite-as np 'propernouns)
(rewrite-as np 'pronouns)

(rewrite-as vp 'intransitive-verbs)
(rewrite-as vp {:schema 'hh21
                :comp 'np
                :head 'transitive-verbs})
(rewrite-as vp {:schema 'ch21
                :comp 'pronouns
                :head 'transitive-verbs})
(rewrite-as vp {:schema 'hh21
                :head 'aux-verbs
                :comp 'intransitive-verbs})
;; uncomment once 'supercool' works on its own.
;(rewrite-as vp {:schema 'hh21
;                :head 'aux-verbs
;                :comp 'vp-transitive})

(rewrite-as vp-transitive {:schema 'hh21
                           :comp 'np
                           :head 'transitive-verbs})

(rewrite-as supercool {:schema 'hh21
                       :head 'aux-verbs
                       :comp 'vp-transitive})

(defn sc []
  (take 1 (gen-all 'supercool supercool)))

(defn vpt []
  (take 1 (gen-all 'vpt vp-transitive)))

;; -- aliases --
(def ds declarative-sentence)


;; -- useful functions
(defn sentences []
  (gen-all 'sentences (shuffle declarative-sentence)))

(defn random-sentence []
  (take 1 (gen-all 'declarative-sentence (shuffle declarative-sentence))))

(defn random-sentences [n]
  (repeatedly n (fn [] (random-sentence))))

(log/info "done loading rules.")
