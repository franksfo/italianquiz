(ns italianverbs.rules
  (:refer-clojure :exclude [get-in resolve])
  (:use [italianverbs.generate :only (rewrite-as)]
        [italianverbs.grammar]
        [italianverbs.ug]
        [italianverbs.morphology :only (fo)])
  (:require [clojure.tools.logging :as log]
))

(log/info "started loading rules.")
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

;; -- aliases --
(def ds declarative-sentence)

(defn sentences []
  (gen-all (shuffle declarative-sentence)))

(defn random-sentence []
  (take 1 (gen-all (shuffle declarative-sentence))))

(defn random-sentences [n]
  (repeatedly n (fn [] (random-sentence))))

(log/info "done loading rules.")
