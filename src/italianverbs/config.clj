;; RESTARTING OF RING NOT REQUIRED FOR CHANGES TO THIS FILE.
(ns italianverbs.config
  (:require [italianverbs.lev :as lev]))

;; examples of constraints on generation.
;; these variables are useful for debugging and testing and perhaps later, user preferences.
;; Unfortunately, nested constraints (e.g. :root {:italian "tornare"}) do not work yet.

(def futuro-semplice {
;                      :italian "tornerai"
                      })

(def random-passato-prossimo-verb-past
  {
;   :italian "corretto"
;   :italian "venuto"
;   :italian "stato"
;   :number :plural
;   :person :1st
;   :italian "mangiato"
   })

(def random-passato-prossimo-subj
  {
;   :person :1st
;   :number :plural
;   :english "she"
   })

(def random-present-subj
  {
;   :person :1st
;   :number :singular
   })

(def random-present-inf
  {
;   :italian "aprire"
   })

(def random-infinitivo {})
