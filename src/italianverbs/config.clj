;; RESTARTING OF RING NOT REQUIRED FOR CHANGES TO THIS FILE.
(ns italianverbs.config
  (:require [italianverbs.lev :as lev]))

; All tests.
;(def tests '(gram/sentence gen/random-present gen/random-passato-prossimo
;                              gen/mobili gen/espressioni))


; All tests.
(def tests '(lev/matrix gen/random-present gen/random-passato-prossimo gen/mobili gen/espressioni random-infinitivo))

;; examples of constraints on generation.

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
