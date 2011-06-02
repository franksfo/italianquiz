;; RESTARTING OF RING NOT REQUIRED FOR CHANGES TO THIS FILE.
(ns italianverbs.config)


;; which tests to run : set to true to run.
(def sentence false)
(def present false)
(def passato true)
(def mobili true)
(def espressioni true)

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
   :person :1st
   :number :plural
;   :english "she"
   })

(def random-present-verb
  {
;   :person :3rd
;   :number :singular
;   :root.italian "essere"
   })



