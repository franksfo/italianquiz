;; RESTARTING OF RING NOT REQUIRED FOR CHANGES TO THIS FILE.
(ns italianverbs.config)


;; which tests to run : set to true to run.
(def sentence true)
(def present true)
(def passato true)
(def mobili true)

;; examples of constraints on generation.

(def random-passato-prossimo-verb-past
  {
;   :italian "corretto"
;   :italian "venuto"
;   :italian "stato"
   })

(def random-passato-prossimo-subj
  {
   })

(def random-present-verb
  {
;   :person :3rd
;   :number :singular
;   :root.italian "essere"
   })



