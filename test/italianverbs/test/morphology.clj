;; RESTARTING OF RING REQUIRED FOR CHANGES TO THIS FILE. (maybe not actually)
(ns italianverbs.morphology
  (:use [rdutest])
  (:require
   [italianverbs.fs :as fs]
   [clojure.contrib.logging :as log]
   [clojure.string :as string]
   [clojure.contrib.string :as stringc]
   [clojure.contrib.str-utils2 :as str-utils]))

;(defn test []
;  (list
;   {:comment "stem verb for futuro semplice"
;    :test (stem-per-futuro "tornare")}))
(def tests
  {:stem-for-futuro
   (rdutest
    "Stem verb for futuro semplice"
    (stem-per-futuro "tornare")
    (fn [future-stem] (= future-stem "torner"))
    :stem-for-futuro)

   :io-mangio ; regular conjugation
   (rdutest
    "Conjugate 'io' + 'mangiare' => 'io mangio'"
    (conjugate-italian-verb {:infl "infinitive", :cat "verb", :italian "mangiare", :english "to eat"}
                            {:italian "io", :english "i", :person :1st, :number :singular})
    (fn [string]
      (= string " mangio"))
    :io-mangio)

   :io-preferisco ; -isco conjugation
   (rdutest
    "Conjugate 'io' + 'prefire' => 'io  preferisco'"
    (conjugate-italian-verb {:infl "infinitive", :cat "verb", :isco true :italian "preferire", :english "to prefer"}
                            {:italian "io", :english "i", :person :1st, :number :singular})
    (fn [string]
      (= string " preferisco"))
    :io-facio)

   :en-plural-1 ; english noun pluralization.
   (rdutest
    "Conjugate a noun to plural"
    (plural-en "girl")
    (fn [string]
      (= string "girls"))
    :en-plural-1)
   
   :en-plural-2 ; english noun pluralization.
   (rdutest
    "Conjugate another noun to plural"
    (plural-en "box")
    (fn [string]
      (= string "boxes"))
    :en-plural-2)

   :it-noun-pluralization-1 ;
   (rdutest
    "cane->cani"
    (plural-masc "cane")
    (fn [string]
      (= string "cani"))
    :it-noun-pluralization-1)

   
   :passato
   (rdutest
    "Regular passato-prossimo conjugation."
    (passato-prossimo "lavorare")
    (fn [verb]
      (= verb "lavorato"))
    :passato)
   
   })
