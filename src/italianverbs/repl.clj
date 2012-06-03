;; HOWTO
; $ lein repl
; user=> (load "italianverbs/repl")
; user=> (ns italianverbs.generate)
; ;;;; namespace warnings about the 'test' symbol..
; ;;; to use italianverbs.X, you must explicitly load "X" again:
; italianverbs.generate=> (load "generate")
; italianverbs.generate=> (ns italianverbs.generate)
; italianverbs.generate=> (random-futuro-semplice)
; {:root {:infl "infinitive", :cat "verb", :subj ...
; ..etc

(ns italianverbs.generate
  (:use [somnium.congomongo]
        [hiccup core])
  (:require
   [italianverbs.morphology :as morph]
   [italianverbs.lev :as lev]
   [italianverbs.grammar :as gram]
   [italianverbs.html :as html]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.config :as config]
   [clojure.string :as string])
  (:require [somnium.congomongo :as mongo]))

(mongo! :db "mydb")
(load "generate")

