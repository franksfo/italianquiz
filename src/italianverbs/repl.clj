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


(ns italianverbs.quiz
  (:use [somnium.congomongo])
  (:require [somnium.congomongo :as mongo]))

(mongo! :db "mydb")
(load "generate")

