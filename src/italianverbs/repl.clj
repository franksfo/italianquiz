;; HOWTO
; $ lein repl
; user=> (load "italianverbs/repl")
; user=> (ns italianverbs.generate)
; ;;;; namespace warnings about the 'test' symbol..
; italianverbs.generate=> (random-futuro-semplice)
; {:root {:infl "infinitive", :cat "verb", :subj ...
; ..etc
; ;;; to use italianverbs.generate/test, you must explicitly load "generate" again:
; italianverbs.generate=> (load "generate")
; italianverbs.generate=> (ns italianverbs.generate)
; italianverbs.generate=> (test)
; ..test output..

(ns italianverbs.generate
  (:use [somnium.congomongo])
  (:require [somnium.congomongo :as mongo]))

(mongo! :db "mydb")
(load "generate")

