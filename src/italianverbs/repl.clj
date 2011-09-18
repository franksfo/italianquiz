;; HOWTO
; $ lein repl
; user=> (load "italianverbs/repl")
; user=> (ns italianverbs.generate)
; italianverbs.generate=> (load "generate")
; ;;;; namespace warnings about the 'test' symbol..
; italianverbs.generate=> (random-futuro-semplice)
; {:root {:infl "infinitive", :cat "verb", :subj ...
; ..etc

(ns italianverbs.generate
  (:use [somnium.congomongo]))

(mongo! :db "mydb")


