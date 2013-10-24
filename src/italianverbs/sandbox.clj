(ns italianverbs.sandbox
  (:refer-clojure :exclude [get-in merge resolve find])
  (:use
   [italianverbs.lexicon :only (it)]
   ;; Prohibit generate/printfs because it writes directly to the filesystem:
   ;; attacker could DOS server by filling up filesystem.
   ;; Also exclude 'generate' so that we can define a wrapper for it in the sandbox,
   ;; rather than using it directly.
   [italianverbs.generate :exclude [printfs]]
   [italianverbs.grammar]
   [italianverbs.lexiconfn]
   [italianverbs.test.lexicon]
   [italianverbs.html]
   [italianverbs.morphology]
   [italianverbs.rules]
   [italianverbs.ug]
   [italianverbs.unify :only (fail? fail-path)]
   [clojail.core :only [sandbox]]
   [clojail.testers])
  (:require
   [italianverbs.generate :as gen]
   [italianverbs.grammar :as gram]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.unify :as fs]
   [italianverbs.html :as html]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.tools.logging :as log]))

;; Sandbox specification derived from:
;;    https://github.com/flatland/clojail/blob/4d3f58f69c2d22f0df9f0b843c7dea0c6a0a5cd1/src/clojail/testers.clj#L76
;;    http://docs.oracle.com/javase/6/docs/api/overview-summary.html
;;    http://richhickey.github.com/clojure/api-index.html

(def workbook-sandbox
  (sandbox
   (conj
    clojail.testers/secure-tester-without-def
    (blacklist-nses '[
;                      clojure.main
;                      java
;                      javax
;                      org.omg
;                      org.w3c
;                      org.xml
                      ])
    (blacklist-objects [
;                        clojure.lang.Compiler
;                        clojure.lang.Ref
;                        clojure.lang.Reflector
;                        clojure.lang.Namespace
;                        clojure.lang.Var clojure.lang.RT
                        ]))
   ;; TODO: make this configurable:
   ;;   might want to have a value for production usage lower/higher than
   ;;   for development usage.
   :timeout 10000 ;; for development, set high (1,000,000 = 1000 seconds)
   :namespace 'italianverbs.sandbox))

;; working on:
;; (workbook-sandbox (read-string "(generate sents 'sents' :top sem-impl)"))

;; does not yet work (but ultimate goal) : (workbook-sandbox (read-string "(sentence)"))
;; this works for some reason:

;(workbook-sandbox (read-string "(fo (take 1 (gen-all (shuffle sents) \"sents\" :top sem-impl)))"))
;; non sandbox:
(fo (take 1 (gen-all (shuffle sents) "sents" :top sem-impl)))

