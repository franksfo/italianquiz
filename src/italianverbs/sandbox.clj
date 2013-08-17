(ns italianverbs.sandbox
  (:refer-clojure :exclude [get-in merge resolve find])
  (:use
   [italianverbs.lexicon]
   ;; Prohibit generate/printfs because it writes directly to the filesystem:
   ;; attacker could DOS server by filling up filesystem.
   ;; Also exclude 'generate' so that we can define a wrapper for it in the sandbox,
   ;; rather than using it directly.
   [italianverbs.generate :exclude [printfs]]
   [italianverbs.grammar]
   [italianverbs.test.grammar]
   [italianverbs.html]
   [italianverbs.morphology]
   [clojail.core :only [sandbox]]
   [clojail.testers])
  (:require
   [italianverbs.generate :as gen]
   [italianverbs.grammar :as gram]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.unify :as fs]
   [italianverbs.html :as html]
   [clojure.set :as set]
   [italianverbs.test.generate :as tgen]
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
    (blacklist-nses '[clojure.main
                      java
                      javax
                      org.omg
                      org.w3c
                      org.xml
                      ])
    (blacklist-objects [clojure.lang.Compiler
                        clojure.lang.Ref
                        clojure.lang.Reflector
                        clojure.lang.Namespace
                        clojure.lang.Var clojure.lang.RT]))
   ;; TODO: make this configurable:
   ;;   might want to have a value for production usage lower/higher than
   ;;   for development usage.
   :timeout 1000000 ;; for development, set high (1,000,000 = 1000 seconds)
   :namespace 'italianverbs.sandbox))
