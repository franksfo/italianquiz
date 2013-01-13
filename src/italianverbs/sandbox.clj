(ns italianverbs.sandbox
  [:use
   [clojure.core :exclude [find]]
   [italianverbs.lexiconfn]
   [italianverbs.lexicon]
   [italianverbs.generate]
   [italianverbs.grammar]
   [italianverbs.morphology]
   [clojail.core :only [sandbox]]
   [clojail.testers]
   ]
  [:require
   [italianverbs.fs :as fs]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.tools.logging :as log]])

;; Sandbox specification derived from:
;;   https://github.com/flatland/clojail/blob/4d3f58f69c2d22f0df9f0b843c7dea0c6a0a5cd1/src/clojail/testers.clj#L76
;; and:
;;    http://docs.oracle.com/javase/6/docs/api/overview-summary.html
;; and:
;;   http://richhickey.github.com/clojure/api-index.html

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
   :timeout 5000
   :namespace 'italianverbs.sandbox))

(defn sandbox-load-string [expression]
  (workbook-sandbox (read-string expression)))
