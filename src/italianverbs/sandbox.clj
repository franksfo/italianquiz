(ns italianverbs.sandbox
  [:use
   [clojure.core :exclude [find]]
   [italianverbs.lexiconfn]
   [italianverbs.lexicon]
   [italianverbs.morphology]
   [italianverbs.generate]
   [italianverbs.grammar]
   [clojail.core :only [sandbox]]
   [clojail.testers]
   ]
  [:require
   [italianverbs.fs :as fs]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.tools.logging :as log]])

(def workbook-sandbox (sandbox

                       (conj
                        clojail.testers/secure-tester-without-def
                        ;; taken from: http://docs.oracle.com/javase/6/docs/api/overview-summary.html
                        ;; http://richhickey.github.com/clojure/api-index.html#
                        (blacklist-nses '[clojure
                                          java
                                          javax
                                          org.omg
                                          org.w3c
                                          org.xml
                                          ]))
                       :timeout 5000
                       :namespace 'italianverbs.sandbox))

(defn sandbox-load-string [expression]
  (workbook-sandbox (read-string expression)))
