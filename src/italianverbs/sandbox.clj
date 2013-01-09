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
                        (blacklist-nses '[clojure.java.io
                                          java.net
                                          ]))
                       :timeout 5000
                       :namespace 'italianverbs.sandbox))

(defn sandbox-load-string [expression]
  (workbook-sandbox (read-string expression)))
