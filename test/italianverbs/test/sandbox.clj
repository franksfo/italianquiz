(ns italianverbs.test.sandbox
  (:refer-clojure :exclude [get-in merge resolve find])
  (:use [clojure.test]
        [italianverbs.workbook]))

(ns italianverbs.sandbox
  (:refer-clojure :exclude [get-in merge resolve find])
  (:use
   [italianverbs.lexicon]
   ;; Prohibit generate/printfs because it writes directly to the filesystem:
   ;; attacker could DOS server by filling up filesystem.
   ;; Also exclude 'generate' so that we can define a wrapper for it in the sandbox,
   ;; rather than using it directly.
   [italianverbs.generate :exclude [printfs]]
   [italianverbs.html]
   [italianverbs.morphology]
   [clojail.core :only [sandbox]]
   [clojure.test]
   [clojail.testers])
  (:require
   [italianverbs.generate :as gen]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.unify :as fs]
   [italianverbs.html :as html]
   [clojure.set :as set]
;;   [italianverbs.test.generate :as tgen]
   [clojure.string :as string]
   [clojure.tools.logging :as log]))

;; TODO: add tests.
(deftest no-tests-yet
  (is (= 42 42)))


