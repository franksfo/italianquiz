(ns italianverbs.test.populate
  (:refer-clojure :exclude [get-in])
  (:require
   [clojure.data.json :as json]
   [clojure.test :refer :all]
   [italianverbs.engine :refer [generate]]
   [italianverbs.italiano :as it]
   [italianverbs.morphology.italiano :as itm]
   [italianverbs.morphology :refer [fo]]
   [italianverbs.populate :refer :all]
   [italianverbs.unify :as unify :refer (fail? get-in strip-refs unify)]
   [korma.core :as k]))


;; TODO: add tests

