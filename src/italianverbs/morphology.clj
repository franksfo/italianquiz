(ns italianverbs.morphology
  (:refer-clojure :exclude [get-in str])
  (:use [italianverbs.unify :only (ref? get-in fail?)])
   (:require
    [clojure.core :as core]
    [clojure.tools.logging :as log]
    [clojure.string :as string]))
