;; NO RESTARTING OF RING REQUIRED FOR CHANGES TO THIS FILE. (must reload browser 2x though).
(ns italianverbs.grammar
  (:use [rdutest])
  (:require
   [italianverbs.fs :as fs]
   [italianverbs.morphology :as morph]
   [italianverbs.lexiconfn :as lexfn]
   [clojure.string :as string]))

(def tests
  {:time-format
   (rdutest
    "Universal (non-localized) time format."
    (english-time 5 43 "pm")
    (fn [formatted-time] (= formatted-time "5:43"))
    :time-format)
   :random-lexeme
   (rdutest
    "Choose a random lexeme with no restrictions."
    (choose-lexeme {})
    (fn [lexeme] (not (= nil (:italian lexeme))))
    :random-lexeme)})




