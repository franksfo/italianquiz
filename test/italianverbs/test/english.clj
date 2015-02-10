(ns italianverbs.test.english
  (:refer-clojure :exclude [lookup])
  (:require
   [clojure.test :refer :all]
   [italianverbs.english :refer :all]
   [italianverbs.engine :as engine]
   [italianverbs.lexicon.english :as lex]
   [italianverbs.lexiconfn :refer :all]
   [italianverbs.morphology.english :as morph]
   ))


