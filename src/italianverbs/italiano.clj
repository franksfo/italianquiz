(ns italianverbs.italiano
  (:refer-clojure :exclude [get-in])
  (:require 
   [italianverbs.grammar.italiano :as gram]
   [italianverbs.lexicon.italiano :as lex]
   [italianverbs.morphology.italiano :as morph]))

(def generate gram/generate)
(def get-string morph/get-string)
(def grammar gram/grammar)
(def lexicon lex/lexicon)
(def lookup lex/lookup)
(def parse gram/parse)
(def sentence gram/sentence)

