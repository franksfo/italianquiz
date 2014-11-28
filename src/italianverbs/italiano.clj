(ns italianverbs.italiano
  (:refer-clojure :exclude [get-in])
  (:require 
   [italianverbs.grammar.italiano :as gram]
   [italianverbs.lexicon.italiano :as lex]))

(def generate gram/generate)
(def grammar gram/grammar)
(def lexicon lex/lexicon)
(def lookup lex/lookup)
(def parse gram/parse)

