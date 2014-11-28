(ns italianverbs.english
  (:refer-clojure :exclude [get-in])
  (:require 
   [italianverbs.grammar.english :as gram]
   [italianverbs.lexicon.english :as lex]))

(def generate gram/generate)
(def grammar gram/grammar)
(def lexicon lex/lexicon)
(def lookup lex/lookup)
(def parse gram/parse)
