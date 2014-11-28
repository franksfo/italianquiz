(ns italianverbs.english
  (:refer-clojure :exclude [get-in])
  (:require 
   [italianverbs.grammar.english :as gram]
   [italianverbs.lexicon.english :as lex]
   [italianverbs.morphology.english :as morph]))

(def generate gram/generate)
(def get-string morph/get-string)
(def grammar gram/grammar)
(def lexicon lex/lexicon)
(def lookup lex/lookup)
(def parse gram/parse)
(def sentence gram/sentence)

