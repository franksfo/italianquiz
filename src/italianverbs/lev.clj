(ns italianverbs.lev
  (:require
   [italianverbs.grammar :as gram]))

(defn matrix []
  (gram/choose-lexeme {}))
