(ns italianverbs.grammar-workshop
  (:refer-clojure :exclude [get-in resolve])
  (:use [clojure.set :only (union)]
        [clojure.core :exclude (get-in resolve)]
        [italianverbs.generate :only (generate moreover-head moreover-comp gen14)]
        [italianverbs.grammar]
        [italianverbs.lexicon :only (it1 lexicon it en)]
        [italianverbs.lexiconfn :only (unify sem-impl)]
        [italianverbs.morphology :only (finalize fo italian-article)]
        [italianverbs.unify :only (copy fail? serialize get-in resolve)]
        )

  (:require [clojure.tools.logging :as log]
            [italianverbs.lexicon :as lex]
            [italianverbs.unify :as unify]
            [clojure.string :as string])
)



