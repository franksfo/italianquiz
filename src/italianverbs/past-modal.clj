(ns italianverbs.past-modal
  (:refer-clojure :exclude [get-in merge resolve find])
  (:require
   [italianverbs.workbook :refer :all]
   [clojure.core :as core]
   [italianverbs.generate :refer :all]
   [italianverbs.grammar :refer :all]
   [italianverbs.lexicon :refer :all]
   [italianverbs.lexiconfn :refer :all]
   [italianverbs.morphology :refer [finalize fo]]
   [italianverbs.ug :refer :all]
   [italianverbs.unify :refer :all :exclude [unify]]))

(def want-to-sleep (take 1 (over modal-vp {:synsem {:sem {:obj {:pred :dormire} :pred :volere}}})))

