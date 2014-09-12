(ns italianverbs.generate
  (:refer-clojure :exclude [get-in merge])
  (:require
   [clojure.set :refer (union)]
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]

   [italianverbs.forest :as forest]
   [italianverbs.lexicon :refer (lexicon it en)]
   [italianverbs.morphology :refer (fo fo-ps)]
   [italianverbs.over :refer :all]
   [italianverbs.ug :refer (head-principle)]
   [italianverbs.unify :refer (fail? get-in lazy-shuffle remove-top-values unify unifyc)]))

(declare generate-from)

(defn sentence [ & [spec ]]
  (if (seq? spec)
    (map (fn [each]
           (sentence each))
         spec)
    (let [sentence-spec {:synsem {:subcat '()
                                  :cat :verb
                                  :subj {:animate true}}};; TODO: why :animate:true? - consider eliminating this.
          spec (if spec spec :top)
          unified-spec (unifyc sentence-spec spec)]
      (log/info (str "generating with unified spec: " unified-spec))
      (generate-from unified-spec))))

(defn nounphrase [ & [ spec ]]
  (let [sentence-spec {:synsem {:subcat '()
                                :cat :noun}}
        spec (if spec spec :top)
        unified-spec (unifyc sentence-spec spec)]
    (generate-from unified-spec)))

;; TODO: use a map destructor to pass in arguments
(defn generate [ spec grammar the-lexicon cache ]
  (log/debug (str "generate with lexicon size: " 
                  (.size the-lexicon) " and grammar size: "
                  (.size grammar) "."))
  (first (take 1 (forest/generate spec grammar lexicon cache))))

(defn generate-all [ spec grammar the-lexicon cache ]
  (log/debug (str "generate with lexicon size: "
                  (.size the-lexicon) " and grammar size: "
                  (.size grammar) "."))
  (let [result
        (forest/generate spec grammar lexicon cache)]
    (log/info (str "generated this many: " (.size result)))
    result))

;; This sentence generation prevents initialization errors that occur when trying to
;; generate sentences within the sandbox.
;; TODO: move to a sandbox-initialization-specific area.
;(def get-stuff-initialized (sentence {:comp {:phrasal false}
;                                      :head {:phrasal true}
;                                      :synsem {:subcat '() :cat :verb
;                                               :sem {:pred :parlare
;                                                     :subj {:pred :lei}}}}
;                                     lexicon it/grammar))
                                                                             
;(log/info (str "done loading generate: " (fo get-stuff-initialized)))
