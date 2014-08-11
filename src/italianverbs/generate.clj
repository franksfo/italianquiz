(ns italianverbs.generate
  (:refer-clojure :exclude [get-in merge])
  (:require
   [clojure.set :refer (union)]
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]

   [italianverbs.cache :refer (build-lex-sch-cache) ]
   [italianverbs.forest :as forest]
   [italianverbs.grammar.english :as en]
   [italianverbs.grammar.italiano :as it]
   [italianverbs.lexicon :refer (lexicon it en)]
   [italianverbs.morphology :refer (fo fo-ps)]
   [italianverbs.over :refer :all]
   [italianverbs.ug :refer (head-principle)]
   [italianverbs.unify :refer (fail? get-in lazy-shuffle remove-top-values unify unifyc)]))

;; TODO: use a map destructor to pass in arguments
(defn generate [ & [spec grammar the-lexicon cache]]
  (let [spec (if spec spec :top)
        grammar (if grammar grammar it/grammar)
        lexicon (if the-lexicon the-lexicon lexicon)
        cache (if cache cache it/cache)] ;; if no cache supplied, use package-level cache 'rule-cache'.
  (log/debug (str "generate with lexicon size: " 
                  (.size the-lexicon) " and grammar size: "
                  (.size grammar) "."))
  (first (take 1 (forest/generate spec grammar lexicon cache)))))

(defn generate-from [spec]
  (let [italiano
        (generate spec it/grammar lexicon it/cache)]
    {:italiano italiano
     :english
     (generate (unifyc spec
                       {:synsem {:sem
                                 (get-in italiano [:synsem :sem])}})
               en/grammar
               lexicon
               en/cache)}))

(defn nounphrase [ & [ spec ]]
  (let [sentence-spec {:synsem {:subcat '()
                                :cat :noun}}
        spec (if spec spec :top)
        unified-spec (unifyc sentence-spec spec)]
    (generate-from unified-spec)))

(defn sentence [ & [spec ]]
  (let [sentence-spec {:synsem {:subcat '()
                                :cat :verb
                                :subj {:animate true}}};; TODO: why :animate:true? - consider eliminating this.
        spec (if spec spec :top)
        unified-spec (unifyc sentence-spec spec)]
    (generate-from unified-spec)))

;; This sentence generation prevents initialization errors that occur when trying to
;; generate sentences within the sandbox.
;; TODO: move to a sandbox-initialization-specific area.
(def get-stuff-initialized (sentence {:comp {:phrasal false}
                                      :head {:phrasal true}
                                      :synsem {:subcat '() :cat :verb
                                               :sem {:pred :parlare
                                                     :subj {:pred :lei}}}}
                                     lexicon it/grammar))
                                                                             
(log/info (str "done loading generate: " (fo get-stuff-initialized)))
