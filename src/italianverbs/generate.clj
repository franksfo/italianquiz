(ns italianverbs.generate
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [clojure.set :refer (union)]
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]

   [italianverbs.cache :refer (build-lex-sch-cache) ]
   [italianverbs.forest :as forest]
   [italianverbs.grammar.english :as en]
   [italianverbs.grammar.italiano :as it]
   [italianverbs.grammar.english :as en]
   [italianverbs.grammar.italiano :as it]
   [italianverbs.html :as html]
   [italianverbs.lexicon :refer (lexicon it en)]
   [italianverbs.morphology :refer (fo fo-ps)]
   [italianverbs.over :refer :all]
   [italianverbs.ug :refer :all]
   [italianverbs.unify :refer (fail? get-in lazy-shuffle merge remove-top-values unify unifyc)]))

(defn printfs [fs & filename]
  "print a feature structure to a file. filename will be something easy to derive from the fs."
  (let [filename (if filename (first filename) "foo.html")]  ;; TODO: some conventional default if deriving from fs is too hard.
    (spit filename (html/static-page (html/tablize fs) filename))))

(defn plain [expr]
  "simply map expr in a map with one key :plain, whose value is expr.
   workbook/workbookq will format this accordingly."
  {:plain expr})

;; this rule-cache is defined outside any function so that all functions can share
;; a single cache.
;; TODO: move to italianverbs.grammar.english
(def english-rule-cache (conj (build-lex-sch-cache en/grammar lexicon en/grammar)
                              {:phrase-constraints head-principle})) ;; for now, only one constraint: ug/head-principle.

;; TODO: move to italianverbs.grammar.italiano
(def italiano-rule-cache (conj (build-lex-sch-cache it/grammar lexicon it/grammar)
                      {:phrase-constraints head-principle})) ;; for now, only one constraint: ug/head-principle.

;; TODO: use a map destructor to pass in arguments
(defn generate [ & [spec grammar the-lexicon cache]]
  (let [spec (if spec spec :top)
        grammar (if grammar grammar it/grammar)
        lexicon (if the-lexicon the-lexicon lexicon)
        cache (if cache cache italiano-rule-cache)] ;; if no cache supplied, use package-level cache 'rule-cache'.
  (log/debug (str "generate with lexicon size: " 
                  (.size the-lexicon) " and grammar size: "
                  (.size grammar) "."))
  (first (take 1 (forest/generate spec grammar lexicon cache)))))

(defn nounphrase [ & [ spec the-lexicon the-grammar cache ]]
  (let [spec (if spec spec :top)
        lexicon (if the-lexicon the-lexicon lexicon)
        grammar (if the-grammar the-grammar it/grammar)
        cache (if cache cache italiano-rule-cache)]
    (first (take 1 (forest/generate (unify spec {:synsem {:cat :noun :subcat '()}}) grammar lexicon cache)))))

(defn sentence [ & [spec ]]
  (let [sentence-spec {:synsem {:subcat '()
                                :cat :verb
                                :subj {:animate true}}}
                       
        spec (if spec spec :top)
        unified-spec (unifyc sentence-spec spec)]
    (let [italiano
          (generate unified-spec it/grammar lexicon it/cache)]
      (log/info (str "semantics of this italian sentence:" (get-in italiano [:synsem :sem])))
      (let [english
            (generate (unifyc unified-spec
                              {:synsem {:sem
                                        (get-in italiano [:synsem :sem])}})
                      en/grammar
                      lexicon
                      english-rule-cache)]
        {:italiano italiano
         :english english}))))

(defn sentence-en [ & [spec ]]
  (sentence spec lexicon en/grammar en/cache))

(defn sentence-it [ & [spec ]]
  (sentence spec lexicon it/grammar it/cache))

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
