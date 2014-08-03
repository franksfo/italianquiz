(ns italianverbs.generate
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [clojure.set :refer (union)]
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]

   [italianverbs.cache :refer (build-lex-sch-cache) ]
   [italianverbs.forest :as forest]
   [italianverbs.grammar.english :as english]
   [italianverbs.grammar.italiano :as italiano]
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
(def english-rule-cache (conj (build-lex-sch-cache english/grammar lexicon english/grammar)
                      {:phrase-constraints head-principle})) ;; for now, only one constraint: ug/head-principle.

(def italiano-rule-cache (conj (build-lex-sch-cache italiano/grammar lexicon italiano/grammar)
                      {:phrase-constraints head-principle})) ;; for now, only one constraint: ug/head-principle.

(defn generate [ & [spec grammar the-lexicon cache]]
  (let [spec (if spec spec :top)
        grammar (if grammar grammar italiano/grammar)
        lexicon (if the-lexicon the-lexicon lexicon)
        cache (if cache cache italiano-rule-cache)] ;; if no cache supplied, use package-level cache 'rule-cache'.
  (log/debug (str "generate with lexicon size: " 
                  (.size the-lexicon) " and grammar size: "
                  (.size grammar) "."))
  (forest/generate spec grammar lexicon cache)))

(defn nounphrase [ & [ spec the-lexicon the-grammar cache ]]
  (let [spec (if spec spec :top)
        lexicon (if the-lexicon the-lexicon lexicon)
        grammar (if the-grammar the-grammar italiano/grammar)
        cache (if cache cache italiano-rule-cache)]
    (first (take 1 (forest/generate (unify spec {:synsem {:cat :noun :subcat '()}}) grammar lexicon cache)))))

(defn sentence [ & [spec the-lexicon the-grammar cache]]
  (let [sentence-spec {:synsem {:subcat '()
                                :cat :verb
                                :subj {:animate true}}}
                       
        spec (if spec spec :top)
        lexicon (if the-lexicon the-lexicon lexicon)
        grammar (if the-grammar the-grammar italiano/grammar)
        cache (if cache cache italiano-rule-cache)]
    (first (take 1 (generate (unifyc sentence-spec spec))))))

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
                                     lexicon italiano/grammar))
                                                                             
(log/info (str "done loading generate: " (fo get-stuff-initialized)))
