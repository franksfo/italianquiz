(ns italianverbs.generate
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [clojure.set :refer (union)]
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]

   [italianverbs.cache :refer (build-lex-sch-cache) ]
   [italianverbs.forest :exclude (lightning-bolt) ]
   [italianverbs.forest :as forest]
   [italianverbs.grammar :refer :all]
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

;; no cache is used, so this version is relatively slow. use only with a small grammar and lexicon.
(defn lightning-bolt [spec & [input-lexicon input-grammar]]
  (let [input-grammar (if input-grammar input-grammar grammar)
        input-lexicon (if input-lexicon input-lexicon lexicon)]
    (log/debug (str "Grammar: " (fo-ps input-grammar)))
    (log/debug (str "Lexicon: " (fo lexicon)))
    (forest/lightning-bolt spec input-lexicon input-grammar)))

;; this rule-cache is defined outside any function so that all functions can share
;; a single cache.
(def rule-cache (conj (build-lex-sch-cache grammar lexicon grammar)
                      {:phrase-constraints head-principle})) ;; for now, only one constraint: ug/head-principle.

(defn generate [ & [head the-lexicon the-grammar cache]]
  (let [head (if head head :top)
        grammar (if the-grammar the-grammar grammar)
        lexicon (if the-lexicon the-lexicon lexicon)
        cache (if cache cache rule-cache)] ;; if no cache supplied, use package-level cache 'rule-cache'.
  (log/debug (str "generate with lexicon size: " 
                  (.size the-lexicon) " and grammar size: "
                  (.size the-grammar) "."))
    (first (take 1 (forest/lightning-bolt head
                                          (lazy-shuffle the-lexicon)
                                          (lazy-shuffle grammar) 0 cache)))))

(def use-hxcx true)

(defn nounphrase [ & [ spec the-lexicon the-grammar cache ]]
  (let [spec (if spec spec :top)
        lexicon (if the-lexicon the-lexicon lexicon)
        grammar (if the-grammar the-grammar grammar)
        cache (if cache cache rule-cache)]
    (first (take 1 (forest/hlcl cache grammar (unify spec {:synsem {:cat :noun :subcat '()}}))))))


(defn short-sentence [ & [ spec the-lexicon the-grammar cache ]]
  (let [spec (if spec spec :top)
        lexicon (if the-lexicon the-lexicon lexicon)
        grammar (if the-grammar the-grammar grammar)
        cache (if cache cache rule-cache)]
    (first (take 1 (forest/hlcp cache grammar (unify spec {:synsem {:subcat '() :cat :verb}}))))))

(defn sentence [ & [spec the-lexicon the-grammar cache]]
  (let [spec (if spec spec :top)
        lexicon (if the-lexicon the-lexicon lexicon)
        grammar (if the-grammar the-grammar grammar)
        cache (if cache cache rule-cache)]
    (if use-hxcx
      (short-sentence spec)
      (do
        (log/debug (str "sentence with lexicon size: " 
                        (.size lexicon) " and grammar size: "
                        (.size grammar) "."))
        (log/debug (str "cache:"
                        (if cache
                          (str "size: " (.size cache))
                          (str "doesn't exist yet."))))
        (generate (unify spec {:synsem {:cat (first (shuffle (list :verb :sent-modifier)))}})
                  lexicon
                  grammar
                  cache)))))


;(def get-stuff-initialized (take 1 (forest/hlcp rule-cache grammar {:synsem {:subcat '() :cat :verb}})))
;(log/info (str "done loading generate: " (fo get-stuff-initialized)))
