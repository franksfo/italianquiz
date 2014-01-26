(ns italianverbs.generate
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [clojure.set :refer (union)]
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]

   [italianverbs.forest :exclude (lightning-bolt) ]
   [italianverbs.forest :as forest]
   [italianverbs.html :as html]
   [italianverbs.lexicon :refer (lexicon it en)]
   [italianverbs.lexiconfn :exclude (unify)]
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

(defn generate [grammar lexicon cache & [head depth] ]
  (let [maxdepth 2
        depth (if depth depth 0)
        head (if head head :top)
        cache (if cache cache (forest/build-lex-sch-cache grammar lexicon))]
    (forest/lightning-bolt head lexicon grammar depth cache)))

;; see example usage in rules.clj.
(defmacro rewrite-as [name value]
  (if (ns-resolve *ns* (symbol (str name)))
    `(def ~name (cons ~value ~name))
    `(def ~name (list ~value))))
