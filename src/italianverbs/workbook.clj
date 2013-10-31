(ns italianverbs.workbook
  (:refer-clojure :exclude [get-in merge resolve find])
  (:require
   [clojail.core :refer [sandbox]]
   [clojail.testers :refer :all]
   [clojure.set :refer :all]
   [clojure.core :exclude [get-in]]
   [clojure.core :as core] ;; This allows us to use core's get-in by doing "(core/get-in ..)"
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [hiccup.core :refer :all]
   [italianverbs.generate :refer :all]
   [italianverbs.grammar :refer :all]
   [italianverbs.html :as html]
   [italianverbs.lexicon :refer :all]
   [italianverbs.lexiconfn :refer :all]
   [italianverbs.morphology :refer [finalize fo]]
   [italianverbs.rules :refer :all]
   [italianverbs.ug :refer :all]
   [italianverbs.unify :refer :all :exclude [unify]]
   [somnium.congomongo :as mongo]))

;; seem to need this sometimes, to avoid initialization errors:
(def populate-ns (sentence))

;; tree-building functions: useful for developing grammars.

(defn into-list-of-maps [arg]
  (cond

   (seq? arg)
   arg

   (set? arg)
   (seq arg)

   (map? arg)
   (list arg)

   (string? arg)
   (seq (it arg))

   (nil? arg)
   (list :top)

   (keyword? arg)
   (list arg)

   true (throw (Exception. (str "into-map: don't know what to do with a " (type arg) ".")))))

;; TODO: move all of these (over-X) functions to their own namespace.
(defn over-each-parent [parents child1 child2]
  (if (not (empty? parents))
    (let [parent (first parents)]
      (log/info (str "parent: " parent))
      (lazy-cat
       (cond (and (map? parent)
                  (not (nil? (:serialized parent))))
             ;; In this case, supposed 'parent' is really a lexical item: for now, definition of 'lexical item' is,
             ;; it has a non-nil value for :serialized - just return nil, nothing else to do.
             nil

             (and (map? parent)
                  (not (nil? (:schema parent))))
                  ;; figure out whether head is child1 or child2:
             (let [head
                   (cond
                    (= \c (nth (str (:schema parent)) 0))
                    child2

                    (= \h (nth (str (:schema parent)) 0))
                    child1

                    true
                    (throw (Exception. (str "Don't know what the head-vs-complement ordering is for parent: " parent))))
                   comp
                   (if (= head child1)
                     child2 child1)]
               (generate (list parent)
                         "parent" {:head head
                                   :comp comp} sem-impl))

             ;; if parent is a symbol, evaluate it; should evaluate to a list of expansions (which might also be symbols, etc).
             (symbol? parent)
             (over-each-parent (eval parent) child1 child2)

             ;; if parent is map, do introspection: figure out the schema from the :schema-symbol attribute,
             ;; and figure out head-comp ordering from :first attribute.
             (and (map? parent)
                  (not (nil? (:schema-symbol parent))))
             (generate (list (eval (:schema-symbol parent)))
                       "parent"
                       {:head (if (= (:first parent) :head)
                                child1 child2)
                        :comp (if (= (:first parent) :head)
                                child2 child1)}
                       sem-impl)


             true
             (throw (Exception. (str "Don't know what to do with parent: " parent))))

       (over-each-parent (rest parents) child1 child2)))))

(defn over-each-child2 [parents child1s child2]
  (if (not (empty? child1s))
    (lazy-cat
     (over-each-parent parents (first child1s) child2)
     (over-each-child2 parents (rest child1s) child2))))

(defn over-each-child1 [parents child1s child2s]
  (if (not (empty? child2s))
    (lazy-cat
     (over-each-child2 parents child1s (first child2s))
     (over-each-child1 parents child1s (rest child2s)))))

(defn over [parents child1 & [child2]]
  (let [child1 (into-list-of-maps child1)
        child2 (into-list-of-maps child2)]
    (over-each-child1 (if (seq? parents)
                        (shuffle parents)
                        (list parents))
                      child1 child2)))

(defn demo-generation []
  (fo (take 1 (over np {:synsem {:gender :masc :number :sing}} {:synsem {:sem {:human true}}})))
  (fo (take 1 (over sents {:synsem {:agr {:gender :masc}}} "dormire"))))

(fo (take 1 (over sents {:synsem {:agr {:gender :masc}}} "dormire")))

;; Sandbox specification derived from:
;;    https://github.com/flatland/clojail/blob/4d3f58f69c2d22f0df9f0b843c7dea0c6a0a5cd1/src/clojail/testers.clj#L76
;;    http://docs.oracle.com/javase/6/docs/api/overview-summary.html
;;    http://richhickey.github.com/clojure/api-index.html
(def workbook-sandbox
  (sandbox
   (conj
    clojail.testers/secure-tester-without-def
    (blacklist-nses '[
                      clojure.main
                      java
                      javax
                      org.omg
                      org.w3c
                      org.xml
                      ])
    (blacklist-objects [
                        clojure.lang.Compiler
                        clojure.lang.Ref
                        clojure.lang.Reflector
                        clojure.lang.Namespace
                        clojure.lang.Var clojure.lang.RT
                        ]))
   :refer-clojure false
   ;; for development-only: for production, use much smaller value.
   :timeout 10000
   :namespace 'italianverbs.workbook))
(defn workbookq [expr notused]
  (do
    ;; TODO: add timing information for each evaluation.
    (log/info (str "workbookq: evaluating expression: \"" expr "\""))
    (if expr
      (let [output
            (string/join " "
                         (let [loaded
                               (try
                                 (workbook-sandbox (read-string expr))
                                 ;; TODO: how can I show the stack trace for the
                                 ;; attempt to process the expression?
                                 (catch Exception e
                                   (log/error (str "failed to sandbox-load-string: " expr ":" e))
                                   (str e)))]
                           (list
                            (str
                             "<div class='evalinput'>"
                             expr
                             "</div>"
                             "<div class='evalresult'>"
                             (cond

                              (and (or (set? loaded) (seq? loaded))
                                   (> (.size loaded) 1))
                              (str "<ol class='workbook'>"
                                   (string/join " "
                                                (map (fn [elem]
                                                       (str "<li>" (html/tablize elem) "</li>"))
                                                     (seq loaded)))
                                   "</ol>")

                              (= (type loaded)
                                 clojure.lang.LazySeq)
                              (string/join " "
                                           (map (fn [elem]
                                                  (html/tablize elem))
                                                (seq loaded)))

                              (or (list? loaded) (set? loaded)
                                  (= (type loaded) clojure.lang.Cons))
                              (string/join " "
                                           (map (fn [elem]
                                                  (html/tablize elem))
                                                loaded))
                              (= (type loaded) clojure.lang.Var)
                              (str (eval loaded))
                              (and (map? loaded)
                                   (= (keys loaded) '(:plain)))
                              (str "<div style='font-family:monospace'>" (strip-refs (:plain loaded)) "</div>")
                              (map? loaded)
                              (html/tablize loaded)
                              (= (type loaded) nil)
                              (str "<b>nil</b>")
                              :else
                              ;; nothing formattable: just stringify result of
                              ;; evaluation.
                              (str "<div style='font-family:monospace'>" loaded " (<b>" (type loaded) "</b>)" "</div>"))
                             "</div>"))))]
        (log/info (str "workbookq: done evaluating: \"" expr "\""))
        output))))

(defn workbook-ui [request]
  (let [search-query (get (get request :query-params) "search")]
    (html
     [:div#workbook-ui {:class "quiz-elem"}
      [:h2 "Libro di Lavoro"]
      [:div#searchbar
       [:textarea {:cols 80 :rows 4 :id "workbookq" }
        (if search-query
          search-query
          "(fo (sentence)))"
          )]
       [:button {:onclick "workbook()"} "evaluate"]]
      [:div#workbooka
       (if search-query
         (workbookq search-query))]])))
