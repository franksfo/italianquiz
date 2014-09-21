(ns italianverbs.workbook
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [clojure.core :exclude [get-in]]
   [clojure.core :as core] ;; This allows us to use core's get-in by doing "(core/get-in ..)"
   [clojure.set :refer :all]
   [clojure.string :as string]

   [clojail.core :refer [sandbox]]
   [clojail.testers :refer :all]
   [clojure.tools.logging :as log]
   [hiccup.core :refer :all]

   [italianverbs.benchmark :refer :all]
   [italianverbs.forest :as forest]
   [italianverbs.forest :refer :all :exclude [lightning-bolt unifyc deref future generate rand-int]]

   [italianverbs.game :as game]

   [italianverbs.generate :as generate :refer :all :exclude [lightning-bolt sentence]]
   [italianverbs.grammar.english :as en]
   [italianverbs.grammar.italiano :as it]
   [italianverbs.html :as html]
   [italianverbs.lexicon :refer :all]
   [italianverbs.lexiconfn :refer :all]
   [italianverbs.morphology :refer [finalize fo fo-ps fo-ps-en fo-ps-it get-english get-italian]]
   [italianverbs.over :refer :all]
   [italianverbs.pos :refer :all]
   ;; we excluded lightning-bolt from italianverbs.forest, so that we can use italianverbs.test.forest's instead:
;;   [italianverbs.test.forest :refer :all]
   [italianverbs.ug :refer :all]
   [italianverbs.unify :refer :all :exclude [unify]]))

;(def avoid-init-errors (nounphrase))
(def avoid-init-errors true)

;; TODO: add user convenience functions: functions that might be useful for workbook users.

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
   ;; using 60000 for development: for production, use much smaller value.
   :timeout 60000
;   :timeout 15000
   :namespace 'italianverbs.workbook))


;; TODO: some exceptions from evaluating a string should be shown to
;; the user for diagnostics rather than just logging them.
;; (e.g. those thrown by (max-lengths).)
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

                              (set? loaded)
                              (html/tablize loaded)

                              (or (set? loaded) (seq? loaded))
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
          "(fo (take 1 (repeatedly #(sentence))))")
        ]
       [:button {:onclick "workbook()"} "evaluate"]]
      [:div#workbooka
       (if search-query
         (workbookq search-query))]])))

(defn sentence [ & [spec it-grammar]]
  (let [spec (if spec spec :top)
        it-grammar (if it-grammar it-grammar it/grammar)]
    (generate/sentence spec en/grammar it-grammar en/cache it/cache)))

(defn show-sem [to-show]
  (cond (seq? to-show)
        (map (fn [each]
               (show-sem each))
             to-show)
        
        (map? to-show)
        (html
         [:table
          
          [:tr
           [:th "it"] [:th "en"]]

          [:tr
           [:td
            (fo (:italiano to-show))]
           [:td
            (fo (:english to-show))]]
          
          [:tr
           [:td
            (fo-ps-it (:italiano to-show))]
           [:td
            (fo-ps-en (:english to-show))]]

          [:tr
           [:td (html/tablize (remove-false (get-in to-show [:italiano :synsem :sem])))]
           [:td (html/tablize (remove-false (get-in to-show [:english :synsem :sem])))]]
    
          ])
        
        true
        (fo to-show)))



;; TODO: remove when I feel safe that I don't need it anymore..
(def get-stuff-initialized (sentence {:comp {:phrasal false}
                                      :head {:phrasal false}
                                      :synsem {:subcat '() :cat :verb
                                              :sem {:pred :sognare
                                                     :subj {:pred :lei}}}}
                                     it/grammar))

;(log/info (str "done initializing workbook. " (fo get-stuff-initialized)))
