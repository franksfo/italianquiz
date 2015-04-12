(ns italianverbs.workbook
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [clojure.core :exclude [get-in]]
   [clojure.core :as core] ;; This allows us to use core's get-in by doing "(core/get-in ..)"
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.tools.logging :as log]

   [clojail.core :refer [sandbox]]
   [clojail.testers :refer :all]

   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]

   [hiccup.core :refer [html]]

   [italianverbs.engine :refer :all :exclude [routes]]
   [italianverbs.english :as en]
   [italianverbs.english :refer [en]]
   [italianverbs.espanol :as es]
   [italianverbs.italiano :as it]
   [italianverbs.italiano :refer [it]]
   [italianverbs.morphology :refer [fo fo-ps]]
   [italianverbs.over :refer [over]]
   [italianverbs.html :as html]
   [italianverbs.parse :refer [parse]]
   [italianverbs.pos :as pos]
   [italianverbs.translate :refer [translate translate-all]]
   [italianverbs.unify :refer [get-in remove-false strip-refs]]
))

;; this does some sample runtime behavior (generates sentences)
;; which allow certain things to get initialized so that remote (i.e. HTTP
;; requests to the same things will not fail with cryptic 'undefined'
;; stack traces.
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

                              (or (set? loaded) (seq? loaded)(vector? loaded))
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
          "(fo (take 1 (repeatedly #(it/sentence))))")
        ]
       [:button {:onclick "workbook()"} "evaluate"]]
      [:div#workbooka
       (if search-query
         (workbookq search-query))]])))

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
            (it/get-string (:italiano to-show))]
           [:td
            (en/get-string (:english to-show))]]

          [:tr
           [:td (html/tablize (remove-false (get-in to-show [:italiano :synsem :sem])))]
           [:td (html/tablize (remove-false (get-in to-show [:english :synsem :sem])))]]
    
          ])
        
        true
        (fo to-show)))

;; TODO: remove when I feel safe that I don't need it anymore..
;(def get-stuff-initialized1 (it/parse "io leggo il libro"))
;(def get-stuff-initialized1 (translate "il gatto"))
;(def get-stuff-initialized2 (translate "io leggo il libro"))

;(log/info (str "done initializing workbook(1): " (fo get-stuff-initialized1)))
;(log/info (str "done initializing workbook(2): " get-stuff-initialized2))

(def routes
  (compojure/routes

   (GET "" request
        {:status 302
         :body (html/page "Workbook" (workbook-ui request) request)
         :headers {"Location" "/workbook/"}})

   (GET "/" request
        {:status 200
         :body (html/page "Workbook" (workbook-ui request) request)
         :headers {"Content-Type" "text/html;charset=utf-8"}})

   (GET "/q/" request
        {:status 200
         :body (workbookq (get (get request :query-params) "search")
                          (get (get request :query-params) "attrs"))
         :headers {"Content-Type" "text/html;charset=utf-8"}})
  ))
