(ns italianverbs.workbook
  (:use [hiccup core]
        [clojure.set])
  (:require
   [somnium.congomongo :as mongo]
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [italianverbs.html :as html]
   [italianverbs.sandbox :as sandbox]
   [italianverbs.lev :as lev]))

(defn workbookq [expr attrs]
  (do
    ;; TODO: add timing information for each evaluation.
    (log/info (str "workbookq: evaluating expression: " expr))
    (if expr
      (let [output
            (string/join " "
                         (let [loaded
                               (try
                                 (sandbox/sandbox-load-string expr)
                                 (catch Exception e
                                   (log/error (str "failed to sandbox-load-string: " expr))
                                   (str e)))]
                           (list
                            (str
                             "<div class='evalinput'>"
                             expr
                             "</div>"
                             "<div class='evalresult'>"
                             (cond
                              (or
                               (and (seq? loaded)
                                    (> (.size loaded) 1))
                               (and false (= (type loaded)
                                       clojure.lang.LazySeq)
                                    (= 0
                                       (.size
                                        (remove
                                         (fn [each]
                                           (= each java.lang.String))
                                        (map (fn [each]
                                               (type each))
                                             loaded))))))
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

                              (or (list? loaded)
                                  (= (type loaded) clojure.lang.Cons)
                                  (set? loaded))
                              (string/join " "
                                           (map (fn [elem]
                                                  (html/tablize elem))
                                                loaded))
                              (= (type loaded) clojure.lang.Var)
                              (str (eval loaded))
                              (and (map? loaded)
                                   (= (keys loaded) '(:plain)))
                              (str "<div style='font-family:monospace'>" (:plain loaded) "</div>")
                              (map? loaded)
                              (html/tablize loaded)
                              (= (type loaded) nil)
                              (str "<b>nil</b>")
                              :else
                              ;; nothing formattable: just stringify result of
                              ;; evaluation.
                              (str "<div style='font-family:monospace'>" loaded " (<b>" (type loaded) "</b>)" "</div>"))
                             "</div>"))))]
        (log/info (str "workbookq: done evaluating: " expr))
        output))))

(defn workbook-ui [request]
  (let [search-query (get (get request :query-params) "search")]
    (html
     [:div#workbook-ui {:class "quiz-elem"}
      [:h2 "Workbook"]
      [:div#searchbar
       [:textarea {:cols 80 :rows 4 :id "workbookq" }
        (if search-query
          search-query
          "(fo (random-sentence))")]
       [:button {:onclick "workbook()"} "evaluate"]]
      [:div#workbooka
       (if search-query
         (workbookq search-query nil))]])))
