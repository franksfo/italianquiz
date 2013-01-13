(ns italianverbs.workbook
  (:use [hiccup core]
        [clojure.set])
  (:require
   [somnium.congomongo :as mongo]
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [italianverbs.fs :as fs]
   [italianverbs.html :as html]
   [italianverbs.sandbox :as sandbox]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.lev :as lev]))

(defn workbookq [expr attrs]
  (do
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
                              ;; TODO: collapse (PersistentList,Cons,LazySeq) into one case.
                              (= (type loaded)
                                 clojure.lang.PersistentList)
                              (string/join " "
                                           (map (fn [elem]
                                                  (html/tablize elem))
                                                loaded))
                              (= (type loaded)
                                 clojure.lang.Cons)
                              (string/join " "
                                           (map (fn [elem]
                                                  (html/tablize elem))
                                                loaded))
                              (and (= (type loaded)
                                      clojure.lang.LazySeq)
                                   (= 0
                                      (.size
                                       (remove
                                        (fn [each]
                                          (= each java.lang.String))
                                        (map (fn [each]
                                               (type each))
                                             loaded)))))
                              (str "<ol>"
                                   (string/join " "
                                                (map (fn [elem]
                                                       (str "<li>" (html/tablize elem) "</li>"))
                                                     (seq loaded)))
                                   "</il>")
                              (= (type loaded)
                                 clojure.lang.LazySeq)
                              (string/join " "
                                           (map (fn [elem]
                                                  (html/tablize elem))
                                                (seq loaded)))
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
          "(formattare (random-sentence))")]
       [:button {:onclick "workbook()"} "evaluate"]]
      [:div#workbooka
       (if search-query
         (workbookq search-query nil))]])))
