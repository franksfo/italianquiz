(ns italianverbs.ajax
  (:use
   [hiccup core page-helpers])
  (:require
   [italianverbs.quiz :as quiz]
   [clojure.string :as string]
   [clojure.contrib.str-utils2 :as str-utils]))

(defn prepend-static []
  (clojure.string/join ""
                       (flatten
                        (list "<div class='click' onclick='got(\"here..\")'>click me</div>"
                              "<table id='guesstable'>
                                   <thead>
                                     <tr><th/><th>h1</th><th>h2</th></tr>
                                   </thead>
                                   <tbody id='guessbody'></tbody>
                                 </table>"))))

(defn prepend-dynamic []
  (let [quiz (quiz/run nil)
        tmp "about quiz.."]
    (clojure.string/join ""
                         (flatten
                          (list "<div class='click' onclick='addguess(\"" tmp  "\",\"io vado\")'>Guess</div>"
                                "<table>
                                   <thead>
                                     <tr><th/><th>En</th><th>It</th></tr>
                                   </thead>
                                   <tbody id='guess-table'></tbody>
                                 </table>")))))

(defn test []
  "this should contain a list of all the tests for the html package. each test can
  return a map or a list or a function. a function will be applied against an
  empty argument list"
  (list
   {:comment "add static content to the top of a table."
    :test (prepend-static)}
   {:comment "add dynamic content to the top of a table."
    :test (prepend-dynamic)}))

