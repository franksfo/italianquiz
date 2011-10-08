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
  (let [type :passato
        question (italianverbs.quiz/generate :passato)
        english (get question :english)
        italian (get question :italian)]
    (clojure.string/join ""
                         (flatten
                          (list "<div class='click' onclick='addguess(\"" english "\",\"" italian "\")'>Guess</div>"
                                "<table>
                                   <thead>
                                     <tr><th/><th>En</th><th>It</th></tr>
                                   </thead>
                                   <tbody id='guess-table'></tbody>
                                 </table>")))))

(defn ajax-update []
  (str
   "<div id='ajax_question'><script>get_next_question();</script></div>"
   "<input id='guess_input' type='text' value='myguess'></input>"
   "<button class='click' onclick='ajax_refresh(\"guess_input\")'>refresh</button>"
   "<table><tbody id='ajax_update'></tbody></table>"))

(defn test []
  "this should contain a list of all the tests for the html package. each test can
  return a map or a list or a function. a function will be applied against an
  empty argument list"
  (list
   {:comment "add static content to the top of a table."
    :test (prepend-static)}
   {:comment "add dynamic content to the top of a table."
    :test (prepend-dynamic)}
   {:comment "add ajax content to a div."
    :test (ajax-update)}))




