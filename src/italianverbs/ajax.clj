(ns italianverbs.ajax
  (:use
   [hiccup core page-helpers])
  (:require
   [clojure.string :as string]
   [clojure.contrib.str-utils2 :as str-utils]))

(defn prepend []
  (clojure.string/join ""
                       (flatten
                        (list "<div class='click' onclick='got(\"here..\")'>click me</div>"
                              "<table id='guesstable'>
                                   <thead>
                                     <tr><th/><th>h1</th><th>h2</th></tr>
                                   </thead>
                                   <tbody id='guessbody'></tbody>
                                 </table>"))))

(defn test []
  "this should contain a list of all the tests for the html package. each test can
  return a map or a list or a function. a function will be applied against an
  empty argument list"
  (list
   {:comment "add rows to the top of a table."
    :test (prepend)}))
