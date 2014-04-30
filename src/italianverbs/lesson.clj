(ns italianverbs.lesson
  (:use [hiccup core]))

;; "I just realized on my way home that besides the time stamp teachers
;; should be allowed to enter their verbs in sets, sort of like the
;; flashcards in quizlet, with set names. This is because most teachers
;; use textbooks and textbooks are divided in chapters or units. So
;; teachers may like to be ablo to tell the system something
;; like: "create a test with 15 verbs from units 4 and 5" rather
;; than "create a test with 15 verbs entered between 1/11 and 4/21".  If
;; it is possible to do both a time stamp and a tag or name of set that
;; would be ideal.

(defn lesson [session request]
  (html
   [:div {:class "major"}
    [:div {:style "width:20em; padding:0.5em;"}
     "List of current tags"]
    [:a {:href "/lesson/new"} "Create a new tag.."]]))

(defn new [session request]
  (html
   [:div {:class "major"}
    [:div {:style "width:30em;height:30em;"}
     "Create a new tag"]]))
