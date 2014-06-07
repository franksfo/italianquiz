(ns italianverbs.student
  (:use [hiccup core])
  (:require
   [clj-time.core :as t]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [formative.core :as f]
   [formative.parse :as fp]
   [italianverbs.html :as html]
   [italianverbs.korma :as db]
   [italianverbs.korma :as db]))

;; db-fetch: add 'where user.type=student...'
(defn student [ & args]
  (let [students
        (db/fetch :user)] 
    (html
     [:div {:class "major tag"}
      [:h2 "Students"]
      [:p "Only teachers should see this."]])))

(defn delete [ & args])

(defn new [ & args])

(defn show [ & args ])

(defn delete-class-from-student [ & args])

(defn add-class-to-student [ & args])

