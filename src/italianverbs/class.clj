(ns italianverbs.class
  (:refer-clojure :exclude [class]) ;; probably bad idea to exclude this..
  (:use [hiccup core])
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [italianverbs.html :as html]
   [italianverbs.korma :as db]))

(defn show [ & args]
  (let [classes
        (db/fetch :classes)]
    (html
     [:div {:class "major tag"}
      [:h2 "Classes"]
      (if (empty? classes)
        [:p "no classes." ])
      [:div [:a {:href "/class/new"}
             "Create a new class"]]])))

(defn delete [ & args])

(defn new [ & args])

(defn new-form [params & {:keys [problems]}]
  (let [now (java.util.Date.) ;; not using any date or time stuff in the form yet, but good to know about for later.
        defaults {:date now
                  :time now}
        groups (map #(:name %)
                    (db/fetch :tag))]
    (html
     [:div.major
      [:h2 "Create a new class"]
      [:div 
       "form elements.."
       ]])))

(defn delete-from-class [ & args])

(defn add-to-tag [ & args])


