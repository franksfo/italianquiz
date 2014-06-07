(ns italianverbs.class
  (:refer-clojure :exclude [class]) ;; probably bad idea to exclude this..
  (:use [hiccup core])
  (:require
   [italianverbs.html :as html]
   [italianverbs.korma :as db]))

(defn class [ & args]
  (let [args
        (db/fetch :classes)]
    (html
     [:div {:class "major tag"}
      [:h2 "Classes.."]])))

(defn delete [ & args])

(defn new [ & args])

(defn show [ & args])

(defn delete-from-class [ & args])

(defn add-to-tag [ & args])


