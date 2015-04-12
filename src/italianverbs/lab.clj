(ns italianverbs.lab
  (:refer-clojure :exclude [get-in])
  (:require
   [hiccup.page :as h]
   [italianverbs.html :as html]
   )
  )

(defn giardino [request]
  (html/page "il giardino"
   [:div#giardino

    [:svg {:id "svg_giardino"}]

     [:i {:id "cloud_0" :class "fa fa-cloud diagonal motion" :style "font-size:100px;left:90%;top:5%"  } " "]

     ]
   request
   {:onload "start_lab();"
    :js "/js/lab.js"}))







