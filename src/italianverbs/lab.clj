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

    [:div#giardino
     [:i {:class "fa fa-cloud" :style "  font-size:100px  "  } " "]

     ]]
   request
   {:onload "start_lab();"}))





