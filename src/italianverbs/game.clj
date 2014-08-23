(ns italianverbs.game
  (:require
   [hiccup.page :refer :all]
   [italianverbs.html :as html]
   ))

(defn game []
  (html5
   [:div#game
    [:i {:class "fa fa-camera-retro fa-lg"} ]
    [:i {:class "fa fa-camera-retro fa-2x"} ]
    [:i {:class "fa fa-camera-retro fa-3x"} ]
    [:i {:class "fa fa-camera-retro fa-4x"} ]
    [:i {:class "fa fa-camera-retro fa-5x"} ]]))



