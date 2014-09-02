(ns italianverbs.game
  (:require
   [hiccup.page :refer :all]
   [italianverbs.html :as html]
   ))

(defn game []
  (html5
   [:div#game

    [:svg {:id "svgarena"}]


    [:div#rainforest

     [:div#wordbar

      [:div#q1 "to go"]

      [:div#q2 "to sleep"]

      [:div#q3 "to dream"]

      ]

     
     [:div#sky ]

     [:div#ground

      ] ;; end of :div#ground

     ] ;; end of :div#rainforest
    ] ;; end of :div #game
) ; html5/div

) ;; end of (defn)









