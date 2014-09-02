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

      [:div#q1 "wordbar"]

      [:div#q2 "not used"]

      [:div#q3 "yet"]

      ]

     
     [:div#sky ]

     [:div#ground

      ] ;; end of :div#ground

     ] ;; end of :div#rainforest

    [:div#gameform
     [:form
      [:input {:size "50"}]
      [:input {:type "submit" :value "answer"}]]]
       

    ] ;; end of :div #game
) ; html5/div

) ;; end of (defn)









