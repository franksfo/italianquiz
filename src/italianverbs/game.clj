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

     
     [:div#sky

      [:i {:class "fa fa-cloud fa-6x"
           :style "left:5%; top:30px"}]
      
      [:i {:class "fa fa-cloud medium-cloud stormy"
           :style "left:25%; top:50px;"}]

      [:i {:class "fa fa-cloud medium-cloud partly-stormy"
           :style "left:45%; top:30px;"}]

      [:i {:class "fa fa-cloud big-cloud"
           :style "left:60%; top:30px;"}]
      
      [:i {:class "fa fa-cloud"
           :style "left:80%; font-size:75px; top:15px;"}]
      ]

     [:div#ground

      [:i {:class "fa fa-tree fa-5x"
           :style "left:10%;top:20px;"}
       ]

      [:div {:class "fa fa-tree"
             :style "left:20%; top:-50px;font-size:160px"}]
      
      [:i {:class "fa fa-tree fa-5x"
           :style "left:35%;top:20px;"}
       ]

      [:i {:class "fa fa-tree fa-3x"
           :style "left:40%;top:20px;"}
       ]

      [:i {:class "fa fa-tree fa-4x"
           :style "left:50%;top:-20px;"}
       ]

      [:div {:class "fa fa-tree"
             :style "left:60%; top:-50px;font-size:140px"}]

      ] ;; end of :div#ground

     ] ;; end of :div#rainforest
    ] ;; end of :div #game
) ; html5/div

) ;; end of (defn)









