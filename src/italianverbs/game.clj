(ns italianverbs.game
  (:require
   [hiccup.page :refer :all]
   [italianverbs.html :as html]
   ))

(defn game []
  (html5
   [:div#game



    [:div#rainforest
     
     [:div#sky

      [:i {:class "fa fa-cloud fa-6x"
           :style "left:130px; top:10px"}]
      
      [:i {:class "fa fa-cloud medium-cloud storm-cloud"
           :style "left:230px; top:10px;"}]

      [:i {:class "fa fa-cloud big-cloud"
           :style "left:330px; top:30px;"}]
      
      [:i {:class "fa fa-cloud"
           :style "left:450px; font-size:75px; top:15px;"}]
      ]

     [:div#ground

      [:i {:class "fa fa-tree fa-5x"
           :style "left:50px;top:-120px;"}
       ]

      [:div {:class "fa fa-tree"
             :style "left:80px; top:-100px;font-size:160px"}]
      
      [:i {:class "fa fa-tree fa-5x"
           :style "left:180px;top:-120px;"}
       ]

      [:i {:class "fa fa-tree fa-3x"
           :style "left:220px;top:-120px;"}
       ]

      [:i {:class "fa fa-tree fa-4x"
           :style "left:330px;top:-120px;"}
       ]

      [:div {:class "fa fa-tree"
             :style "left:400px; top:-110px;font-size:140px"}]
      


      ]]
] ; html5/div
))







