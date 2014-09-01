(ns italianverbs.game
  (:require
   [hiccup.page :refer :all]
   [italianverbs.html :as html]
   ))

(defn game []
  (html5
   [:div#game

    [:svg {:id "svgarena"}
     [:text {:class "bear"  :x "50"  :y "20"} "Bear"]
     [:text {:class "cat"   :x "150" :y "20"} "Cat"]
     [:text {:class "cow"   :x "250" :y "20"} "Cow"]
     [:text {:class "dog"   :x "350" :y "20"} "Dog"]
     [:text {:class "gecko" :x "450" :y "20"} "Gecko"]
     [:text {:class "otter" :x "550" :y "20"} "Otter"]
     [:text {:class "snake" :x "650" :y "20"} "Snake"]
     [:text {:class "wolf"  :x "750" :y "20"} "Wolf"]
     ]


    [:div#rainforest
     
     [:div#sky

      [:i {:class "fa fa-cloud fa-6x"
           :style "left:5%; top:30px"}]
      
      [:i {:class "fa fa-cloud medium-cloud storm-cloud"
           :style "left:25%; top:50px;"}]

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









