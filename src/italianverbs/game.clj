(ns italianverbs.game
  (:require
   [hiccup.page :refer :all]
   [italianverbs.html :as html]
   ))

(defn game []
  (html5
   [:div#game

    [:svg {:id "gamesvg"}
     [:text {:class "bear"  :x "50"  :y "20"} "Bear"]
     [:text {:class "cat"   :x "150" :y "20"} "Cat"]
     [:text {:class "cow"   :x "250" :y "20"} "Cow"]
     [:text {:class "dog"   :x "350" :y "20"} "Dog"]
     [:text {:class "gecko" :x "450" :y "20"} "Gecko"]
     [:text {:class "otter" :x "550" :y "20"} "Otter"]
     [:text {:class "snake" :x "650" :y "20"} "Snake"]
     [:text {:class "wolf"  :x "750" :y "20"} "Wolf"]
     ]

    (if true
      (html5
    [:div#rainforest


     "HELLO"

    (if false
      (html5

     
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

      ] ;; end of :div#ground

     )) ;; end of (if <bool> [:div#sky ..)
     ] ;; end of :div#rainforest
    ) ;; end of (str)
      ) ;; end of (if <bool> [:div#rainforest ..)
    ] ;; end of :div #game
) ; html5/div

) ;; end of (defn)









