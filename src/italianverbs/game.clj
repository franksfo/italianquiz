(ns italianverbs.game
  (:require
   [clojure.tools.logging :as log]
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

      [:i {:class "fa fa-tree x2" :style "left:1%; top:-10%" }]

      [:i {:class "fa fa-tree x1" :style "left:25%; top:5%" }]

      [:i {:class "fa fa-tree x2" :style "left:50%; top:-35%" }
       ]

      [:i {:class "fa fa-tree x3" :style "left:75%;top:-50%" }
       ]


      ] ;; end of :div#ground

     ] ;; end of :div#rainforest

    [:div#gameform
      [:input {:id "game_input" :size "50"}]
      [:button {:class "click"
                :onclick "submit_game_response('game_input');"} "Answer" ]
      
     ] ;; end of :div #gameform

    ] ;; end of :div #game
) ; html5/div

) ;; end of (defn)

(defn evaluate [user-response]
  (let [params (:form-params user-response)]
    (log/info (str "keys: " (keys params)))
    (let [guess (get-in params '("guess"))]
      (log/info (str "guess: " (get-in params '("guess"))))
      (str "clojure saw your response:" guess))))










