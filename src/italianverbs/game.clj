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

      ] ;; end of :div#ground

     ] ;; end of :div#rainforest

    [:div#gameform
     [:input {:id "game_input" :size "50"}]
     [:button {:class "click"
               :onclick "submit_game_response('game_input');"} "Answer" ]]
       

    ] ;; end of :div #game
) ; html5/div

) ;; end of (defn)

(defn evaluate [user-response]
  (let [params (:form-params user-response)]
    (log/info (str "keys: " (keys params)))
    (let [guess (get-in params '("guess"))]
      (log/info (str "guess: " (get-in params '("guess"))))
      (str "clojure saw your response:" guess))))










