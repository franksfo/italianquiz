(ns italianverbs.about
  (:require
   [compojure.core :refer [GET]]
   [italianverbs.html :as html]))

(declare about)

(def routes 
  (GET "/about" request
       {:status 200
        :headers {"Content-Type" "text/html;charset=utf-8"}
        :body (html/page "Welcome to Verbcoach"
                         (about)
                         request)}))


(defn about []
   [:div {:style "width:100%; height: 500px;"   :class "major"} [:h2 "Welcome to Verbcoach."]

;   [:p 
;    "You can use this website just by clicking on the language you want to ;practice, or you can login using your existing Google account (enter your g;oogle username and password)."
;    ]

    [:div.intro 

     "The best place on the web to learn how to conjugate verbs."

     ]


    [:div#flags
     [:div.flag

      [:a {:href "/tour/it"}
       [:img {:src "/png/Flag_of_Italy.svg.png" }]]

      [:a {:href "/tour/it"}
       [:div.language "Italiano"]
       ]
      ]

     [:div.flag {:style "float:right"}

      [:a {:href "/tour/es"}
       [:img {:src "/png/Flag_of_Spain.svg.png" }]]

      [:a {:href "/tour/es"}
       [:div.language "Español"]
       ]
      ]

     ]


])

