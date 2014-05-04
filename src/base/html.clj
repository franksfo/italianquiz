;; TODO: move as much app-specific code out of here
;; into italianverbs.html.
(ns base.html
  (:use [hiccup core page])
  (:require [base.lib :as baselib]
            [clojure.tools.logging :as log]
            [hiccup.page :as hpage]
            [clojure.set :as set]
            [italianverbs.db :refer (fetch)]
            [italianverbs.session :as session]
            [clojure.string :as string]))

(defn message [msg]
  (html
   [:div msg]))

(defn powered-by [name link]
  (html
   [:div {:class "poweredby"}
    [:a {:href link}
     name ]]))

(defn footer []
  (html
   [:div {:class "poweredbox major"}
    [:h2 [:i "imparare l'Italiano"] " è alimentato da .."]
    [:table
     [:tr
      [:td {:colspan "5"}
       [:div {:class "groupinglabel"}
        (powered-by "italianquiz" "https://github.com/ekoontz/italianquiz")]

       [:table {:class "italianquiz"}
        [:tr
         [:td {:colspan "3"}
          [:div {:class "groupinglabel2"}
           [:span {:class "label"} "app layer"]
           [:table {:class "groupinglabel2"}
            [:tr
             [:td {:colspan "1"}
              (powered-by "quiz" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/quiz.clj")]
             [:td {:colspan "1"}
              (powered-by "workbook" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/workbook.clj")]
             [:td {:colspan "1"}
              "others.."]]]]]]

         [:tr
          [:td {:colspan "3"}
           (powered-by "generate" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/generate.clj")
           ]
          ]
         [:tr
          [:td {:colspan "3"}
           (powered-by "forest" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/forest.clj")
           ]
          ]
         [:tr
          [:td
           (powered-by "grammar" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/grammar.clj")]
          [:td {:rowspan "1"}
           (powered-by "lexicon" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/lexicon.clj")
           ]
          [:td {:rowspan "1"}
           (powered-by "morphology" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/morphology.clj")
           ]
          ]

         [:tr
          [:td {:colspan "3"}
           (powered-by "unify" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/unify.clj")
           ]
          ]

         ]]]
     [:tr
      [:td {:colspan "2"}
       (powered-by "compojure" "https://github.com/weavejester/compojure")]
      [:td {:rowspan "2" :colspan "1"}
       (powered-by "clojail" "https://github.com/flatland/clojail")]
      [:td {:rowspan "2" :colspan "2"}
       (powered-by "congomongo" "https://github.com/aboekhoff/congomongo")]]
      [:tr
       [:td {:colspan "2"}
        (powered-by "ring" "https://github.com/mmcgrana/ring")]]
     [:tr
      [:td
       (powered-by "jetty" "http://jetty.codehaus.org/jetty/")]
      [:td {:colspan "3"}
       (powered-by "clojure" "http://clojure.org/")]
      [:td {:colspan "1"}
       (powered-by "mongodb" "http://www.mongodb.org/")
       ]]]]))

(defn about []
  (footer))

(defn req-tr [key-value-pair]
  (let [key (first key-value-pair)
        val (second key-value-pair)]
    (str "<tr><th>" key "</th><td>" val "</td></tr>")))






