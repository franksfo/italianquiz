(ns base.lib
  (:use [hiccup core page-helpers])
  (:require
   [clojure.set :as set]
   [clojure.string :as string]))

(defn req-tr [key-value-pair]
  (let [key (first key-value-pair)
        val (second key-value-pair)]
    (str "<tr><th>" key "</th><td>" val "</td></tr>")))

(defn get-session-key [request]
  (let [cookies (get request :cookies)]
    (if cookies
      (let [ring-session (get cookies "ring-session")]
        (if ring-session
          (get ring-session :value))))))

(defn reqdata [request]
  (html
   [:div
    [:h2 "Cookie:"
     (get-session-key request)]

    [:h2 "Request Map"]
    [:table
     [:tr
      [:th "key"]
      [:th "val"]
      ]
     ;; do headers separately: since it's so long, it stretches the table too much.
      (string/join " " (seq
                        (map req-tr
                             (map (fn [key]
                                    (list key (get request key)))
                                  (set/difference (set (keys request))
                                                  (set (list :body :headers)))))))
     [:tr
      [:th {:colspan "2" :style "text-align:left"} "headers"]
      ]
     [:tr
      [:th {:colspan "2" :style "text-align:left"} "body"]
      ]
     [:tr
            [:td {:colspan "2"} (get request :headers)]]]]))
     
       
