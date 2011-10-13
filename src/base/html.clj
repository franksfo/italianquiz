;; probably can remove congomongo from here.
(ns base.html
  (:use [hiccup core page-helpers]
        [somnium.congomongo])
  (:require [base.lib :as baselib]
            [clojure.set :as set]
            [italianverbs.session :as session]
            [clojure.string :as string]))

(defn message [msg] 
  (html
   [:div msg]))

(defn welcome [username] 
  (html
   [:div
    (if username 
      [:div "benvenuti, " username "."
       [:a {:href "/italian/session/clear/"} "Logout"]
       ]
      [:a {:href "/italian/session/set/"} "Login"]
      )]))

(defn menubar [session-row relative-url]
  (html
   [:div {:class "menubar major"}
;    "URL:" relative-url
    (if session-row ;; only show quiz option if there's a session to save the quiz for.
      [:div
       (if (= relative-url "/italian/quiz/") {:class "selected"})
       [:a {:href "/italian/quiz/"} "Quiz"]])
    [:div
     (if (= relative-url "/italian/lexicon/") {:class "selected"})
     [:a {:href "/italian/lexicon/"} "Lexicon"  ] ] 
    [:div
     (if (= relative-url "/italian/test/") {:class "selected"})
     [:a {:href "/italian/test/"} "Unit Tests"  ] ] 
    ]))

(defn powered-by [name link]
  (html
   [:div {:class "poweredby"}
    [:a {:href link}
     name ]]))

(defn footer [session-row]
  (html
   [:div {:class "poweredbox major"}
    [:h3 "Powered by"]
    [:table
     [:tr
      [:td {:colspan "4"}
       (powered-by "italianquiz" "https://github.com/ekoontz/italianquiz/tree/italian")]]
     [:tr
      [:td {:colspan "2"}
       (powered-by "compojure" "https://github.com/weavjester/compojure")]
      [:td {:rowspan "2" :colspan "2"}
       (powered-by "congomongo" "https://github.com/somnium/congomongo")]]
      [:tr
       [:td {:colspan "2"}
        (powered-by "ring" "https://github.com/mmcgrana/ring")]]
     [:tr
      [:td 
       (powered-by "jetty" "http://jetty.codehaus.org/jetty/")]
      [:td {:colspan "2"}
       (powered-by "clojure" "http://clojure.org/")]
      [:td {:colspan "1"}
       (powered-by "mongodb" "http://www.mongodb.org/")
       ]]]]))

(defn req-tr [key-value-pair]
  (let [key (first key-value-pair)
        val (second key-value-pair)]
    (str "<tr><th>" key "</th><td>" val "</td></tr>")))

(defn reqdata [request]
  (let [prefs (get (fetch :filter :where {:session (baselib/get-session-key request)} :limit 1) :form-params)]
    (html
     [:div
      [:h2 "Cookie:"
       (baselib/get-session-key request)]

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
        [:td {:colspan "2"}
         [:div {:style "overflow:scroll;width:90%"}
          (get request :headers)]]]
       [:tr
        [:th {:colspan "2" :style "text-align:left"} "body"]]
       [:tr
        [:td {:colspan "2"} (get request :body)]]]])))

(defn page [title & [content request onload]]
  (html5
   [:head 
    [:meta  {:charset "ISO-8859-1"}]
    [:title "Verbi italiani &#0187; " title]
    [:script {:type "text/javascript" :src "/italian/js/jquery-1.6.4.min.js"}]
    [:script {:type "text/javascript" :src "/italian/js/quiz.js"}]
    (include-css "/italian/css/style.css")
    (include-css "/italian/css/layout.css")
    (include-css "/italian/css/fs.css")
    ]
   [:body
    {:onload
     (cond
      onload onload
      (= title "Quiz")
      "document.quiz.guess.focus();"
      (= title "testx")
      "setTimeout('location.reload(true);',5000);"
      true "")}
    [:h1
     [:a {:href "/italian/"} "Verbi italiani" ]
      " &#0187;" title ]

    (if false
      (if request
        [:div {:class "welcome major"}
         (welcome (session/get-username request))]))

    (menubar (session/get-session-row request)
             (if request (get request :uri)))
    
    [:div#content content]

    (if request
      (footer (session/get-session-row request)))
    
    (if request
      [:div.reqdata
       (reqdata request)])]))
   



