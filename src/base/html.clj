(ns base.html
    (:use [hiccup core page-helpers])
    (:require [base.lib :as baselib]
              [italianverbs.session :as session]))

(defn message [msg] 
  (html
   [:div msg]))

(defn welcome [username] 
  (html
   [:div.sessiondata
    (if username 
      [:p "Welcome, " username "." 
       [:a {:href "/session/clear/"} "Logout"]
       ]
      [:a {:href "/session/set/"} "Login"]
      )]))

(defn menubar [session-row relative-url]
  (html
   [:div {:class "menubar major"}
;    "URL:" relative-url
    [:div
     (if (= relative-url "/") {:class "selected"})
     [:a {:href "/"} "Main"  ] ] 
    (if session-row ;; only show quiz option if there's a session to save the quiz for.
      [:div
       (if (= relative-url "/quiz/") {:class "selected"})
       [:a {:href "/quiz/"} "Quiz"]])
    [:div
     (if (= relative-url "/lexicon/") {:class "selected"})
     [:a {:href "/lexicon/"} "Lexicon"  ] ] 
    [:div
     (if (= relative-url "/test/") {:class "selected"})
     [:a {:href "/test/"} "Test"  ] ] 
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



(defn page [title & [content request]]
  (html5
   [:head 
    [:meta  {:charset="ISO-8859-1"}]
    [:title "Verbi italiani &#0187; " title]
    (include-css "/css/style.css")]
   [:body
    {:onload
     (cond
      (= title "Quiz")
      "document.quiz.guess.focus();"
      (= title "test")
      "setTimeout('location.reload(true);',5000);"
      true "")}
    [:h1
     [:a {:href "/"} "Verbi italiani" ]
      " &#0187;" title ]

    (if request
      [:div {:class "welcome major"}
       (welcome (session/get-username request))])

    (menubar (session/get-session-row request)
             (if request (get request :uri)))
    
    [:div#content content]

    (if request
      (footer (session/get-session-row request)))



    
    (if request
      [:div.reqdata
       (baselib/reqdata request)])]))
   



