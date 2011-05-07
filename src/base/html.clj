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

(defn menubar [session-row]
  (html
   [:div {:class "footer major"}
    [:div 
     [:a {:href "/"} "Main"  ] ] 
    (if session-row
      [:div [:a {:href "/quiz/"} "Quiz"]])
    [:div 
     [:a {:href "/lexicon/"} "Lexicon"  ] ] 
    [:div 
     [:a {:href "/test/"} "Test"  ] ] 
    [:div 
     [:a {:href "/form/"} "Forms"  ] ] 
    ]))

(defn footer [session-row]
  (html

   [:div {:class "poweredbox major"}
    
    [:div {:class "poweredby"}
     "Powered by " [:a {:href "https://github.com/ekoontz/italianquiz/tree/italian"}
                    "italianverbs" ] ]
    
    [:div {:class "poweredby"}
     "Powered by " [:a {:href "http://github.com/weavejester/compojure"}
                    "compojure" ] ]
    
    [:div {:class "poweredby"}
     "Powered by " [:a {:href "https://github.com/somnium/congomongo"}
                    "congomongo" ] ]
    
    ]))

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
      (menubar (session/get-session-row request)))

    [:div#content content]


    (if request
      (footer (session/get-session-row request)))


    (if request
      [:div {:class "http major"}
       (welcome (session/get-username request))])

    
    (if request
      [:div.reqdata
       (baselib/reqdata request)])]))
   



