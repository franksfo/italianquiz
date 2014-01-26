;; TODO: move as much app-specific code out of here
;; into italianverbs.html.
(ns base.html
  (:use [hiccup core page]
        [somnium.congomongo])
  (:require [base.lib :as baselib]
            [clojure.tools.logging :as log]
            [hiccup.page :as hpage]
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
      [:div "Benvenuti, " username "."
       [:a {:href "/session/clear/"} "Logout"]
       ]
      [:a {:href "/session/set/"} "Login"]
      )]))

(defn menubar [session-row relative-url]
  (html
   [:div {:class "menubar major"}
;    "URL:" relative-url
    [:div
     (if (= relative-url "/quiz/") {:class "selected"})
     [:a {:href "/quiz/"} "Quiz"]]
;    [:div
;     (if (= relative-url "/lexicon/") {:class "selected"})
;     [:a {:href "/lexicon/"} "Lexicon"  ] ]
;    [:div
;     (if (= relative-url "/search/") {:class "selected"})
;     [:a {:href "/search/"} "Search"  ] ]
    [:div
     (if (= relative-url "/workbook/") {:class "selected"})
     [:a {:href "/workbook/"} "Libro di Lavoro"  ] ]
;    [:div
;     (if (= relative-url "/preferiti/") {:class "selected"})
;     [:a {:href "/preferiti/"} "I tuoi preferiti"]]
    [:div
     (if (= relative-url "/about/") {:class "selected"})
     [:a {:href "/about/"} "Che è?"  ] ]
    ]))

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
          [:td
           (powered-by "grammar" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbsgrammar.clj")]
          [:td {:rowspan "2"}
           (powered-by "lexicon" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/lexicon.clj")
           ]
          [:td {:rowspan "2"}
           (powered-by "morphology" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/morphology.clj")
           ]
          ]
         [:tr
          [:td {:colspan "1"}
           (powered-by "generate" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/generate.clj")
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
    [:meta  {:Content-Type "text/html; charset=UTF-8"}]
    [:title (str title
                 (if (and title (not (= title "")))
                     ": " "")
                 "imparare l'italiano")]
    [:script {:type "text/javascript" :src "/js/jquery-1.6.4.min.js"}]
    [:script {:type "text/javascript" :src "/js/autogrow.js"}]
    [:script {:type "text/javascript" :src "/js/quiz.js"}]
    [:script {:type "text/javascript" :src "/js/workbook.js"}]
    [:script {:type "text/javascript" :src "/js/search.js"}]
    ; enable this 'reset.css' at some point.
                                        ;    (include-css "/italian/css/reset.css")
    (include-css "/css/style.css")
    (include-css "/css/layout.css")
    (include-css "/css/fs.css")
    (include-css "/css/quiz.css")
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
    (if false
      (if request
        [:div {:class "welcome major"}
         (welcome (session/get-username request))]))

    [:div#top
     (menubar (session/request-to-session request)
              (if request (get request :uri)))]
    [:div#content content]

    (if request
      [:div.reqdata
       (reqdata request)])]))

;; TODO: replace (page) with this once (eval) works right.
(defmacro pagemacro [title & [content request onload]]
  (let [error-english "Sorry, there was an internal problem with this site that prevented your content from being displayed."
        error-italian "Scusi, che è stato una errore che ha preventato questo site. Purtroppo è non possibile guardare il tuo contento."]
    (try
      (let [evaluated-content (eval content)]
        (log/info (str "html.clj: request: " request))
        (page title evaluated-content request onload))
      (catch Exception e
        (page "Exception caught"
              (str "<div class='error'>"
                   "  <div class='english'>" error-english "</div>"
                   "  <div class='italian'>" error-italian "</div>"
                   "  <div class='code'>" e "</div>"
                   "</div>")
              request
              ;; still allow the onload even for caught exceptions(?) possible security risk?
              ;;nil)))))
              onload)))))






