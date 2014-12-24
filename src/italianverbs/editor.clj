(ns italianverbs.editor
  (:require
   [cemerick.friend :as friend]
   [clj-time.format :as f]
   [clj-time.core :as t]
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
   [italianverbs.auth :as auth]
   [italianverbs.english :as en]
   [italianverbs.html :as html]
   [italianverbs.italiano :as it]
   [italianverbs.morphology :refer (normalize-whitespace)]
   [italianverbs.korma :as db]
   [hiccup.core :refer (html)]
   [korma.core :as k]
))

(declare control-panel)

(defn read-request [request])

(defn update [request])
(defn update-form [request])

(defn delete [request])
(defn delete-form [request])

(declare onload)

(def route-graph
  {:home {:create {:get "Create new game"}}
   :create {:home {:get "Cancel"}
            :create {:post {:button "New Game"}}}})

(def games-table "games")

(defn create-games-table []
  (k/exec-raw [(str "CREATE TABLE " games-table " (id bigint NOT NULL,
                                                   name text);")])
  (k/exec-raw [(str "CREATE SEQUENCE games_id_seq
                     START WITH 1
                     INCREMENT BY 1
                     NO MINVALUE
                     NO MAXVALUE
                     CACHE 1;")])
  (k/exec-raw [(str "ALTER TABLE ONLY " games-table " ALTER COLUMN id SET DEFAULT nextval('games_id_seq'::regclass);")]))

(defn show [request]
  (let [table-name games-table
        games
        (try
          (k/exec-raw [(str "SELECT *
                               FROM " table-name)] :results)
          (catch Exception e
            (let [message (.getMessage e)]
              (if (= (subs message 0 38)
                     (str "ERROR: relation \"" table-name "\" does not exist"))
                (do (create-games-table)
                    ;; retry now that we have created the table.
                    (show request))
                (throw e)))))]
    games))

(defn home-page [request]
  (let [links (html 
               (map (fn [action]
                      [:a {:href (str "/editor/" (string/replace-first (str action) ":" ""))} (get-in route-graph [:home action :get])])
                    (keys (:home route-graph))))
        games (show request)]
    (html
     (cond
      (empty? games)
      [:div "no games yet."]
      true
      [:table
       (map (fn [each]
              [:tr [:td [:a {:href (str "/editor/" (:id each))} (:name each)]]])
            games)])
     
     [:div {:style "border:2px dashed blue"}
      links])))

(defn create [request]
  (let [links (html 
               (map (fn [action]
                      [:a {:href (str "/editor/" (string/replace-first (str action) ":" ""))} (get-in route-graph [:create action :get])])
                    (keys (:create route-graph))))]
    (html
     [:div {:style "border:2px dashed pink"}
      links])))

(defn body [title content request]
  (html/page 
   title
   (html
    [:div {:style "border:2px dashed green"}
     content])
   request
   {:css "/css/editor.css"
    :js "/js/editor.js"
    :onload (onload)}))

(defn routes []
  (compojure/routes
   (GET "/" request
        {:body (body "Editor: Top-level" (home-page request) request)
         :status 200
         :headers {"Content-Type" "text/html;charset=utf-8"}})

   ;; alias for '/editor' (above)
   (GET "/home" request
        {:status 302
         :headers {"Location" "/editor"}})
  
   (GET "/create" request
        {:body (body "Editor: Create a new game" (create request) request)
         :status 200
         :headers {"Content-Type" "text/html;charset=utf-8"}})

   (POST "/create" request
        (create request))

   (GET "/read" request
        (read-request request))

   (GET "/update" request
        (update-form request))
   (POST "/update" request
        (update request))

   (GET "/delete" request
        (delete-form request))
   (POST "/delete" request
        (delete request))))

(defn onload []
  (str "log(INFO,'editor onload: stub.');"))

(declare table-of-examples)

(def generate-this-many-at-once 10)

(defn control-panel [request haz-admin]
  (let [current-size "5,436"
        desired-size "10,000"]
    (html
     [:div#generation {:class "major"}
      [:h2 "Generation"]

;      [:div
;       [:button "Update"]]
      
      [:div#vocabulary
       [:h3 "Lexicon"]

       [:div#verbs 
        [:h4 "Verbs"]

        [:table 

          [:tr
           
           [:th ""]
           [:th {:style "width:10em"} "Italian"]
           [:th {:style "width:20em"} "Example"]

;           [:th {:style "width:10em"} "Semantics"]
           [:th {:style "width:10em"} "English"]
           [:th {:style "width:20em"} "Translation"]
           [:th {:style "width:3em"} ""]
           ]

         (map (fn [lexeme]
                [:tr.lexeme
                 
                 [:td
                  [:input {:type "checkbox"} ]]

                 [:td lexeme ]
                 [:td.example
                  [:div.gen_source {:id (str "verb_" lexeme)}  [:i {:class "fa fa-spinner fa-spin"} "" ] ]]

;                 [:td.semantics {:id (str "semantics_" lexeme)} [:i {:class "fa fa-spinner fa-spin"} "" ] ]

                 [:td {:id (str "english_verb_" lexeme)}  [:i {:class "fa fa-spinner fa-spin"} "" ] ]

                 [:td {:id (str "english_translation_" lexeme)} [:i {:class "fa fa-spinner fa-spin"} "" ]  ]

                 [:td [:i {:class "fa fa-refresh"} "" ] ]

                 ])
         
              (let [all-verbs
                    (filter (fn [lexeme]
                              (not (empty?
                                    (filter (fn [lex]
                                              (and
                                               (= :top (get-in lex [:synsem :infl]))
                                               (or true (= :bere (get-in lex [:synsem :sem :pred])))
                                               (= :verb
                                                  (get-in lex [:synsem :cat]))))
                                            (get @it/lexicon lexeme)))))
                            (sort (keys @it/lexicon)))]
                all-verbs))]]

       [:div#noun
        [:h4 "Nouns and Pronouns"]
        [:table

         (map (fn [lexeme]
                [:tr 
                 [:th [:input {:type "checkbox"}]]
                 [:td lexeme]])
              (filter (fn [lexeme]
                        (not (empty?
                              (filter (fn [lex]
                                        (= :noun
                                           (get-in lex [:synsem :cat])))
                                      (get @it/lexicon lexeme)))))
                      (sort (keys @it/lexicon))))
         ]
        ]

       [:div#dets
        [:h4 "Determiners"]
        [:table

         (map (fn [lexeme]
                [:tr 
                 [:th [:input {:type "checkbox"}]]
                 [:td lexeme]])
              (filter (fn [lexeme]
                        (not (empty?
                              (filter (fn [lex]
                                        (= :det
                                           (get-in lex [:synsem :cat])))
                                      (get @it/lexicon lexeme)))))
                      (sort (keys @it/lexicon))))
         ]
       ]
       ]

      [:div#inflections
       [:h3 "Inflections"]
       [:table

        (map (fn [infl]
               [:tr 
                [:th [:input {:type "checkbox"}]]
                [:td infl]])
             ["Condizionale"
              "Imperfetto"
              "Presente"
              "Futuro"
              "Passato Prossimo"])]
        ]


      [:div#examples
       [:h3 "Examples"] ;; see (defn onload)
                                 
         [:table
          [:tr
           [:th]
           [:th "English"]
           [:th "Italiano"]
           ]
          (table-of-examples 1 generate-this-many-at-once)
          ]
       ]

      [:div#currentsize
       [:h3 "Corpus Size" ]
       [:table
        [:tr
         [:th "Current"]
         [:td current-size]]
        [:tr
         [:th "Desired"]
         [:td [:input {:value desired-size}]]]]]

      ]
    ))) 

(defn table-of-examples [index upto]
  (if (<= index upto)
    (html
     [:tr
      [:th (str index)]
      [:td {:id (str "example_q_" index)}]
      [:td {:id (str "example_a_" index)}]]
     
     (table-of-examples (+ 1 index) generate-this-many-at-once))))
