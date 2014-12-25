(ns italianverbs.editor
  (:refer-clojure :exclude [get-in])
  (:require
   [cemerick.friend :as friend]
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
   [formative.core :as f]
   [formative.parse :as fp]
   [italianverbs.auth :as auth]
   [italianverbs.english :as en]
   [italianverbs.html :as html]
   [italianverbs.italiano :as it]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.korma :as db]
   [italianverbs.unify :refer [get-in]]
   [hiccup.core :refer (html)]
   [korma.core :as k]
))

(def route-graph
  {:home {:create {:get "Create new game"}}
   :create {:home {:get "Cancel"}
            :create {:post {:button "New Game"}}}
   :read {:home {:get "Show all games"}}})

(declare body)
(declare control-panel)
(declare create)
(declare create-form)
(declare delete)
(declare delete-form)
(declare home-page)
(declare links)
(declare read-request)
(declare update)
(declare update-form)

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
        {:body (body "Editor: Create a new game" (create-form request) request)
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

   (GET "/delete/:game" request
        (delete-form request))

   (POST "/delete/:game" request
        (delete request))

   ;; alias for '/read' (above)
   (GET "/:game" request
        {:body (read-request request)
         :status 200})))

(defn onload []
  (str "log(INFO,'editor onload: stub.');"))

(defn body [title content request]
  (html/page 
   title
   (html
    [:div {:class "major"}
     [:h2 "Game Editor"]
     content])
   request
   {:css "/css/editor.css"
    :js "/js/editor.js"
    :onload (onload)}))

(defn read-request [request]
  "show a single game, along with UI to edit or delete the game."
  (let [game-id (:game (:params request))
        game-row (first (k/exec-raw [(str "SELECT * FROM games WHERE id='" game-id "'")] :results))]
    (body (str "Showing game: " (:name game-row))
          (html
           [:h3 (:name game-row)]

           [:button {:onclick (str "javascript:window.location.href = '/editor/delete/' + '" game-id "';")}
            "Delete" ]

           ;; allows application to send messages to user after redirection via URL param: "&message=<some message>"
           [:i {:class "user-alert"} (:message (:params request))]

           (links request :read))
          request)))

(defn update [request])
(defn update-form [request])

(defn delete [request]
  {:status 302
   :headers {"Location" (str "/editor/" "?message=deleted.")}})

(defn delete-form [request]
 (let [game-id (:game (:params request))
        game-row (first (k/exec-raw [(str "SELECT * FROM games WHERE id='" game-id "'")] :results))]
    (body (str "Deleting game: " (:name game-row))
          (html
           [:h3 (str "Confirm - deleting '" (:name game-row) "'")]

           [:p "Are you sure you want to delete this game?"]

           [:form {:method "post"
                   :action (str "/editor/delete/" game-id)}
            [:button {:onclick (str "javascript:submit();")}
             "Confirm" ]]

           ;; allows application to send messages to user after redirection via URL param: "&message=<some message>"
           [:i {:class "user-alert"} (:message (:params request))]

           (links request :read))
          request)))

(declare onload)

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
              (if (= (subs message 0 38) ;; TODO: too fragile: depends on an exact string match from the error message.
                     (str "ERROR: relation \"" table-name "\" does not exist"))
                (do (create-games-table)
                    ;; retry now that we have created the table.
                    (show request))
                (throw e)))))]
    games))

(defn home-page [request]
  (let [links (links request :home)
        games (show request)]
    (html
     [:div
      (cond
       (empty? games)
       [:div "No games yet."]
       true
       
       ;; TODO: derive <td> links from route-graph, like (links) does.
       [:table
        (map (fn [each]
               [:tr [:td [:a {:href (str "/editor/" (:id each))} (:name each)]]])
             games)])

      links])))

(def game-form
  {:fields [{:name :name :size 50 :label "Name"}]
   :validations [[:required [:name]]
                 [:min-length 1 :verbs "Select one or more verbs"]]})

(def all-verbs
  (filter (fn [lexeme]
            (not (empty?
                  (filter (fn [lex]
                            (and
                             (= :top (get-in lex [:synsem :infl]))
                             (= :verb
                                (get-in lex [:synsem :cat]))))
                          (get @it/lexicon lexeme)))))
          (sort (keys @it/lexicon))))

(def defaults {})

(defn create [request]
  (log/info (str "editor/new with request: " (:form-params request)))
  (let [values (fp/parse-params game-form (:form-params request))
        insert-sql (str "INSERT INTO games (id,name) VALUES (DEFAULT,'" (:name values) "') RETURNING id")]
    (let [results (k/exec-raw [insert-sql] :results)
          new-game-id (:id (first results))]
      {:status 302
       :headers {"Location" (str "/editor/" new-game-id "?message=created.")}})))

(defn links [request current]
  ;; TODO: _current_ param can be derived from request.
  (let [method :get]
    (html
     [:div {:class "links"}

      ;; TODO: vary these links depending on context (which can be obtained by loooking in request).
      [:span [:a {:href (str "/editor/" (string/replace-first (str :create) ":" ""))} "Create new game"]]
      [:span [:a {:href (str "/editor/" (string/replace-first (str :home) ":" ""))} "Show all games"]]

      ])))

(defn create-form [request]
  (let [links (links request :create)]
    (html
     [:div

      (f/render-form (assoc (-> game-form
                                (f/merge-fields [{:name :tests :label "Verbs for this game"
                                                  :type :checkboxes
                                                  :options all-verbs}]))
                       :values (merge defaults (:form-params request))
                       :action "/editor/create"
                       :method "post"))
      links])))

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
