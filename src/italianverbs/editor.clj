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
(declare onload)
(declare read-request)
(declare show-words-per-game)
(declare update)
(declare update-form)

(def headers {"Content-Type" "text/html;charset=utf-8"})

(defn routes []
  (compojure/routes
   (GET "/" request
        {:body (body "Editor: Top-level" (home-page request) request)
         :status 200
         :headers headers})

   ;; alias for '/editor' (above)
   (GET "/home" request
        {:status 302
         :headers {"Location" "/editor"}})
  
   (GET "/create" request
        (create-form request))

   (POST "/create" request
        (create request))

   (GET "/read" request
        (read-request request))

   (GET "/update" request
        (update-form request))

   (POST "/update/:game" request
         (update request))

   (GET "/delete/:game" request
        (delete-form request))

   (POST "/delete/:game" request
        (delete request))

   ;; alias for '/read' (above)
   (GET "/:game" request
        {:body (read-request request)
         :status 200})))

(declare all-verbs)

(def game-form
  {:fields [{:name :name :size 50 :label "Name"}
            {:name :game :type :hidden}
            {:name :words
             :label "Verbs for this game.."
             :type :checkboxes
             :datatype :strs
             :options all-verbs}]
   :validations [[:required [:name]]
                 [:min-length 1 :verbs "Select one or more verbs"]]})

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
        game-row (first (k/exec-raw [(str "SELECT * FROM games WHERE id=?") [(Integer. game-id)] ] :results))]
    (body (str "Showing game: " (:name game-row))
          (html
           [:h3 (:name game-row)]

           [:h4 "Verbs"]
           [:ul
            (map (fn [word]
                   [:li (str word)])
                 (map #(:word %)
                      (show-words-per-game request)))
            ]

           ;; allows application to send messages to user after redirection via URL param: "&message=<some message>"
           [:div.user-alert (:message (:params request))]

           (update-form request game-row)
           
           [:div.delete
            [:button {:onclick (str "javascript:window.location.href = '/editor/delete/' + '" game-id "';")}
             "Delete" ]]


           (links request :read))
          request)))


(defn update-verb-for-game [game-id words]
  (if (not (empty? words))
    (let [word (first words)]
      (log/info (str "INSERT INTO words_per_game (game,word) " game-id "," word))
      (k/exec-raw [(str "INSERT INTO words_per_game (game,word) VALUES (?,?)") [(Integer. game-id)
                                                                                     word]])
      (update-verb-for-game game-id (rest words)))))
    

(defn update-verbs-for-game [game-id words]
  (let [words (remove #(= % "")
                      (get words "words[]"))]
    (log/info (str "updating verbs for this game: " game-id " with: " (string/join "," words)))
    (k/exec-raw [(str "DELETE FROM words_per_game WHERE game=?") [(Integer. game-id)]])
    (update-verb-for-game game-id words)))

(defn update [request]
  (log/info (str "editor/update with request: " (:form-params request)))
  (fp/with-fallback #(update-form request :problems %)
    (let [values (fp/parse-params game-form (:form-params request))
          debug (log/debug (str "values: " values))
          results (k/exec-raw ["UPDATE games SET (name) = (?) WHERE id=? RETURNING id" [(:name values)
                                                                                        (Integer. (:game values))]] 
                              :results)
          edited-game-id (:id (first results))]
      (update-verbs-for-game edited-game-id (:form-params request))
      {:status 302
       :headers {"Location" (str "/editor/" edited-game-id "?message=updated.")}})))

(defn delete [request]
  (let [game-id (:game (:params request))
        game-row (first (k/exec-raw [(str "SELECT * FROM games WHERE id=?") [(Integer. game-id)]] :results))]
    (k/exec-raw [(str "DELETE FROM words_per_game WHERE game=?") [(Integer. game-id)]])
    (k/exec-raw [(str "DELETE FROM games WHERE id=?") [(Integer. game-id)]])
    {:status 302
     :headers {"Location" (str "/editor/" "?message=deleted+game:" (:name game-row))}}))

(defn delete-form [request]
 (let [game-id (:game (:params request))
       game-row (first (k/exec-raw [(str "SELECT * FROM games WHERE id=?") [(Integer. game-id)]] :results))]
    (body (str "Deleting game: " (:name game-row))
          (html
           [:h3 (str "Confirm - deleting '" (:name game-row) "'")]

           [:p "Are you sure you want to delete this game?"]

           [:form {:method "post"
                   :action (str "/editor/delete/" game-id)}
            [:button {:onclick (str "javascript:submit();")}
             "Confirm" ]]

           ;; allows application to send messages to user after redirection via URL param: "&message=<some message>"
           [:div.user-alert (:message (:params request))]

           (links request :read))
          request)))

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
  (k/exec-raw [(str "ALTER TABLE ONLY " games-table " ALTER COLUMN id SET DEFAULT nextval('games_id_seq'::regclass);")])
  (k/exec-raw [(str "ALTER TABLE ONLY " games-table " ADD CONSTRAINT games_pkey PRIMARY KEY (id)")]))

(def words-per-game-table "words_per_game")
(defn create-wpg-table []
  (k/exec-raw [(str "CREATE TABLE " words-per-game-table " (game bigint REFERENCES " games-table "(id),word text)")]))

(defn show [request]
  (let [table-name games-table
        games
        (try
          (k/exec-raw [(str "SELECT *
                               FROM " table-name " ORDER BY name")] :results)
          (catch Exception e
            (let [message (.getMessage e)]
              (if (= (subs message 0 38) ;; TODO: too fragile: depends on an exact string match from the error message.
                     (str "ERROR: relation \"" table-name "\" does not exist"))
                (do (create-games-table)
                    ;; retry now that we have created the table.
                    (show request))
                (throw e)))))]
    games))

(defn show-words-per-game [request]
  (let [table-name words-per-game-table
        game-id (:game (:params request))
        words
        (try
          (k/exec-raw [(str "SELECT *
                               FROM " table-name " WHERE game = ? ORDER BY word") [(Integer. game-id)]] :results)
          (catch Exception e
            (let [message (.getMessage e)
                  error-message (str "ERROR: relation \"" table-name "\" does not exist")]
              (if (= (subs message 0 (.length error-message)) ;; TODO: too fragile: depends on an exact string match from the error message.
                     error-message)
                (do (create-wpg-table)
                    ;; retry now that we have created the table.
                    (show-words-per-game request))
                ;; if it still fails, we throw a 500 I suppose.
                (throw e)))))]
    words))

(defn home-page [request]
  (let [links (links request :home)
        games (show request)]
    (html
     [:div.user-alert (:message (:params request))]
     [:div
      (cond
       (empty? games)
       [:div "No games."]
       true
       
       ;; TODO: derive <td> links from route-graph, like (links) does.
       [:table
        (map (fn [each]
               [:tr [:td [:a {:href (str "/editor/" (:id each))} (:name each)]]])
             games)])

      links])))

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

(defn verbs-per-game [game-id]
  (let [verbs (map #(:word %)
                   (k/exec-raw [(str "SELECT word
                                        FROM " words-per-game-table " WHERE game = ? ORDER BY word") [(Integer. game-id)]] :results))]
    (log/debug (str "check these boxes: " (string/join "," verbs)))
    verbs))

(def game-default-values {})

(defn create [request]
  (log/info (str "editor/new with request: " (:form-params request)))
  (fp/with-fallback #(create-form request :problems %)
    (let [values (fp/parse-params game-form (:form-params request))
          results 
          (try
            (k/exec-raw [(str "INSERT INTO games (id,name) VALUES (DEFAULT,?) RETURNING id") [(:name values)]] :results)
            (catch Exception e
              (let [message (.getMessage e)]
                (log/error (str "Failed inserting into games:{:name=>" (:name values) "}"))
                ;; TODO: maybe there's something we can do to remedy the problem..?
                (throw e))))
          new-game-id (:id (first results))]
      (update-verbs-for-game new-game-id (:form-params request))
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

(defn create-form [request & [:problems problems]]
  {:body (body "Editor: Create a new game"
               (let [links (links request :create)]
                 (html
                  [:div
                   (f/render-form (assoc (-> game-form
                                             (f/merge-fields [{:name :words
                                                               :label "Verbs for this game"
                                                               :type :checkboxes
                                                               :cols 3
                                                               :options all-verbs}]))
                                    :values (merge game-default-values (:form-params request))
                                    :action "/editor/create"
                                    :method "post"
                                    :problems problems))
                   links]))
               request)
   :status 200
   :headers headers})

(defn update-form [request game & [:problems problems]]
  (let [game-id (:game (:params request))]
    (html
     [:div
      (f/render-form (assoc (-> game-form
                                (f/merge-fields [{:name :words}]))
                       :values (merge game-default-values {:name (:name game)
                                                           :game game-id
                                                           :words (verbs-per-game game-id)})
                       :action (str "/editor/update/" (:id game)) 
                       :method "post"
                       :problems problems))])))

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
