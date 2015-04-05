(ns italianverbs.editor
  (:refer-clojure :exclude [get-in])
  (:require
   [clojure.data.json :as json]
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
   [formative.core :as f]
   [formative.parse :as fp]
   [italianverbs.auth :refer [is-admin is-authenticated]]
   [italianverbs.borges.reader :refer :all]
   [italianverbs.english_rt :as en]
   [italianverbs.html :as html]
   [italianverbs.italiano_rt :as it]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.korma :as db]
   [italianverbs.unify :refer [get-in strip-refs]]
   [italianverbs.verb :refer [generation-table predicates-from-lexicon]]
   [hiccup.core :refer (html)]
   [korma.core :as k]
))

(declare create-game)
(declare game-form)
(declare show-games)
(declare show-groups)
(declare unabbrev)

(defn home-page [ & [ {game-to-delete :game-to-delete
                       game-to-edit :game-to-edit
                       group-to-edit :group-to-edit
                       message :message} ]]
  (html
   [:div.user-alert message]
   
   [:div.section [:h3 "Games"]
    (show-games {:game-to-delete game-to-delete
                 :game-to-edit game-to-edit
                 })
    ]
   
   [:div.section [:h3 "Groups"]
    (show-groups {:group-to-edit :group})
    ]
   ))

(defn insert-game [name source target source-set target-set]
  "Create a game with a name, a source and target language and two
  arrays, one for the source language and one for the target
  language. Each array is an integer which refers to a set of
  'anyof-sets', each member of which is a possible specification in its
  respective language. See example usage in test/editor.clj."
  (log/debug (str "source-set: " source-set))
  (log/debug (str "source-set with commas: " (str "ARRAY[" (string/join "," (map #(str "ARRAY[" (string/join "," %) "]") source-set)) "]")))
  (log/debug (str "target-set with commas: " (str "ARRAY[" (string/join "," (map #(str "ARRAY[" (string/join "," %) "]") target-set)) "]")))

  (let [source-or-str (str "ARRAY[" (string/join "," (map #(str "ARRAY[" (string/join "," %) "]") source-set)) "]::integer[]")

        target-or-str (str "ARRAY[" (string/join "," (map #(str "ARRAY[" (string/join "," %) "]") target-set)) "]::integer[]")
        sql (str "INSERT INTO game (name,source_groupings,target_groupings,source,target) 
                       SELECT ?, " source-or-str "," target-or-str ",?,? RETURNING id")]
    (log/debug (str "inserting new game with sql: " sql))
    ;; return the row ID of the game that has been inserted.
    (:id (first (k/exec-raw [sql [name source target]] :results)))))

(defn show-games [ & [ {game-to-edit :game-to-edit
                        game-to-delete :game-to-delete
                        name-of-game :name-of-game} ]]
  (let [game-to-edit (if game-to-edit (Integer. game-to-edit))
        game-to-delete (if game-to-delete (Integer. game-to-delete))
        results (k/exec-raw [
                             "SELECT game.name AS game_name,game.id AS id,
                                     source,target,
                                     uniq(array_agg(source_groupings.name)) AS source_groups,
                                     uniq(array_agg(target_groupings.name)) AS target_groups 
                                FROM game 
                           LEFT JOIN grouping AS source_groupings 
                                  ON source_groupings.id = ANY(source_groupings) 
                           LEFT JOIN grouping AS target_groupings 
                                  ON target_groupings.id = ANY(target_groupings) GROUP BY game.id"] :results)]
    (html

     [:table {:class "striped padded"}
      [:tr
       [:th {:style "width:3%"} 
        (cond
         (or game-to-edit game-to-delete)
         "Cancel"
         :else "")
        ]

       [:th {:style "width:15%"} "Name"]
       [:th {:style "width:5%"} "Source"]
       [:th {:style "width:5%"} "Target"]
       [:th {:style "width:35%"} "Source Groups"]
       [:th {:style "width:35%"} "Target Groups"]
       (cond
        game-to-edit [:th "Update"]
        game-to-delete [:th "Delete"]
        :else "")
       ]

      (map (fn [result]
             (let [game-id (:id result)
                   game-to-edit game-to-edit
                   game-to-delete game-to-delete]
               [:tr 
                [:td
                 (cond (not (or game-to-edit game-to-delete))
                       [:div 
                        [:button 
                         {:onclick (str "edit_game_dialog(" game-id ");")}
                         "Edit"]
                        [:button
                         {:onclick (str "document.location='/editor/game/delete/" game-id "';" )}
                         "Delete"]
                        ]
                       
                       (or (= game-to-edit game-id)
                           (= game-to-delete game-id))
                       [:a
                        {:href "/editor"} "Cancel"]

                       true "")]

                [:td
                 (if (= game-to-edit game-id)
                   [:input {:size (+ 5 (.length (:game_name result))) 
                            :value (:game_name result)}]
                   (:game_name result)
                   )]
                [:td (unabbrev (:source result))]
                [:td (unabbrev (:target result))]
                [:td (str "<div class='group sourcegroup'>"
                          (string/join "</div><div class='group sourcegroup'>" (.getArray (:source_groups result)))
                          "</div>")]
                [:td (str "<div class='group targetgroup'>"
                          (string/join "</div><div class='group targetgroup'>" (.getArray (:target_groups result)))
                          "</div>")]

                [:td 
                 (cond (= game-to-edit game-id)
                       [:div
                        [:form#update
                         {:method "POST"
                          :action (str "/editor/game/edit/" game-id)}]

                        [:button {:onclick (str "$(\"#update\").submit();")} "Confirm"]]

                       (= game-to-delete game-id)
                       [:div
                        [:form#confirm_delete
                         {:method "POST"
                          :action (str "/editor/game/delete/" game-id)}]

                        [:button.confirm_delete {:onclick (str "$(\"#confirm_delete\").submit();")} "Confirm"]]

                       true
                       "")]]))
                
           results)]


     [:div.new
      [:button {:onclick (str "document.location='/editor/game/new';")} "New Game"]
      ]


     ;; make the hidden game-editing forms.
     (map (fn [result]
            (let [game-id (:id result)
                  game-to-edit game-to-edit
                  problems nil ;; TODO: should be optional param of this (defn)
                  game-to-delete game-to-delete]
              [:div.editgame
               {:id (str "editgame" game-id)}
               [:h2 (str "Editing game: " (:game_name result))]

               (f/render-form (assoc game-form
                                :values {:name (:game_name result)
                                         :source (:source result)
                                         :target (:target result)
                                         :source_groups (.getArray (:source_groups result))
                                         :target_groups (.getArray (:target_groups result))}
                                :action (str "/editor/game/edit/" game-id)
                                :method "post"
                                :problems problems))
                                         

               ]
              ))
          
          results)

     )))

(defn insert-grouping [name selects]
  "Create a grouping. This is a set of specifications, any of which may be
true for an expression to be part of the grouping. The grouping's
semantics are evaluated by OR-ing together the selects
when (expressions-per-game) evaluates the grouping for a particular
game to find what expressions are appropriate for particular game."

  (let [selects (if (not (seq? selects))
                  (vec selects)
                  selects)
        as-json-array (str "ARRAY['"
                           (string/join "'::jsonb,'"
                                        (map #(json/write-str (strip-refs %))
                                             selects))

                           "'::jsonb]")
        sql (str "INSERT INTO grouping (name,any_of) VALUES (?," as-json-array ") RETURNING id")]
    (log/debug (str "inserting new anyof-set with sql: " sql))
    (map #(:id %)
         (k/exec-raw [sql
                      [name]] :results))))

(defn expressions-for-game [game-id]
  "Return possible source->target mappings given a game-id."
  (let [sql "SELECT source.surface AS source,source.structure AS source_structure,
                    target.surface AS target,target.structure AS target_structure
  FROM (SELECT surface AS surface,structure AS structure
          FROM (SELECT DISTINCT surface,structure,count(target_grouping.id) AS groups
                           FROM (SELECT DISTINCT surface,structure,language
                                            FROM expression) AS target_expression
                     INNER JOIN grouping AS target_grouping
                             ON target_expression.structure @> ANY(target_grouping.any_of)
                     INNER JOIN game
                             ON game.id = ?
                            AND target_grouping.id = ANY(game.target_groupings)
                            AND game.target = target_expression.language
                       GROUP BY surface,structure) AS targets
         WHERE groups = (SELECT COUNT(*)
                           FROM grouping AS target_grouping
                     INNER JOIN game
                             ON target_grouping.id = ANY(game.target_groupings)
                            AND game.id = ?)) AS target

INNER JOIN (SELECT surface AS surface,structure AS structure
              FROM (SELECT DISTINCT surface,structure,count(source_grouping.id) AS groups
                               FROM (SELECT DISTINCT surface,structure,language
                                                FROM expression) AS source_expression
                         INNER JOIN grouping AS source_grouping
                                 ON source_expression.structure @> ANY(source_grouping.any_of)
                         INNER JOIN game
                                 ON game.id = ?
                                AND source_grouping.id = ANY(game.source_groupings)
                                AND game.source = source_expression.language
                           GROUP BY surface,structure) AS sources
             WHERE groups = (SELECT COUNT(*)
                               FROM grouping AS source_grouping
                         INNER JOIN game
                                 ON source_grouping.id = ANY(game.source_groupings)
                                AND game.id = ?)) AS source
       ON ((target.structure->'synsem'->'sem') @> (source.structure->'synsem'->'sem')
           OR
           (source.structure->'synsem'->'sem') @> (target.structure->'synsem'->'sem'))"]

    ;; Parse the returned JSON in clojure maps.  TODO: the :value-fns
    ;; below are wrongfully converting things to keywords that should
    ;; legitimately stay strings (e.g. values of the :espanol,
    ;; :italiano, and :english keys).
    (map (fn [row]
           {:source (:source row)
            :target (:target row)
            :source-structure (json/read-str (str (:source_structure row))
                                             :key-fn keyword
                                             :value-fn (fn [k v]
                                                         (cond (string? v)
                                                               (keyword v)
                                                               :else v)))
            :target-structure (json/read-str (str (:target_structure row))
                                             :key-fn keyword
                                             :value-fn (fn [k v]
                                                         (cond (string? v)
                                                               (keyword v)
                                                               :else v)))})

         (k/exec-raw [sql
                      [game-id game-id game-id game-id]] :results))))

(declare body)
(declare control-panel)
(declare create)
(declare delete)
(declare delete-form)
(declare edit)
(declare home-page)
(declare links)
(declare onload)
(declare read-request)
(declare show-inflections-per-game)
(declare show-words-per-game)
(declare update)
(declare update-form)
(declare set-as-default)
(declare tenses-per-game)
(declare verbs-per-game)
(declare short-language-name-to-long)

(defn show-expression [name source target & [source-groupings target-groupings]]
  (let [source-sqls (let [non-empty-groupings (filter #(not (= % {}))
                                                  source-groupings)]
                      (if (not (empty? non-empty-groupings))
                        (string/join " AND " 
                                     (map (fn [source-grouping]
                                            (let [json-source-grouping (json/write-str (strip-refs source-grouping))]
                                              (str "source.structure @> '" json-source-grouping "'")))
                                          non-empty-groupings))
                      " true "))

        target-sqls (let [non-empty-groupings (filter #(not (= % {}))
                                                  target-groupings)]
                      (if (not (empty? non-empty-groupings))
                        (string/join " AND "
                                     (map (fn [grouping]
                                            (let [json-grouping (json/write-str (strip-refs grouping))]
                                              (str "target.structure @> '" json-grouping "'")))
                                          non-empty-groupings))
                      " true "))
                     
        select (str "SELECT DISTINCT * FROM (SELECT source.surface AS source, 
                                                    target.surface AS target
                                               FROM expression AS source
                                         INNER JOIN expression AS target
                                                 ON source.language = ? 
                                                AND target.language = ? "
                                              " AND " source-sqls
                                              " AND " target-sqls
                                              " AND (target.structure->'synsem'->'sem') @> (source.structure->'synsem'->'sem')) AS pairs
                                              LIMIT 10")
        debug (log/debug (str "expressions select sql: " select))
        results (k/exec-raw [select [source target]] :results)]
    (html
     [:div.specs
     [:h4 {:style ""} (str name)]
      [:div.spec 
       [:h4 (short-language-name-to-long source)]
       (if (empty? source-groupings) [:i (str "no source grouping.")]
           (html/tablize source-groupings))
       ]

      [:div.spec 
       [:h4 (short-language-name-to-long target)]
       (if (empty? target-groupings) [:i (str "no target grouping.")]
           (html/tablize target-groupings))
       ]
      
      [:table.striped
       [:tr
        [:th ]
        [:th (str (short-language-name-to-long source))]
        [:th (str (short-language-name-to-long target))]
        ]
       (map (fn [result]
              [:tr 
               [:td ]
               [:td (:source result)]
               [:td (:target result)]])
            results)
       ]])))

(defn show-expressions []
  (let [select (str "SELECT name,source,target,source_grouping::text AS source_grouping ,target_grouping::text AS target_grouping FROM expression_select")
        results (k/exec-raw [select] :results)]
    (html
     (if (empty? results)
       (str "no expressions to show.")
       [:div {:style "float:left"}
        (map (fn [result]
               (show-expression (:name result)
                                (:source result)
                                (:target result)
                                (list (json/read-str (:source_grouping result))) (list (json/read-str (:target_grouping result)))))
             results)]))))

(def headers {"Content-Type" "text/html;charset=utf-8"})

(def routes
  (compojure/routes
   (GET "/" request
        (is-admin {:body (body "Editor: Top-level" (home-page request) request)
                   :status 200
                   :headers headers}))

   ;; alias for '/editor' (above)
   (GET "/home" request
        {:status 302
         :headers {"Location" "/editor"}})
  
   (POST "/create" request
         (is-admin (create request)))

   (GET "/game/delete/:game-to-delete" request
        (let [game-to-delete (:game-to-delete (:route-params request))]
          (is-admin {:body (body (str "Editor: Confirm: delete game: " game-to-delete)
                                 (home-page {:game-to-delete game-to-delete}) 
                                 request)
                     :status 200
                     :headers headers})))

   (POST "/game/delete/:game-to-delete" request
         (is-admin (delete (:game-to-delete (:route-params request)))))

   (GET "/game/edit/:game-to-edit" request
        (let [game-to-edit (:game-to-edit (:route-params request))]
          (is-admin {:body (body (str "Editor: Editing game: " game-to-edit)
                                 (home-page {:game-to-edit game-to-edit}) 
                                 request)
                     :status 200
                     :headers headers})))

   (POST "/game/edit/:game-to-edit" request
         (is-admin (edit (:game-to-edit (:route-params request))
                         (:params request))))

   (GET "/game/new" request
        (do
          (insert-game "untitled" "" "" [] [])
          (is-admin {:status 302
                     :headers {"Location" "/editor"}})))

   (POST "/game/new" request
         (is-admin (create-game request)))

   (GET "/group/new" request
        (do
          (insert-grouping "untitled" [{}])
          (is-admin {:status 302
                     :headers {"Location" "/editor"}})))

   (GET "/read" request
        (is-admin
         {:body (read-request request)
          :headers headers}))

   (GET "/delete/:game" request
        (is-admin
         {:headers headers
          :body (delete-form request)}))

   (POST "/delete/:game" request
        (is-admin (delete request)))

   ;; alias for '/read' (above)
   (GET "/:game" request
        (is-admin {:body (read-request request)
                   :headers headers
                   :status 200}))

   ;; which game(s) will be active (more than one are possible).
   (POST "/use" request
         (is-admin (set-as-default request)))))

(def all-inflections
  (map #(string/replace-first (str %) ":" "")
       [:conditional :present :future :imperfect :passato]))

(defn predicates-from-db []
  (map #(string/replace (str (:predicate %)) "\"" "")
       (k/exec-raw [(str "SELECT DISTINCT structure->'synsem'->'sem'->'pred'::text
                                       AS predicate
                                     FROM expression
                                 ORDER BY predicate")] :results)))

(def game-form
  {:fields [{:name :name :size 50 :label "Name"}
            {:name :source :type :select 
             :label "Source Language"
             :options [{:value "en" :label "English"}
                       {:value "it" :label "Italian"}
                       {:value "es" :label "Spanish"}]}
            {:name :target :type :select 
             :label "Target Language"
             :options [{:value "en" :label "English"}
                       {:value "it" :label "Italian"}
                       {:value "es" :label "Spanish"}]}



            ]
   :cancel-href "/editor"
   :validations [[:required [:name]]
                 [:min-length 1 :groups "Select one or more groups"]]})

(defn onload []
  (string/join " " 
               (map (fn [game]
                      (let [game-id (:game game)
                            source (:source game)
                            target (:target game)]
                        (str 
                         "log(INFO,'editor onload: loading gen_per_verb( " game-id " )');"
                         "gen_per_verb('" game-id "','" source "','" target "');")))
                    (k/exec-raw [(str "SELECT games_to_use.game,games.source,games.target 
                                         FROM games_to_use 
                                   INNER JOIN games 
                                           ON games_to_use.game = games.id")] :results))))

(defn body [title content request]
  (html/page 
   title
   (html
    [:div {:class "major"}
     [:h2 "Game Editor"]
     content])
   request
   {:css "/css/editor.css"
    :jss ["/js/editor.js" "/js/gen.js"]
    :onload (onload)}))

(defn examples-per-game [game-id]
  (let [results (k/exec-raw [(str "SELECT source.surface AS source, 
                                          target.surface AS target,
                                          source.structure->'synsem'->'sem' AS source_sem,
                                          target.structure->'synsem'->'sem' AS target_sem
                                     FROM expression AS source
                               INNER JOIN expression AS target
                                       ON ((source.structure->'synsem'->'sem')::jsonb = (target.structure->'synsem'->'sem')::jsonb)
                                    WHERE source.language='en' AND target.language='it'
                                      AND source.structure->'synsem'->'sem'->'pred' 
                                       IN (SELECT ('\"'||word||'\"')::jsonb 
                                             FROM words_per_game WHERE game=?)
                                 ORDER BY source.id LIMIT 10") [(Integer. game-id)]]
                            :results)]
    results))

(defn example-table [request]
  (html
   [:div {:style "width:100%"}
    [:h4 "Examples"]
    (let [examples (examples-per-game (:game (:params request)))]
      [:div#exampletable
       [:table.striped
        [:tr
         [:th "Source"]
         [:th "Target"]
         [:th "Source sem"]
         [:th "Target sem"]
         ]

        (map (fn [row]
               (let [debug (log/debug (str "source expression:"
                                           (:source row)))
                     source (:source row)
                     target (:target row)
                     source_sem (let [sem (str (:source_sem row))]
                                  (json/read-str sem
                                                 :key-fn keyword
                                                 :value-fn (fn [k v]
                                                             (cond (string? v)
                                                                   (keyword v)
                                                                   :else v))))
                     target_sem (let [sem (str (:target_sem row))]
                                  (json/read-str sem
                                                 :key-fn keyword
                                                 :value-fn (fn [k v]
                                                             (cond (string? v)
                                                                   (keyword v)
                                                                   :else v))))]
                 [:tr 
                  [:td source]
                  [:td target]
                  [:td [:div {:onclick "toggle_expand(this);" :style "height:100px; width:200px ;overflow:scroll"} (html/tablize source_sem)]]
                  [:td [:div {:onclick "toggle_expand(this);" :style "height:100px; width:200px ;overflow:scroll"} (html/tablize target_sem)]]]))
             examples)]])]))

(defn read-request [request]
  "show a single game, along with UI to edit or delete the game."
  (let [game-id (:game (:params request))
        game-row (first (k/exec-raw [(str "SELECT * FROM games WHERE id=?") [(Integer. game-id)] ] 
                                    :results))]
    (body (str "Showing game: " (:name game-row))
          (html
           [:h3 (:name game-row)]

           [:div.gameinfo
            [:div
             [:h4 "Predicates"]
             (let [verbs (show-words-per-game request)]
               (if (empty? verbs)
                 [:p "No predicates chosen."]
                [:ul
                 (map (fn [word]
                        [:li (str word)])
                      (map #(:word %)
                           verbs))]))]
            [:div
             [:h4 "Tenses"]
             (let [inflections (show-inflections-per-game request)]
               (if (empty? inflections)
                 [:p "No tenses chosen."]
                 [:ul
                  (map (fn [inflection]
                         [:li (str inflection)])
                       (map #(:inflection %)
                            inflections))]))]

            (example-table request)

            ] ;; div.gameinfo
                 

           [:h4 {:style "float:left;width:100%;"} "Edit"]

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
    (log/info (str "updating verbs for this group: " game-id " with: " (string/join "," words)))
    (k/exec-raw [(str "DELETE FROM words_per_game WHERE game=?") [(Integer. game-id)]])
    (update-verb-for-game game-id words)))

(defn update-inflection-for-game [game-id inflections]
  (if (not (empty? inflections))
    (let [inflection (first inflections)]
      (log/info (str "INSERT INTO inflections_per_game (game,inflection) " game-id "," inflection))
      (k/exec-raw [(str "INSERT INTO inflections_per_game (game,inflection) VALUES (?,?)") [(Integer. game-id)
                                                                                     inflection]])
      (update-inflection-for-game game-id (rest inflections)))))

(defn update-inflections-for-game [game-id inflections]
  (let [inflections (remove #(= % "")
                      (get inflections "inflections[]"))]
    (log/info (str "updating verbs for this group: " game-id " with: " (string/join "," inflections)))
    (k/exec-raw [(str "DELETE FROM inflections_per_game WHERE game=?") [(Integer. game-id)]])
    (update-inflection-for-game game-id inflections)))

(defn set-each-as-default [params]
  (if (not (empty? params))
    (let [param (first params)]
      (log/debug (str "use param: " param))
      (let [k (first param)
            v (second param)]
      (if (= v "on")
        (k/exec-raw [(str "INSERT INTO games_to_use (game) VALUES (?)") [(Integer. k)]]))
      (set-each-as-default (rest params))))))

(defn set-as-default [request]
  (let [games-to-use (:params request)]
    (k/exec-raw [(str "DELETE FROM games_to_use")])
    (set-each-as-default games-to-use)
    {:status 302
     :headers {"Location" (str "/editor/?message=updated.")}}))

(defn delete [game-id]
  (if game-id
    (do
      (log/debug (str "DELETING GAME: " game-id))
      (let [game-id (Integer. game-id)
            game-row (first (k/exec-raw [(str "SELECT * FROM game WHERE id=?") [game-id]] :results))]
        (log/debug (str "GAME ROW: " game-row))
        (k/exec-raw [(str "DELETE FROM game WHERE id=?") [game-id]])
        {:status 302
         :headers {"Location" (str "/editor/" "?message=Deleted+game:" (:name game-row))}}))
    {:status 302
     :headers {"Location" (str "/editor/" "?message=no+game+to+delete")}}))

(defn edit [game-id params]
  (log/debug (str "EDITING GAME WITH PARAMS: " params))
  (let [game-id game-id]
    (log/debug (str "Editing game with id= " game-id))
    (k/exec-raw ["UPDATE game SET (name,source,target) = (?,?,?) WHERE id=?" 
                 [(:name params)
                  (:source params)
                  (:target params)
                  (Integer. game-id)]])

    {:status 302
     :headers {"Location" (str "/editor/" "?message=Edited+game:" game-id)}}))

(defn delete-form [request]
 (let [game-id (:game (:params request))
       game-row (first (k/exec-raw [(str "SELECT * FROM games WHERE id=?") [(Integer. game-id)]] :results))]
    (body (str "Deleting game: " (:name game-row))
          (html
           [:h3 (str "Confirm - deleting '" (:name game-row) "'")]

           [:p "Are you sure you want to delete this group?"]

           [:form#delete_confirm
            {:method "post"
             :action (str "/editor/delete/" game-id)}]

           ;; allows application to send messages to user after redirection via URL param: "&message=<some message>"
           [:div.user-alert (:message (:params request))]

           (links request :read))
          request)))

(def games-table "games")
(def words-per-game-table "words_per_game")
(def inflections-per-game-table "inflections_per_game")

(defn show [request]
  (k/exec-raw [(str "SELECT * FROM " games-table " ORDER BY name")] :results))

(defn show-inflections-per-game [request]
  (let [table-name inflections-per-game-table
        game-id (:game (:params request))]
    (k/exec-raw [(str "SELECT * FROM " table-name " WHERE game = ? ORDER BY inflection") [(Integer. game-id)]] :results)))

(defn show-words-per-game [request]
  (let [table-name words-per-game-table
        game-id (:game (:params request))]
    (k/exec-raw [(str "SELECT * FROM " table-name " WHERE game = ? ORDER BY word") [(Integer. game-id)]] :results)))


(defn show-cities []
  "Show which cities are set for which games"
  (let [results (k/exec-raw [(str "SELECT city.name AS city ,game.name AS game 
                                     FROM city 
                               INNER JOIN city_game 
                                       ON (city_game.city = city.id) 
                               INNER JOIN game 
                                       ON (city_game.game = game.id)
                                 ORDER BY city.name")] :results)]
    (html
     [:table.striped
      [:tr
       [:th "City"]
       [:th "Game"]]
      (map (fn [result]
             [:tr 
              [:td (:city result)]
              [:td (:game result)]])
           results)
      ])))

(defn show-groups [ [& request ]]
  (let [results (k/exec-raw ["SELECT id,name,any_of FROM grouping"] :results)]
    (html
     [:table {:class "striped padded"}
      [:tr
       [:th "Name"]
       [:th "Any Of"]

       ]
      (map (fn [result]
             [:tr 
              [:td [:div.group (:name result)]]
              [:td (str "<div class='anyof'>" (string/join "</div><div class='anyof'>" (map #(json/read-str %
                                                                           :key-fn keyword
                                                                           :value-fn (fn [k v]
                                                                                       (cond (string? v)
                                                                                             (keyword v)
                                                                                             :else v)))
                                                           (.getArray (:any_of result))))
                        "</div>")]
              ])
           results)]
     [:div.new
      [:button {:onclick (str "document.location='/editor/group/new';")} "New Group"]
      ]
)))

(defn tenses-per-game [game-id]
  (log/info (str "verbs-per-game: " game-id))
  (map #(:inflection %)
       (k/exec-raw [(str "SELECT inflection
                                  FROM " inflections-per-game-table 
                               " WHERE game = ? ORDER BY inflection") [(Integer. game-id)]] :results)))

(defn verbs-per-game [game-id]
  (log/info (str "verbs-per-game: " game-id))
  (let [verbs
        (map #(:word %)
             (k/exec-raw [(str "SELECT word
                                  FROM " words-per-game-table 
                               " WHERE game = ? ORDER BY word") [(Integer. game-id)]] :results))]
    (log/debug (str "check these boxes: " (string/join "," verbs)))
    verbs))

(defn inflections-per-game [game-id]
  (let [inflections
        (map #(:inflection %)
             (k/exec-raw [(str "SELECT inflection
                                  FROM " inflections-per-game-table 
                               " WHERE game = ? ORDER BY inflection") [(Integer. game-id)]] :results))]
    (log/debug (str "check these boxes: " (string/join "," inflections)))
    inflections))

(def game-default-values {})

(defn links [request current]
  ;; TODO: _current_ param can be derived from request.
  (let [method :get]
    (html
     [:div {:class "links"}

      ;; TODO: vary these links depending on context (which can be obtained by loooking in request).
      [:span [:a {:href (str "/editor/" (string/replace-first (str :create) ":" ""))} "Create new list"]]
      [:span [:a {:href (str "/editor/" (string/replace-first (str :home) ":" ""))} "Show all lists"]]

      ])))

(defn create-form [request & [:problems problems]]
  {:status 200
   :body (body "Editor: Create a new game"
               (let [links (links request :create)]
                 (html
                  [:div
                   (f/render-form (assoc game-form
                                    :values (merge game-default-values (:form-params request))
                                    :action "/editor/create"
                                    :method "post"
                                    :problems problems))
                   ]))
               request)
   :headers headers})

;; TODO: update-form should be a complete request structure (with :status, :body, and :headers), just like
;; (create-form) above.
(defn update-form [request game & [:problems problems]]
  (log/debug (str "update-form with game: " game))
  (let [game-id (:id game)]
    (html
     [:div
      (f/render-form (assoc game-form
                       :values (merge game-default-values
                                      (:form-params request)
                                      {:name (:name game)
                                       :source (:source game)
                                       :target (:target game)
                                       :game game-id
                                       :words (verbs-per-game game-id)
                                       :inflections (inflections-per-game game-id)})
                       :action (str "/editor/update/" (:id game))
                       :method "post"
                       :problems problems))])))

(defn short-language-name-to-long [lang]
  (cond (= lang "it") "Italian"
        (= lang "en") "English"
        (= lang "es") "Spanish"
        true "???"))

(defn unabbrev [lang]
  (short-language-name-to-long lang))

