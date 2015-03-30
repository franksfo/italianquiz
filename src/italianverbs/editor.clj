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

(defn insert-game [name source target source-set target-set]
  "Create a game with a name, a source and target language and two
  arrays, one for the source language and one for the target
  language. Each array is an integer which refers to a set of
  'anyof-sets', each member of which is a possible specification in its
  respective language. See example usage in test/editor.clj."
  (log/debug (str "source-set: " source-set))
  (log/debug (str "source-set with commas: " (str "ARRAY[" (string/join "," (map #(str "ARRAY[" (string/join "," %) "]") source-set)) "]")))
  (log/debug (str "target-set with commas: " (str "ARRAY[" (string/join "," (map #(str "ARRAY[" (string/join "," %) "]") target-set)) "]")))

  (let [source-or-str (str "ARRAY[" (string/join "," (map #(str "ARRAY[" (string/join "," %) "]") source-set)) "]")

        target-or-str (str "ARRAY[" (string/join "," (map #(str "ARRAY[" (string/join "," %) "]") target-set)) "]")
        sql (str "INSERT INTO game (name,source_specs,target_specs,source,target) 
                       SELECT ?, " source-or-str "," target-or-str ",?,? RETURNING id")]
    (log/debug (str "inserting new game with sql: " sql))
    ;; return the row ID of the game that has been inserted.
    (:id (first (k/exec-raw [sql [name source target]] :results)))))

(defn insert-anyof-set [name selects]

  "Create an anyof-set. This is a set of specs, any of which may be
true for the whole anyof-set's semantics to be true. The set's
semantics are evaluated by OR-ing together the selects
when (expressions-per-game) evaluates the anyof-sets for a particular
game to find what expressions are appropriate for particular game."

  (let [selects (if (not (seq? selects))
                  (vec selects)
                  selects)
        as-json-array (str "ARRAY['"
                           (string/join "'::jsonb,'"
                                        (map #(json/write-str (strip-refs %))
                                             selects))

                           "'::jsonb]")
        sql (str "INSERT INTO spec (name,any_of) VALUES (?," as-json-array ") RETURNING id")]
    (log/debug (str "inserting new anyof-set with sql: " sql))
    (map #(:id %)
         (k/exec-raw [sql
                      [name]] :results))))

(declare selects-of-game)

(defn target-expressions-per-game [game-id]
  (let [sql 
        "SELECT surface AS target FROM (SELECT surface,COUNT(grouping) AS groups
                                          FROM (  SELECT DISTINCT surface,target_grouping.id AS grouping
                                                    FROM expression AS target_expression
                                              INNER JOIN spec AS target_grouping
                                                      ON target_expression.structure @> ANY(target_grouping.any_of)
                                              INNER JOIN game 
                                                      ON game.id = ? AND target_grouping.id = ANY(game.target_specs))
                                                      AS surface_per_grouping
                                                GROUP BY surface 
                                                ORDER BY groups desc) AS target_expression
                                           WHERE groups=(SELECT COUNT(*) 
                                                           FROM spec AS target_grouping
                                                     INNER JOIN game 
                                                             ON target_grouping.id = ANY(game.target_specs)
                                                            AND game.id = ?)"]
    (k/exec-raw [sql
                 [game-id game-id]] :results)))

(defn expressions-for-game [game-id]
  (let [selects-of-game (selects-of-game game-id)

        source-sql (string/join " AND "
                                (map (fn [anyof-set]
                                       (str " source.structure @> ANY(ARRAY['"
                                            (string/join "'::jsonb,'" (map json/write-str anyof-set))
                                            "'::jsonb])"))
                                     (:source selects-of-game)))

        target-sql (string/join " AND "
                                (map (fn [anyof-set]
                                       (str " target.structure @> ANY(ARRAY['"
                                            (string/join "'::jsonb,'" (map json/write-str anyof-set))
                                            "'::jsonb])"))
                                     (:target selects-of-game)))

        source-language (:source (first (k/exec-raw [(str "SELECT source FROM game WHERE id=?")
                                                     [game-id]] :results)))
        target-language (:target (first (k/exec-raw [(str "SELECT target FROM game WHERE id=?")
                                                     [game-id]] :results)))]

    (let [sql (str "SELECT DISTINCT * FROM (SELECT      
                                               source.surface AS source, 
                                               target.surface AS target,
                                             source.structure AS source_structure, 
                                             target.structure AS target_structure
                                              FROM expression AS source
                                        INNER JOIN expression AS target
                                                ON source.language = ? 
                                               AND target.language = ? "
                                             " AND " source-sql
                                             " AND " target-sql
                                             " AND (target.structure->'synsem'->'sem') @> (source.structure->'synsem'->'sem')) AS pairs")
          debug (log/debug (str "select expressions with: " sql))]
      (k/exec-raw
       [sql
        [source-language
         target-language]]
     :results))))

(def route-graph
  {:home {:create {:get "Create new game"}}
   :create {:home {:get "Cancel"}
            :create {:post {:button "New Game"}}}
   :read {:home {:get "Show all games"}}})

(declare body)
(declare control-panel)
(declare create)
(declare create-game)
(declare create-form)
(declare delete)
(declare delete-form)
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
(declare unabbrev)

(defn show-expression [name source target & [source-specs target-specs]]
  (let [source-sqls (let [non-empty-specs (filter #(not (= % {}))
                                                  source-specs)]
                      (if (not (empty? non-empty-specs))
                        (string/join " AND " 
                                     (map (fn [source-spec]
                                            (let [json-source-spec (json/write-str (strip-refs source-spec))]
                                              (str "source.structure @> '" json-source-spec "'")))
                                          non-empty-specs))
                      " true "))

        target-sqls (let [non-empty-specs (filter #(not (= % {}))
                                                  target-specs)]
                      (if (not (empty? non-empty-specs))
                        (string/join " AND "
                                     (map (fn [spec]
                                            (let [json-spec (json/write-str (strip-refs spec))]
                                              (str "target.structure @> '" json-spec "'")))
                                          non-empty-specs))
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
       (if (empty? source-specs) [:i (str "no source spec.")]
           (html/tablize source-specs))
       ]

      [:div.spec 
       [:h4 (short-language-name-to-long target)]
       (if (empty? target-specs) [:i (str "no target spec.")]
           (html/tablize target-specs))
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
  (let [select (str "SELECT name,source,target,source_spec::text AS source_spec ,target_spec::text AS target_spec FROM expression_select")
        results (k/exec-raw [select] :results)]
    (html
     (if (empty? results)
       (str "no expressions to show.")
       [:div {:style "float:left"}
        (map (fn [result]
               (show-expression (:name result)
                                (:source result)
                                (:target result)
                                (list (json/read-str (:source_spec result))) (list (json/read-str (:target_spec result)))))
             results)]))))

(defn selects-of-game [game-id]
  (let [source-select 
        (str "SELECT source.any_of AS source
                FROM game 
          INNER JOIN spec AS source
                  ON source.id = ANY(game.source_specs)
               WHERE game.id=?")
        target-select
        (str "SELECT target.any_of AS target
                FROM game 
          INNER JOIN spec AS target
                  ON target.id = ANY(game.target_specs)
               WHERE game.id=?")
        source-results (k/exec-raw [source-select [game-id]] :results)
        target-results (k/exec-raw [target-select [game-id]] :results)]
    {:target (map (fn [target-result]
                    (map #(json/read-str %
                                         :key-fn keyword
                                         :value-fn (fn [k v]
                                                     (cond (string? v)
                                                           (keyword v)
                                                           :else v)))
                         (.getArray (:target target-result))))
                  target-results)
     :source (map (fn [source-result]
                    (map #(json/read-str %
                                         :key-fn keyword
                                         :value-fn (fn [k v]
                                                     (cond (string? v)
                                                           (keyword v)
                                                           :else v)))
                         (.getArray (:source source-result))))
                  source-results)}))

(defn expressions-for-target-specs [game-id]
  (let [sql
        (str "SELECT target_expression.surface,
                     target_spec.any_of 
                FROM expression AS target_expression
          INNER JOIN spec AS target_spec
                  ON target_expression.structure @> ALL(any_of)
          INNER JOIN game
                  ON (game.id=?
                 AND  target_spec.id = ANY(game.source_specs))
               WHERE (target_expression.language = game.target)")]
    (k/exec-raw [sql [game-id]] :results)))

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
  
   (GET "/create" request
        (is-admin (create-form request)))

   (POST "/create" request
         (is-admin (create request)))

   (POST "/game/new" request
         (is-admin (create-game request)))

   (GET "/read" request
        (is-admin
         {:body (read-request request)
          :headers headers}))

   (POST "/update/:game" request
        (is-admin
         (update request)))

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
            {:name :game :type :hidden}
            {:name :words
             :label "Predicates for this group"
             :cols 3
             :type :checkboxes
             :datatype :strs
             :options (predicates-from-db)}
            {:name :inflections
             :label "Tenses for this group"
             :type :checkboxes
             :datatype :strs
             :options all-inflections}
            ]
   :validations [[:required [:name]]
                 [:min-length 1 :verbs "Select one or more verbs"]]})

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

(defn update [request]
  (let [game-id (:game (:params request))
        game-row (first (k/exec-raw [(str "SELECT * FROM games WHERE id=?") [(Integer. game-id)] ] :results))]
    (log/debug (str "editor/update with request: " (:form-params request)))
    (fp/with-fallback #(update-form request game-row :problems %)
      (let [debug (log/debug (str "editor/update..about to fp/parse-params with params: " (:form-params request)))
            values (fp/parse-params game-form (:form-params request))
            debug (log/debug (str "values parsed from form params: " values))
            results (k/exec-raw ["UPDATE games SET (name, source,target) = (?,?,?) WHERE id=? RETURNING id" [(:name values)
                                                                                                             (:source values)
                                                                                                             (:target values)
                                                                                                             (Integer. (:game values))]] 
                                :results)
            edited-game-id (:id (first results))]
        (update-verbs-for-game edited-game-id (:form-params request))
        (update-inflections-for-game edited-game-id (:form-params request))
        {:status 302
         :headers {"Location" (str "/editor/" edited-game-id "?message=updated.")}}))))

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

(defn delete [request]
  (let [game-id (:game (:params request))
        game-row (first (k/exec-raw [(str "SELECT * FROM games WHERE id=?") [(Integer. game-id)]] :results))]
    (k/exec-raw [(str "DELETE FROM words_per_game WHERE game=?") [(Integer. game-id)]])
    (k/exec-raw [(str "DELETE FROM inflections_per_game WHERE game=?") [(Integer. game-id)]])
    (k/exec-raw [(str "DELETE FROM games_to_use WHERE game=?") [(Integer. game-id)]])
    (k/exec-raw [(str "DELETE FROM games WHERE id=?") [(Integer. game-id)]])
    {:status 302
     :headers {"Location" (str "/editor/" "?message=deleted+game:" (:name game-row))}}))

(defn delete-form [request]
 (let [game-id (:game (:params request))
       game-row (first (k/exec-raw [(str "SELECT * FROM games WHERE id=?") [(Integer. game-id)]] :results))]
    (body (str "Deleting game: " (:name game-row))
          (html
           [:h3 (str "Confirm - deleting '" (:name game-row) "'")]

           [:p "Are you sure you want to delete this group?"]

           [:form {:method "post"
                   :action (str "/editor/delete/" game-id)}
            [:button {:onclick (str "javascript:submit();")}
             "Confirm" ]]

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

(defn show-games []
  (let [results (k/exec-raw ["SELECT name,source,target FROM game"] :results)]
    (html
     [:button {:onclick (str "document.location='/editor/game/new';")} "New Game"]
     [:table.striped
      [:tr
       [:th "Name"]
       [:th "Source"]
       [:th "Target"]]
      (map (fn [result]
             [:tr 
              [:td (:name result)]
              [:td (unabbrev (:source result))]
              [:td (unabbrev (:target result))]])
           results)])))

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

(defn home-page [request]
  (let [links (links request :home)
        games (show request)
        games-to-use (set (mapcat vals (k/exec-raw ["SELECT * FROM games_to_use"] :results)))]
    (html
     [:div.user-alert (:message (:params request))]

     [:div.section [:h3 "Cities"]

      (show-cities)

      ]
     

     [:div.section [:h3 "Games"]

      (show-games)

      ]
     
     [:div.section [:h3 "Translation Selects"]
      [:button "New Translation Select"]

      (show-expressions)])))

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

(defn create-game [request]
  {:status 302
   :headers {"Location" (str "/editor/?message=Game+created.")}})

(defn create [request]
  (log/info (str "editor/new with request: " (:form-params request)))
  (fp/with-fallback #(create-form request :problems %)
    (let [values (fp/parse-params game-form (:form-params request))
          debug (log/debug (str "editor/new: values: " values))
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

  
