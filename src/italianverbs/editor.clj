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
   [italianverbs.html :as html]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.korma :as db]
   [italianverbs.unify :refer [get-in strip-refs]]
   [italianverbs.verb :refer [generation-table predicates-from-lexicon]]

   [hiccup.core :refer (html)]
   [korma.core :as k]
))

(declare show-games)
(declare show-groups)
(declare unabbrev)

(defn home-page [ & [ {game-to-delete :game-to-delete
                       game-to-edit :game-to-edit
                       group-to-edit :group-to-edit
                       group-to-delete :group-to-delete
                       message :message} ]]
  (html
   [:div.user-alert message]
   
   [:div.section [:h3 "Games"]
    (show-games {:game-to-delete game-to-delete
                 :game-to-edit game-to-edit})
    ]
   
   [:div.section [:h3 "Groups"]
    (show-groups {:group-to-delete group-to-delete
                  :group-to-edit group-to-edit})
                  
    ]
   ))

(defn insert-game [name source target source-set target-set]
  "Create a game with a name, a source and target language and two
  arrays, one for the source language and one for the target
  language. Each array is an integer which refers to a set of
  'anyof-sets', each member of which is a possible specification in its
  respective language. See example usage in test/editor.clj."
  (log/debug (str "source-set: " source-set))
  (log/debug (str "source-set with commas: " (str "ARRAY[" (string/join "," (map #(str "" (string/join "," %) "") source-set)) "]")))
  (log/debug (str "target-set with commas: " (str "ARRAY[" (string/join "," (map #(str "" (string/join "," %) "") target-set)) "]")))

  (let [source-grouping-str (str "ARRAY[" (string/join "," (map #(str "" (string/join "," %) "") source-set)) "]::integer[]")
        target-grouping-str (str "ARRAY[" (string/join "," (map #(str "" (string/join "," %) "") target-set)) "]::integer[]")
        sql (str "INSERT INTO game (name,source_groupings,target_groupings,source,target) 
                       SELECT ?, " source-grouping-str "," target-grouping-str ",?,? RETURNING id")]
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
                                     uniq(array_agg(source_groupings.id)) AS source_group_ids,
                                     uniq(array_agg(target_groupings.name)) AS target_groups,
                                     uniq(array_agg(target_groupings.id)) AS target_group_ids
                                FROM game 
                           LEFT JOIN grouping AS source_groupings 
                                  ON source_groupings.id = ANY(source_groupings) 
                           LEFT JOIN grouping AS target_groupings 
                                  ON target_groupings.id = ANY(target_groupings) GROUP BY game.id ORDER BY game.id"] :results)]
    (html

     [:table {:class "striped padded"}
      [:tr
       [:th {:style "width:9%"} 
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
             (let [game-id (:id result)]
               [:tr 
                [:td
                 (cond (not (or game-to-edit game-to-delete))
                       [:div {:style "width:100%"}
                        [:div.edit
                         [:button 
                          {:onclick (str "edit_game_dialog(" game-id ");")}
                          "Edit"]]
                        [:div.delete
                         [:button
                         {:onclick (str "document.location='/editor/game/delete/" game-id "';" )}
                          "Delete"]]]
                       
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
                [:td 

                 (if (not (empty? (remove nil? (seq (.getArray (:source_groups result))))))
                   (str 
                    "<div class='group sourcegroup'>"
                    (string/join "</div><div class='group sourcegroup'>" (sort (.getArray (:source_groups result))))
                    "</div>")
                   [:i "No source-language groups."])

                 ]

                [:td
                 (if (not (empty? (remove nil? (seq (.getArray (:target_groups result))))))
                   (str "<div class='group targetgroup'>"
                        (string/join "</div><div class='group targetgroup'>" (sort (.getArray (:target_groups result))))
                        "</div>")
                   [:i "No target-language groups."])]

                [:td 
                 (cond (= game-to-edit game-id)
                       [:div
                        [:form#update_game
                         {:method "POST"
                          :action (str "/editor/game/edit/" game-id)}]

                        [:button {:onclick (str "$(\"#update_game\").submit();")} "Confirm"]]

                       (= game-to-delete game-id)
                       [:div
                        [:form#confirm_delete_game
                         {:method "POST"
                          :action (str "/editor/game/delete/" game-id)}]

                        [:button.confirm_delete {:onclick (str "$(\"#confirm_delete_game\").submit();")} "Confirm"]]

                       true
                       "")]]))
                
           results)]

     ;; TODO: should be a POST (form submit), not a GET.
     [:div.new
      [:button {:onclick (str "document.location='/editor/game/new';")} "New Game"]
      ]
     
     (let [all-groups
           (k/exec-raw ["SELECT id,name FROM grouping"] :results)]

       ;; make the hidden game-editing forms.
       (map (fn [result]
              (let [game-id (:id result)
                    debug (log/debug (str "ALL GAME INFO: " result))
                    game-to-edit game-to-edit
                    problems nil ;; TODO: should be optional param of this (defn)
                    game-to-delete game-to-delete

                    source-group-ids (:source_group_ids result)
                    source-groups (if source-group-ids
                                    (vec (map str (remove nil? (.getArray (:source_group_ids result)))))
                                    [])
                    target-group-ids (:target_group_ids result)
                    target-groups (if target-group-ids
                                    (vec (map str (remove nil? (.getArray (:target_group_ids result)))))
                                    [])
                    debug (log/debug (str "SOURCE GROUPS: " source-groups))
                    debug (log/debug (str "TARGET GROUPS: " target-groups))
                    ]
                [:div.editgame
                 {:id (str "editgame" game-id)}
                 [:h2 (str "Editing game: " (:game_name result))]

                 (f/render-form 
                  {:action (str "/editor/game/edit/" game-id)
                   :method :post
                   :fields [{:name :name :size 50 :label "Name"}
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
                          
                            {:name :source_groupings
                             :type :checkboxes
                             :cols 3
                             :options (map (fn [grouping]
                                             {:value (:id grouping)
                                              :label (:name grouping)})
                                           all-groups)}

                            {:name :target_groupings
                             :type :checkboxes
                             :cols 3
                             :options (map (fn [grouping]
                                             {:value (:id grouping)
                                              :label (:name grouping)})
                                           all-groups)}

                            ]
                   
                   :cancel-href "/editor"
                   :values {:name (:game_name result)
                            :target (:target result)
                            :source (:source result)
                            :source_groupings source-groups
                            :target_groupings target-groups}

                   :validations [[:required [:name]   
                                  :action "/editor"
                                  :method "post"
                                  :problems problems]]})]
                ))
            results))
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
                                                         (cond 
                                                          (and (or (= k :english) (= k :espanol) (= k :italiano))
                                                               (not (map? v)))
                                                          (str v)
                                                          (string? v)
                                                          (keyword v)
                                                          :else v)))
            :target-structure (json/read-str (str (:target_structure row))
                                             :key-fn keyword
                                             :value-fn (fn [k v]
                                                         (cond 
                                                          (and (or (= k :english) (= k :espanol) (= k :italiano))
                                                               (not (map? v)))
                                                          (str v)
                                                          (string? v)
                                                          (keyword v)
                                                          :else v)))})

         (k/exec-raw [sql
                      [game-id game-id game-id game-id]] :results))))

(declare body)
(declare control-panel)
(declare create)
(declare delete)
(declare delete-game)
(declare delete-group)
(declare update-game)
(declare edit-group)
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
        (is-admin
         {:status 302
          :headers {"Location" "/editor"}}))
  
   (POST "/create" request
         (is-admin (create request)))

   (GET "/game/delete/:game-to-delete" request
        (is-admin
         (let [game-to-delete (:game-to-delete (:route-params request))]
           {:body (body (str "Editor: Confirm: delete game: " game-to-delete)
                        (home-page {:game-to-delete game-to-delete}) 
                        request)
            :status 200
            :headers headers})))

   (GET "/group/delete/:group-to-delete" request
        (is-admin
         (let [group-to-delete (:group-to-delete (:route-params request))]
           {:body (body (str "Editor: Confirm: delete group: " group-to-delete)
                        (home-page {:group-to-delete group-to-delete})
                        request)
            :status 200
            :headers headers})))

   (POST "/game/delete/:game-to-delete" request
         (is-admin (delete-game (:game-to-delete (:route-params request)))))

   (POST "/group/delete/:group-to-delete" request
         (is-admin (delete-group (:group-to-delete (:route-params request)))))

   (GET "/game/edit/:game-to-edit" request
        (let [game-to-edit (:game-to-edit (:route-params request))]
          (is-admin {:body (body (str "Editor: Editing game: " game-to-edit)
                                 (home-page {:game-to-edit game-to-edit}) 
                                 request)
                     :status 200
                     :headers headers})))

   (POST "/game/edit/:game-to-edit" request
         (is-admin (update-game (:game-to-edit (:route-params request))
                              (:params request))))

   (POST "/group/edit/:group-to-edit" request
         (is-admin (edit-group (:group-to-edit (:route-params request))
                               (:params request))))

   ;; TODO: should be a POST
   (GET "/game/new" request
        (do
          ;; Defaults: source language=English, target language=Italian.
          (insert-game "untitled" "en" "it" [] [])
          (is-admin {:status 302
                     :headers {"Location" "/editor"}})))

   (GET "/group/new" request
        (is-admin
         (do
           (insert-grouping "untitled" [{}])
           {:status 302
            :headers {"Location" "/editor"}})))

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
                                                             (cond (and (or (= k :english) (= k :espanol) (= k :italiano))
                                                                        (not (map? v)))
                                                                   (str v)
                                                                   (string? v)
                                                                   (keyword v)
                                                                   :else v))))
                     target_sem (let [sem (str (:target_sem row))]
                                  (json/read-str sem
                                                 :key-fn keyword
                                                 :value-fn (fn [k v]
                                                             (cond (and (or (= k :english) (= k :espanol) (= k :italiano))
                                                                        (not (map? v)))
                                                                   (str v)
                                                                   (string? v)
                                                                   (keyword v)
                                                                   :else v))))]
                 [:tr 
                  [:td source]
                  [:td target]
                  [:td [:div {:onclick "toggle_expand(this);" :style "height:100px; width:200px ;overflow:scroll"} (html/tablize source_sem)]]
                  [:td [:div {:onclick "toggle_expand(this);" :style "height:100px; width:200px ;overflow:scroll"} (html/tablize target_sem)]]]))
             examples)]])]))

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

(defn delete-game [game-id]
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

(defn delete-group [group-id]
  (if group-id
    (do
      (log/debug (str "DELETING GROUP: " group-id))
      (let [group-id (Integer. group-id)
            group-row (first (k/exec-raw [(str "SELECT * FROM grouping WHERE id=?") [group-id]] :results))]
        (log/debug (str "GROUP ROW: " group-row))
        (k/exec-raw [(str "DELETE FROM grouping WHERE id=?") [group-id]])
        {:status 302
         :headers {"Location" (str "/editor/" "?message=Deleted+group:" (:name group-row))}}))
    {:status 302
     :headers {"Location" (str "/editor/" "?message=no+group+to+delete")}}))

(defn update-game [game-id params]
  (log/debug (str "UPDATING GAME WITH PARAMS: " params))
  (let [game-id game-id

        source-grouping-set (:source_groupings params)
        target-grouping-set (:target_groupings params)

        debug (log/debug (str "edit: source-groupings type(1):" (type source-grouping-set)))
        debug (log/debug (str "edit: target-groupings type(1):" (type target-grouping-set)))

        debug (log/debug (str "edit: source-groupings(1):" source-grouping-set))
        debug (log/debug (str "edit: target-groupings(1):" target-grouping-set))


        ;; cleanup
        source-grouping-set (vec (map #(Integer/parseInt %)
                                      (remove #(= "" %)
                                              source-grouping-set)))
        
        target-grouping-set (vec (map #(Integer/parseInt %)
                                      (remove #(= "" %)
                                              target-grouping-set)))
                                 
        debug (log/debug (str "edit: source-groupings length(2):" (.length source-grouping-set)))
        debug (log/debug (str "edit: target-groupings length(2):" (.length target-grouping-set)))

        debug (log/debug (str "edit: source-groupings type(2):" (type source-grouping-set)))
        debug (log/debug (str "edit: target-groupings type(2):" (type target-grouping-set)))

        debug (log/debug (str "edit: source-groupings(2):" source-grouping-set))
        debug (log/debug (str "edit: target-groupings(2):" target-grouping-set))


        source-grouping-str (str "ARRAY[" (string/join "," source-grouping-set) "]::integer[]")

        target-grouping-str (str "ARRAY[" (string/join "," target-grouping-set) "]::integer[]")]

    (log/debug (str "Editing game with id= " game-id))

    (let [sql (str "UPDATE game SET (name,source,target,source_groupings,target_groupings) = (?,?,?," 
                   source-grouping-str "," target-grouping-str ") WHERE id=?")]

      (log/debug (str "UPDATE sql: " sql))
      (k/exec-raw [sql
                   [(:name params)
                    (:source params)
                    (:target params)
                    (Integer. game-id)]])

      {:status 302
       :headers {"Location" (str "/editor/" "?message=Edited+game:" game-id)}})))

(defn edit-group [group-id params]
  (log/debug (str "UPDATING GROUP WITH PARAMS: " params))
  {:headers {"Content-type" "application/json;charset=utf-8"}
   :body (json/write-str params)})
;  {:status 302
;   :headers {"Location" (str "/editor/" "?message=Edited+group:" group-id)}})

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

(defn show-groups [ & [{group-to-edit :group-to-edit
                        group-to-delete :group-to-delete}]]
  (let [group-to-edit (if group-to-edit (Integer. group-to-edit))
        group-to-delete (if group-to-delete (Integer. group-to-delete))
        results (k/exec-raw ["SELECT id,name,any_of 
                                FROM grouping 
                            ORDER BY name ASC"] :results)]
    (html
     [:table {:class "striped padded"}
      [:tr
       [:th {:style "width:9%"} 
        (cond
         (or group-to-edit group-to-delete)
         "Cancel"
         :else "")
        ]

       [:th "Name"]
       [:th "Any Of"]

       ]
      (map (fn [result]
             (let [group-id (:id result)]
               [:tr 
                [:td
                 (cond (not (or group-to-edit group-to-delete))
                       [:div {:style "width:100%"}
                        [:div.edit
                         [:button 
                          {:onclick (str "edit_group_dialog(" group-id ");")}
                          "Edit"]]
                        [:div.delete
                         [:button
                          {:onclick (str "document.location='/editor/group/delete/" group-id "';" )}
                          "Delete"]]]
                       
                       (or (= group-to-edit group-id)
                           (= group-to-delete group-id))
                       [:a
                        {:href "/editor"} "Cancel"]

                     true "")]

              [:td [:div.group (:name result)]]
              [:td 
               (str "<div class='anyof'>" 
                    (string/join 
                     "</div><div class='anyof'>" 
                     (map #(json/read-str 
                            % 
                            :key-fn keyword
                            :value-fn (fn [k v]
                                        (cond (and
                                               ;; TODO: improve
                                               (or (= k :english)
                                                   (= k :espanol)
                                                   (= k :italiano))
                                               (not (map? v)))
                                              (str v)
                                              (string? v)
                                              (keyword v)
                                              :else v)))
                          (.getArray (:any_of result))))
                    "</div>")]

                [:td 
                 (cond (= group-to-edit group-id)
                       [:div
                        [:form#update_group
                         {:method "POST"
                          :action (str "/editor/group/edit/" group-id)}]
                        
                        [:button {:onclick (str "$(\"#update_group\").submit();")} "Confirm"]]
                       
                       (= group-to-delete group-id)
                       [:div
                        [:form#confirm_delete_group
                         {:method "POST"
                          :action (str "/editor/group/delete/" group-id)}]
                        
                        [:button.confirm_delete {:onclick (str "$(\"#confirm_delete_group\").submit();")} "Confirm"]]
                       
                       true
                       "")]]))
              
           results)]
     [:div.new
      [:button {:onclick (str "document.location='/editor/group/new';")} "New Group"]
      ]

     (let [results
           ;; add more stuff to SELECT; this is just to get things working.
           (k/exec-raw ["SELECT id,name AS group_name,any_of FROM grouping"] :results)]

       ;; make the hidden group-editing forms.
       (map (fn [result]
              (let [group-id (:id result)
                    debug (log/debug (str "ALL GROUP INFO: " result))
                    group-to-edit group-to-edit
                    problems nil ;; TODO: should be optional param of this (defn)
                    group-to-delete group-to-delete

                    ]
                [:div.editgroup
                 {:id (str "editgroup" group-id)}
                 [:h2 (str "Editing group: " (:group_name result))]

                 (f/render-form 
                  {:action (str "/editor/group/edit/" group-id)
                   :method :post
                   :fields (concat

                            [{:name :name :size 50 :label "Name"}
                             {:name :note :type :html
                              :html [:div.alert.alert-info "Leave a specification blank below to remove it."]}]

                            (map (fn [each-spec-index]
                                   {:name (keyword (str "any_of" each-spec-index))
                                    :label " "
                                    :type :textarea
                                    :rows 3
                                    :cols 80})
                                 (if (and (:any_of result)
                                          (.getArray (:any_of result)))
                                   (range 0 (.size (seq (.getArray (:any_of result)))))
                                   []))

                            [{:name "new_spec"
                              :label "New Spec:"
                              :type :textarea
                              :rows 3
                              :cols 80}]
                            )

                   :cancel-href "/editor"
                   :values (reduce merge 
                                   (cons {:name (:group_name result)
                                          :new_spec ""}
                                         (map (fn [each-spec-index]
                                                {(keyword (str "any_of" each-spec-index))
                                                 (nth (seq (.getArray (:any_of result))) each-spec-index)})
                                              (if (and (:any_of result)
                                                       (.getArray (:any_of result)))
                                                (range 0 (.size (seq (.getArray (:any_of result)))))
                                                []))))
                   
                   :validations [[:required [:name]   
                                  :action "/editor"
                                  :method "post"
                                  :problems problems]]})]))
            results)))))

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

;                   :fields [{:name :source_groups
;                                              :type :checkboxes
;                                              :options ["Chocolate" "Vanilla"]}]

(defn short-language-name-to-long [lang]
  (cond (= lang "it") "Italian"
        (= lang "en") "English"
        (= lang "es") "Spanish"
        true "???"))

(defn unabbrev [lang]
  (short-language-name-to-long lang))

