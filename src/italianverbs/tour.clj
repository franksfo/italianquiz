(ns italianverbs.tour
  (:refer-clojure :exclude [get-in merge])
  (:require
   [clojure.data.json :refer [write-str]]
   [clojure.tools.logging :as log]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
   [italianverbs.borges.reader :refer [generate-question-and-correct-set]]
   [italianverbs.html :refer [page]]
   [italianverbs.morphology :refer (fo remove-parens)]
   [italianverbs.unify :refer (get-in unify)]
   [korma.core :as k]))

;; For now, source language and locale are constant.
(def source-language "en")
(def source-locale "US")

(declare direction-chooser)
(declare generate-q-and-a)
(declare tour)

(def routes
  (let [headers {"Content-Type" "text/html;charset=utf-8"}]

    (compojure/routes

     (GET "/it" request
          {:headers headers
           :status 200
           :body (page "Map Tour" (tour "it" "IT") request {:onload "start_tour('it','IT');"
                                                            :css ["/css/tour.css"]
                                                            :jss ["/js/cities.js"
                                                                  "/js/gen.js"
                                                                  "/js/leaflet.js"
                                                                  "/js/it.js"
                                                                  "/js/tour.js"]})})
     (GET "/es" request
          {:status 302
           :headers {"Location" "/tour/es/ES"}})

     (GET "/es/ES" request
          {:status 200
           :headers headers
           :body (page "Map Tour" (tour "es" "ES") request {:onload "start_tour('es','ES');"
                                                       :css ["/css/tour.css"]
                                                       :jss ["/js/cities.js"
                                                             "/js/gen.js"
                                                             "/js/leaflet.js"
                                                             "/js/es.js"
                                                             "/js/tour.js"]})})
     (GET "/es/MX" request
          {:status 200
           :headers headers
           :body (page "Map Tour" (tour "es" "MX") request {:onload "start_tour('es','MX');"
                                                            :css ["/css/tour.css"]
                                                            :jss ["/js/cities.js"
                                                                  "/js/gen.js"
                                                                  "/js/leaflet.js"
                                                                  "/js/es.js"
                                                                  "/js/tour.js"]})})

     (GET "/it/generate-q-and-a" request
          {:status 302
           :headers {"Location" "/tour/it/IT/generate-q-and-a"}})

     (GET "/it/IT/generate-q-and-a" request
          (generate-q-and-a "it" "IT" request))

    (GET "/es/ES/generate-q-and-a" request
         (generate-q-and-a "es" "ES" request))

     (GET "/es/MX/generate-q-and-a" request
          (generate-q-and-a "es" "MX" request))

     ;; Default: no locale given.
     ;; TODO: redirect to /tour/es/es/ES/generate-q-and-a
     (GET "/es/generate-q-and-a" request
          (generate-q-and-a "es" "ES" request))

     ;; below URLs are for backwards-compatibility:
     (GET "/" request
          {:status 302
           :headers {"Location" "/tour/it"}})

     (GET "/generate-q-and-a" request
          {:status 302
           :headers {"Location" "/tour/it/generate-q-and-a"}}))))

(defn get-game-spec [target-language]
  (let [results (k/exec-raw
                 [(str "SELECT name,source,target,
                              source_spec::text AS source_spec,
                             target_spec::text AS target_spec 
                          FROM translation_select
                         WHERE target=? LIMIT 1") [target-language]] :results)]
    {:target_spec
     (get (first results) :target_spec)
     :source_spec
     (get (first results) :source_spec)}))

(defn generate-q-and-a [target-language target-locale request]
  "generate a question in English and a set of possible correct answers in the target language, given parameters in request"
  (let [headers {"Content-Type" "application/json;charset=utf-8"
                 "Cache-Control" "no-cache, no-store, must-revalidate"
                 "Pragma" "no-cache"
                 "Expires" "0"}]
    (try (let [spec (get (:form-params request) "spec" :top)
               game-spec (get-game-spec target-language)
               debug (log/debug (str "game-spec: " game-spec))
               pair 
               (generate-question-and-correct-set spec source-language source-locale
                                                  target-language target-locale)]
           {:status 200
            :headers headers
            :body (write-str
                   pair)})
         (catch Exception e
           (do
             (log/error (str "attempt to (generate-question-and-correct-set) threw an error: " e))
             {:status 500
              :headers headers
              :body (write-str {:exception (str e)})})))))

(defn accent-characters [language locale]
  (cond (= language "it")
        [:div.accents
         [:button.accented {:onclick (str "add_a_grave('it','IT');")} "&agrave;"]
         [:button.accented {:onclick (str "add_e_grave('it','IT');")} "&egrave;"]
         [:button.accented {:onclick (str "add_o_grave('it','IT');")} "&ograve;"]
         ]
        (= language "es")
        (cond (= locale "MX")
              [:div.accents
               [:button.accented {:onclick (str "add_a_acute('es','MX');")} "&aacute;"]
               [:button.accented {:onclick (str "add_e_acute('es','MX');")} "&eacute;"]
               [:button.accented {:onclick (str "add_i_acute('es','MX');")} "&iacute;"]
               [:button.accented {:onclick (str "add_n_tilde('es','MX');")} "&ntilde;"]
               [:button.accented {:onclick (str "add_u_acute('es','MX');")} "&uacute;"]]
              :else
              [:div.accents
               [:button.accented {:onclick (str "add_a_acute('es','ES');")} "&aacute;"]
               [:button.accented {:onclick (str "add_e_acute('es','ES');")} "&eacute;"]
               [:button.accented {:onclick (str "add_i_acute('es','ES');")} "&iacute;"]
               [:button.accented {:onclick (str "add_n_tilde('es','ES');")} "&ntilde;"]
               [:button.accented {:onclick (str "add_u_acute('es','ES');")} "&uacute;"]])
        true
        ""))

(defn dont-know [language locale]
  (let [non_lo_so
        (cond (= language "it")
              (str "non_lo_so('it','IT');")
              (and (= language "es")
                   (= locale "MX"))
              (str "non_lo_so('es','MX');")
              (= language "es")
              (str "non_lo_so('es','ES');")
              :else
              (str "non_lo_so('it','IT');"))]
    [:button#non_lo_so {:onclick non_lo_so}
     (cond (= language "it")
           "Non lo so"
           (= language "es")
           "No sé"
           true
           "")]))

;; TODO: Move this to javascript (tour.js) - tour.clj should only be involved in
;; routing requests to responses.
(defn tour [language locale]
  [:div#game

   [:div#correctanswer 
    " "
    ]

    (direction-chooser)

    [:div#map ]

   [:div#streetview
    [:iframe#streetviewiframe
     {:src ""}] ;; src value is filled in with Javascript.
    ]

   [:div#tourgameform

    [:div#tourquestion
     ""
     ]

    [:div#gameinputdiv
      [:input#gameinput {:size "20"}]
     
     (accent-characters language locale)

     (dont-know language locale)]

    [:div#userprogresscontainer
     [:div#userprogress 
      ]]

     
    [:div#kilos {:style "z-index:4"}
     "Score:"
     [:span#scorevalue
      "0"
      ]
     ]
    ]

   ]
  )

(def game-pairs
  [{:source "en"
    :destination "it"
    :source_flag "/svg/britain.svg"
    :destination_flag "/svg/italy.svg"}])

(defn get-possible-games [] 
  (map (fn [row]
         (keyword (:word row)))
       (k/exec-raw [(str "SELECT * FROM games_to_use ")] :results)))

(defn choose-random-verb-group []
  "choose a verb group randomly from the games that are currently possible to play as determined by the games_to_use table."
  (let [games (k/exec-raw [(str "SELECT id FROM games WHERE games.id IN (SELECT game FROM games_to_use)")] :results)]
    (:id (nth games (rand-int (.size games))))))

(defn get-possible-preds [game-id]
  (map (fn [row]
         (:pred row))
       (k/exec-raw [(str "SELECT word AS pred
                            FROM words_per_game
                      INNER JOIN games
                              ON words_per_game.game = games.id
                           WHERE games.id = ?") [game-id]] :results)))

(defn get-possible-inflections [game-id]
  (map (fn [row]
         (:inflection row))
       (k/exec-raw [(str "SELECT inflection
                            FROM inflections_per_game
                           WHERE game = ?") [game-id]] :results)))

(defn direction-chooser []
  [:dev#chooser
   [:select {:style "display:none" :disabled "true" };; TODO: not working yet, so disabled.
    [:option {:onclick "location='/cloud?src=en&dest=it';"}
     "en -> it"
     ]
    
    [:option {:onclick "location='/cloud?src=it&dest=en';"}
     "it -> en"
     ]
    
    [:option {:onclick "location='/cloud?src=en&dest=es';"}
     "en -> es"
     ]

    [:option {:onclick "location='/cloud?src=es&dest=en';"}
     "es -> en"
     ]
    ]])

(defn evaluate [user-response]
  (let [params (:form-params user-response)]
    (log/info (str "keys: " (keys params)))
    (let [guess (get-in params '("guess"))]
      (log/info (str "guess: " (get-in params '("guess"))))
      (str "clojure saw your response:" guess))))

(defn map-realize [the-fn seq]
  (if (not (empty? seq))
    (cons (apply the-fn (list (first seq)))
          (map-realize the-fn (rest seq)))))

(defn html-form [question]
  (do
    (log/info (str "html-form: question: " (fo question)))
    {:left_context_source (remove-parens (fo (get-in question [:comp])))
     :head_of_source (remove-parens (fo (get-in question [:head])))
     :right_context_source ""
     :right_context_destination ""}))

(defn additional-generation-constraints [spec]
  "apply additional constraints to improve generation results or performance."
  (cond false ;;(= generate-by :runtime)
        (unify spec
               {:head {:phrasal :top}
                :comp {:phrasal false}})
        true
        spec))
