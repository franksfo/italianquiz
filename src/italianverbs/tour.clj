(ns italianverbs.tour
  (:refer-clojure :exclude [get-in merge])
  (:require
   [clojure.data.json :refer [read-str write-str]]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
   [hiccup.core :refer (html)]
   [hiccup.page :refer (include-css html5)]
   [italianverbs.borges.reader :refer [generate-question-and-correct-set]]
   [italianverbs.html :as html :refer (page tablize)]
   [italianverbs.morphology :refer (fo remove-parens)]
   [italianverbs.unify :refer (get-in merge strip-refs unify)]
   [korma.core :as k]))

(declare direction-chooser)
(declare generate-q-and-a)
(declare tour)

(def routes
  (let [headers {"Content-Type" "text/html;charset=utf-8"}]

    (compojure/routes

     (GET "/it" request
          {:headers headers
           :status 200
           :body (page "Map Tour" (tour "it") request {:onload "start_tour('it');"
                                                       :css ["/css/tour.css"]
                                                       :jss ["/js/cities.js"
                                                             "/js/gen.js"
                                                             "/js/leaflet.js"
                                                             "/js/it.js"
                                                             "/js/tour.js"]})})
     (GET "/es" request
          {:status 200
           :headers headers
           :body (page "Map Tour" (tour "es") request {:onload "start_tour('es');"
                                                       :css ["/css/tour.css"]
                                                       :jss ["/js/cities.js"
                                                             "/js/gen.js"
                                                             "/js/leaflet.js"
                                                             "/js/es.js"
                                                             "/js/tour.js"]})})

     (GET "/it/generate-q-and-a" request
          (generate-q-and-a "it" request))

     (GET "/es/generate-q-and-a" request
          (generate-q-and-a "es" request))

     (GET "/es" request
          {:status 302
           :headers {"Location" "/tour/es"}})

     (GET "/it" request
          {:status 302
           :headers {"Location" "/tour/it"}})

     ;; below URLs are for backwards-compatibility:
     (GET "/" request
          {:status 302
           :headers {"Location" "/tour/it"}})

     (GET "/generate-q-and-a" request
          {:status 302
           :headers {"Location" "/tour/it/generate-q-and-a"}}))))

(defn generate-q-and-a [target-language request]
  "generate a question in English and a set of possible correct answers in the target language, given parameters in request"
  (let [headers {"Content-Type" "application/json;charset=utf-8"
                 "Cache-Control" "no-cache, no-store, must-revalidate"
                 "Pragma" "no-cache"
                 "Expires" "0"}]
    (try (let [spec (get (:form-params request) "spec" :top)
               debug
               (log/debug (str "generate-q-and-a for spec: " spec))
               pair 
               (generate-question-and-correct-set spec "en" target-language)]
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

(defn accent-characters [language]
  (cond (= language "it")
        [:div.accents
         [:button.accented {:onclick (str "add_a_grave_it();")} "&agrave;"]
         [:button.accented {:onclick (str "add_e_grave_it();")} "&egrave;"]
         [:button.accented {:onclick (str "add_o_grave_it();")} "&ograve;"]
         ]
        (= language "es")
        [:div.accents
         [:button.accented {:onclick (str "add_e_acute_es();")} "&eacute;"]
         [:button.accented {:onclick (str "add_i_acute_es();")} "&iacute;"]
         [:button.accented {:onclick (str "add_n_tilde_es();")} "&ntilde;"]
         [:button.accented {:onclick (str "add_u_acute_es();")} "&uacute;"]]
        true
        ""))

(defn dont-know [language]
  (cond (= language "it")
        "Non lo so"
        (= language "es")
        "No se"
        true
        ""))

;; TODO: Move this to javascript (tour.js) - tour.clj should only be involved in
;; routing requests to responses.
(defn tour [language]
  [:div#game

   [:div#correctanswer 
    " "
    ]


   [:svg {:id "svgarena"}]
     
    (direction-chooser)
     
    [:div#kilos {:style "z-index:4"}
     "Score:"
     [:span#scorevalue
      "0"
      ]
     ]

    [:div#map ]

   [:div#streetview
    [:img#streetviewimage
     {:src ""}] ;; src value is filled in with Javascript.
    ]

   [:div#tourgameform

    [:div#tourquestion
     ""
     ]

    [:div#gameinputdiv
      [:input {:id "gameinput" :size "20"}]]
     
     (accent-characters language)

      [:button {:id "non_lo_so" :onclick "non_lo_so();"}  (dont-know language)]

    [:div#userprogresscontainer
     [:div#userprogress 
      ]]

    [:table#navigation
     [:tr
      [:td [:input {:id "lat" :size "5"}]]
      [:td [:input {:id "long" :size "5"}]]
      [:td [:input {:id "offset" :size "5"}]]
      [:td [:input {:id "quadrant" :size "5" :val ""  }]]
      [:td [:input {:id "heading" :size "5"}]]]

     [:tr
      [:td [:input {:id "lat1" :size "5"}]]
      [:td [:input {:id "long1" :size "5"}]]
      ]
     ]

    ] ;; end of :div #gameform
   ] ;; end of :div #game

) ;; end of (defn)

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
