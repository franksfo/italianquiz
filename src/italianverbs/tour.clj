(ns italianverbs.tour
  (:refer-clojure :exclude [get-in merge])
  (:use [hiccup core page])
  (:require
   [clojure.data.json :refer [read-str write-str]]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
   [hiccup.page :refer (html5)]

   [italianverbs.engine :refer [generate]]
   [italianverbs.html :refer [page tablize]]
   [italianverbs.morphology :refer [fo fo-ps remove-parens]]
   [italianverbs.translate :refer [get-meaning]]
   [italianverbs.ug :refer (head-principle)]
   [italianverbs.unify :refer [fail? get-in merge strip-refs unify]]
   [italianverbs.english :as en]
   [italianverbs.italiano :as it]
   [korma.core :as k]))

(declare evaluate)
(declare generate-answers)
(declare generate-question)
(declare tour)

(def routes
  (compojure/routes
   (GET "/" request
        {:status 200
         :body (page "Map Tour" (tour) request {:onload "start_tour();"
                                                :css ["/css/tour.css"]
                                                :jss ["/js/tour.js" "/js/gen.js"
                                                      "http://cdn.leafletjs.com/leaflet-0.7.3/leaflet.js"]})})

   (POST "/evaluate" request
         {:status 200
          :headers {"Content-Type" "text/html;charset=utf-8"}
          :body (evaluate request)})

   (GET "/generate-answers" request
        (generate-answers request))

   (GET "/generate-question" request
        (generate-question request))))

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

(defn generate-question [request]
  (let [verb-group (choose-random-verb-group)
        possible-preds (get-possible-preds verb-group)
        possible-inflections (get-possible-inflections verb-group)
        pred (if (not (= :null (get-in request [:params :pred] :null)))
               (keyword (get-in request [:params :pred]))
               (nth possible-preds (rand-int (.size possible-preds))))
        debug (log/info (str "generate-question: pred: " pred))
        debug (log/info (str "possible-inflections: " verb-group))
        spec
        {:head {:phrasal :top}
         :comp {:phrasal false}
         :synsem {:sem {:pred pred}
                  :cat :verb
                  :subcat '()}}

        ;; TODO: use runtime to decide which language rather than
        ;; hard-coded en/inflection.
        spec (unify spec
                    (en/inflection (keyword (nth possible-inflections (rand-int (.size possible-inflections))))))

        ;; TODO: use runtime to decide which language and grammar rather than
        ;; hard-coded en/small.
        question (generate spec en/small)
        form (html-form question)]

    (log/info "generate-question: question: " (fo question))

    {:status 200
     :headers {"Content-Type" "application/json;charset=utf-8"
               
               "Cache-Control" "no-cache, no-store, must-revalidate"
               "Pragma" "no-cache"
               "Expires" "0"}
     :body (write-str
            {:left_context_of_question (:left_context_source form)
             :full_question (fo question)
             :question (:head_of_source form)
             :right_context_of_question (:right_context_source form)
             :semantics (strip-refs (get-meaning question))})}))

;; This is the counterpart to (generate-question) immediately above: it generates
;; an expression that the user should be learning.
(defn generate-answers [request]
  "generate a single sentence according to the semantics of the request."
  ;; TODO: generate more than one answer, possibly.
  (log/info (str "generate-answers: request semantics: " (get-in request [:params :semantics])))
  (log/info (str "cloud_id: " (get-in request [:params :cloud_id])))
  (let [semantics (read-str (get-in request [:params :semantics])
                            :key-fn keyword
                            :value-fn (fn [k v]
                                        (cond (string? v)
                                              (keyword v)
                                              :else v)))
        debug (log/debug (str "semantics: " semantics))

        more-constraints {:synsem {:subcat '()}
                          :head {:phrasal :top}
                          :comp {:phrasal false}}

        to-generate (merge semantics more-constraints)

        debug (log/info (str "to-generate: " to-generate))

        debug (log/info (str "enriched   : " (string/join " ; " (it/enrich to-generate))))

        ;; TODO: for now, we are hard-wired to generate an answer in Italian,
        ;; but function should accept an input parameter to determine which language should be
        ;; used.
        answer (generate (it/enrich to-generate) it/small)

        ;; used to group questions by some common feature - in this case,
        ;; we'll use the pred since this is a way of cross-linguistically
        ;; grouping verbs with the same meaning together.
        group_by (get-in answer [:synsem :sem :pred])

        debug (log/info (str "generate-answers: group_by: " group_by))

        debug (log/info (str "generate-answers: answers: " (remove-parens (fo answer))))
        ]

    (if (not (= "true" (get-in request [:params :debug])))
      {:status 200
       :headers {"Content-Type" "application/json;charset=utf-8"}
       :body
       (write-str
        {:cloud_id (get-in request [:params :cloud_id])
         :group_by group_by

         ;; left-context is given here because the user should be able to omit
         ;; this without penalty.
         :left_context_of_answer (remove-parens (fo (get-in answer [:comp])))

         ;; here we combine all of the possible answers,
         ;; using punctuation within the string as means of delimitation (a comma).
         ;; TODO: use a JSON array instead, and also, remove the use of (remove-parens),
         ;; since this is another way that punctuation being abused to serialize data structures.
         :answer (string/join ", "
                              (list 
                               (remove-parens (fo answer))))
         :semantics semantics
         :right_context_of_answer ""})}

      {:status 200
       :headers {"Content-Type" "text/html;charset=utf-8"
                 "Cache-Control" "no-cache, no-store, must-revalidate"
                 "Pragma" "no-cache"
                 "Expires" "0"}
       :body (html
              [:head
               [:title "tour: answers debug"]
               (include-css "/css/fs.css")
               (include-css "/css/layout.css")
               (include-css "/css/quiz.css")
               (include-css "/css/style.css")
               (include-css "/css/debug.css")
               ]
              [:body
               [:div

                [:div.major
                 [:h2 "input"]

                 (tablize {:semantics (read-str (get-in request [:params :semantics])
                                                :key-fn keyword
                                                :value-fn (fn [k v]
                                                            (cond (string? v)
                                                                  (keyword v)
                                                                  :else v)))})
                 

                 ]

                [:div.major
                 [:h2 "output"]
                 (tablize {:answer-full answer
                           :left-context-of-answer (remove-parens (fo (get-in answer [:comp])))
                           :answer (string/join ", "
                                                (list
                                                 (remove-parens (fo answer))))})]


                ]])})))

(defn tour []
  [:h3 {:style "background:lightgreen;padding:0.25em"} "Benvenuto a Napoli!"]

  [:div#game

   [:svg {:id "svgarena"}]

   [:div#rainforest

    [:div#wordbar

     [:div#q1 "wordbar"]

     [:div#q2 "not used"]

     [:div#q3 "yet"]

     ]
     
    (direction-chooser)
     
    [:div#kilos {:style "z-index:4"}
     "Score:"
     [:span#scorevalue
      "0"
      ]
     ]

    [:div#sidebyside {:style "z-index:2"}
      
     ;; map and streetview should be side by side
     [:div#map ]

     [:div#streetview
      [:img#streetviewimage
       {:src ""}]] ;; src value is filled in with Javascript.
     ]

    [:div#correction_dialog {:style "display:none"}
     [:form {:onsubmit "return false;"}
      [:div#cd_left_context_of_answer {:class "correct_answer" } "" ]
      [:div#cd_rca {:class "correct_answer" } "" ]
      [:h3#correct_answer "" ]
      [:div#full_question {:class "question"} " " ]
      [:input {:id "correction_bare_id" :type "hidden"}]
      ]
     ;; end of :form

     ] ;; end of :div #correction_dialog


    ] ;; end of :div#rainforest

   [:div#tourgameform

    [:div#tourquestion
     ""
     ]

    [:input {:id "gameinput" :size "30"}]

    [:div#userprogress 
     ]

    [:div#correctanswer 
     ]


    ] ;; end of :div #gameform
   ] ;; end of :div #game

) ;; end of (defn)

