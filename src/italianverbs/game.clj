(ns italianverbs.game
  (:refer-clojure :exclude [get-in merge])
  (:require
   [clojure.data.json :as json]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [hiccup.page :refer :all]
   [hiccup.page :as h]
   [italianverbs.cache :refer (create-index)]
   [italianverbs.english :as en]
   [italianverbs.generate :as gen]
   [italianverbs.morphology :as morph]
   [italianverbs.morphology :refer [fo fo-ps]]
   [italianverbs.html :as html]
   [italianverbs.italiano :as it]
   [italianverbs.translate :refer [get-meaning]]
   [italianverbs.ug :refer (head-principle)]
   [italianverbs.unify :refer [get-in merge strip-refs unifyc]]))

(def game-pairs
  [{:source "en"
    :destination "it"
    :source_flag "/svg/britain.svg"
    :destination_flag "/svg/italy.svg"}])

(def possible-preds [:bere :leggere :parlare])

(defn direction-chooser []
  (h/html5
   [:dev#chooser
    [:select
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
     ]]))

(defn game []
  (h/html5
   [:div#game

    [:svg {:id "svgarena"}]

    [:div#rainforest

     [:div#wordbar

      [:div#q1 "wordbar"]

      [:div#q2 "not used"]

      [:div#q3 "yet"]

      ]
     
     (direction-chooser)
     
     [:div#score
      "Score:"
      [:span#scorevalue
       "0"
       ]
     ]

     [:div#sky ]

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



     [:div#ground
      


      ] ;; end of :div#ground

     ] ;; end of :div#rainforest

    [:div#gameform


     ;; e.g., if the Italian is "io parlo", and the English is "I speak", and you are supposed to answer "parlo", then
     ;; the left_context_destination will be "io".
     [:span {:id "left_context_destination"}
      ]


      [:input {:id "game_input" :size "50"}]
     
      [:button#answer_button {:class "click;float:right;width:auto"
                             :onclick "submit_game_response('game_input'); event.preventDefault(); return false;"} "Answer" ]
     
      ] ;; end of :div #gameform
    ] ;; end of :div #game
) ; html5/div

) ;; end of (defn)

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
    {:left_context_source (morph/remove-parens (fo (get-in question [:comp])))
     :head_of_source (morph/remove-parens (fo (get-in question [:head])))
     :right_context_source ""
     :right_context_destination ""}))

(def mini-en-grammar
  (filter #(= (:rule %) "s-present")
          en/grammar))

(def mini-en-index (create-index mini-en-grammar (flatten (vals en/lexicon)) head-principle))

(def mini-it-grammar
  (filter #(= (:rule %) "s-present")
          it/grammar))

(def mini-it-index (create-index mini-it-grammar (flatten (vals it/lexicon)) head-principle))

(defn generate-question [request]
  (let [pred (nth possible-preds (rand-int (.size possible-preds)))
        spec
        {:head {:phrasal :top}
         :comp {:phrasal false}
         :synsem {:sem {:pred pred}
                  :cat :verb
                  :subcat '()}}
        question (en/generate spec {:grammar mini-en-grammar
                                    :index mini-en-index})
        form (html-form question)]

    (log/info "generate-question: question: " (fo question))

    {:status 200
     :headers {"Content-Type" "application/json;charset=utf-8"
               
               "Cache-Control" "no-cache, no-store, must-revalidate"
               "Pragma" "no-cache"
               "Expires" "0"}
     :body (json/write-str
            {:left_context_of_question (:left_context_source form)
             :full_question (fo question)
             :question (:head_of_source form)
             :right_context_of_question (:right_context_source form)
             :semantics (strip-refs (get-meaning question))})}))

(defn generate-answers [request]
  "generate a single sentence according to the semantics of the request."
  (log/info (str "generate-answers: request params: " (get-in request [:params])))
  (log/info (str "generate-answers: request semantics: " (get-in request [:params :semantics])))
  (log/info (str "cloud_id: " (get-in request [:params :cloud_id])))
  (log/info (str "semantics:" (json/read-str (get-in request [:params :semantics]))))
  (let [semantics (json/read-str (get-in request [:params :semantics])
                                 :key-fn keyword
                                 :value-fn (fn [k v]
                                            (cond (string? v)
                                                  (keyword v)
                                                  :else v)))
        debug (log/info (str "semantic2: " semantics))

        more-constraints {:synsem {:subcat '()}
                          :head {:phrasal :top}
                          :comp {:phrasal false}}


        to-generate (merge semantics more-constraints)

        debug (log/info (str "to-generate: " to-generate))

        generated (it/generate
                   to-generate
                   {:grammar mini-it-grammar
                    :index mini-it-index})

        group_by (get-in generated [:synsem :sem :pred])
        debug (log/info (str "group_by: " group_by))
        ]
    
    {:status 200
     :headers {"Content-Type" "application/json;charset=utf-8"}
     :body
     (json/write-str
      {:cloud_id (get-in request [:params :cloud_id])
       :tree (morph/fo-ps (strip-refs generated))
       :group_by group_by
       :left_context_of_answer (morph/remove-parens (fo (get-in generated [:comp])))
       :answer (morph/remove-parens (fo (get-in generated [:head])))
       :full_answer (morph/remove-parens (fo generated))
       :semantics semantics
       :right_context_of_answer ""})}))

(def map_src_openstreetmaps "http://www.openstreetmap.org/export/embed.html?bbox=9%2C43.700395862593545%2C11.283645629882812%2C43.860524730744096&layer=mapnik")

(def map_src_google "https://maps.google.com/maps?z=7&ll=40.603723,14.381593&output=embed")

;(def map_src map_src_openstreetmaps)
(def map_src map_src_google)

(defn tour []
  (h/html5
   [:h3 {:style "background:lightgreen;padding:0.25em"} "Benvenuto a Italia!"]

   [:div#game

    [:svg {:id "svgarena"}]

    [:div#rainforest

     [:div#wordbar

      [:div#q1 "wordbar"]

      [:div#q2 "not used"]

      [:div#q3 "yet"]

      ]
     
     (direction-chooser)
     
     [:div#kilos
      "Kilometri:"
      [:span#scorevalue
       "0"
       ]
     ]

     [:div {:style "z-index:2"}
      [:iframe {:height "500px" :width "100%" :src map_src} ]
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


     ] ;; end of :div#world

    [:div#gameform


     ;; e.g., if the Italian is "io parlo", and the English is "I speak", and you are supposed to answer "parlo", then
     ;; the left_context_destination will be "io".
     [:span {:id "left_context_destination"}
      ]


      [:input {:id "game_input" :size "50"}]
     
      [:button#answer_button {:class "click;float:right;width:auto"
                             :onclick "submit_game_response('game_input'); event.preventDefault(); return false;"} "Answer" ]
     
      ] ;; end of :div #gameform
    ] ;; end of :div #game
) ; html5/div

) ;; end of (defn)

