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
     ;; the left_context_italian will be "io".
     [:span {:id "left_context_italian"}
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
    (log/info (str "question: " (morph/fo question)))
    (log/info (str "head english: " (get-in question [:head :english])))
    {:left_context_english (morph/remove-parens (en/get-string (get-in question [:comp :english])))
     :head_of_english (morph/remove-parens (en/get-string (get-in question [:head :english])))
     :right_context_english ""
     :right_context_italian ""}))

(def mini-en-grammar
  (filter #(= (:rule %) "s-present")
          en/grammar))

(def mini-en-index (create-index mini-en-grammar (flatten (vals en/lexicon)) head-principle))

(def mini-it-grammar
  (filter #(= (:rule %) "s-present")
          it/grammar))

(def mini-it-index (create-index mini-it-grammar (flatten (vals it/lexicon)) head-principle))

(defn generate-question [request]
  (let [spec
        {:head {:phrasal :top}
         :comp {:phrasal false}
         :synsem {:sem {:pred :leggere}
                  :cat :verb
                  :subcat '()}}
        question (en/generate spec {:grammar mini-en-grammar
                                    :index mini-en-index})
        form (html-form question)]

    (log/info "generate-question: english: " (fo question))

    {:status 200
     :headers {"Content-Type" "application/json;charset=utf-8"
               
               "Cache-Control" "no-cache, no-store, must-revalidate"
               "Pragma" "no-cache"
               "Expires" "0"}
     :body (json/write-str
            {:left_context_of_question (:left_context_english form)
             :question (:head_of_english form)
             :right_context_of_question (:right_context_english form)
             :english (fo question)
             :semantics (strip-refs (get-meaning question))})}))

(defn genlab [request]
  (do
    (log/info "rendering genlab page..")
    {:status 200
     :headers {"Content-Type" "text/html;charset=utf-8"}
     :body (html/page "Rain Forest: Lab" 
                      (h/html5
                       [:h1 "Welcome to the lab.."]
                       [:div#labform
                        [:h2 "input" ]
                        [:textarea ]
                        [:h3 "output "]
                        [:div#output ]]))}))

;; a reference for how to use the generate API.
(defn working [ & [spec]]
  (let [spec (if spec spec :top)]
    (it/sentence spec)))

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

        debug (log/info (str "generated:" generated))

        italian (fo generated)

        debug (log/info (str "italian:" italian))

]

    {:status 200
     :headers {"Content-Type" "application/json;charset=utf-8"}
     :body
     (json/write-str
      {:cloud_id (get-in request [:params :cloud_id])
       :tree (morph/fo-ps (strip-refs generated))
       :group_by (if (= (get-in generated [:head :synsem :aux]))
                   (get-in (strip-refs generated) [:head :comp :italian :infinitive])
                   (get-in generated [:head :italian :infinitive]))
       :left_context_of_answer (morph/remove-parens (fo (get-in generated [:comp])))
       :answer (morph/remove-parens (fo (get-in generated [:head])))
       :semantics semantics
       :right_context_of_answer ""
       :italian italian})}))

