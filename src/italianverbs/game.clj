(ns italianverbs.game
  (:require
   [clojure.data.json :as json]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [hiccup.page :refer :all]
   [italianverbs.generate :as gen]
   [italianverbs.grammar.english :as en]
   [italianverbs.grammar.italiano :as it]
   [hiccup.page :as h]
   [italianverbs.html :as html]
   [italianverbs.lexicon :as lex]
   [italianverbs.morphology :as morph]
   [italianverbs.unify :refer [strip-refs]]
   ))

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
       [:div#cd_lca {:class "correct_answer" } "" ]
       [:div#cd_rca {:class "correct_answer" } "" ]
       [:h3#correct_answer "" ]
       [:input {:id "correction_bare_id" :type "hidden"}]
       ]
      ;; end of :form
     
      ] ;; end of :div #correction_dialog

     [:div#ground
      
      [:i {:class "fa fa-tree x2" :style "left:1%; top:-10%" }]

      [:i {:class "fa fa-tree x1" :style "left:25%; top:-15%" }]

      [:i {:class "fa fa-tree x2" :style "left:50%; top:-5%" }]

      [:i {:class "fa fa-tree x3" :style "left:75%;top:-50%" }]


      ] ;; end of :div#ground

     ] ;; end of :div#rainforest

    [:div#gameform
      [:input {:id "game_input" :size "50"}]
     
      [:button#answer_button {:class "click"
                             :onclick "submit_game_response('game_input'); event.preventDefault(); return false;"} "Answer" ]
     
      [:button#correct_button {:style "display:none" :class "click"
                              :onclick "submit_correction_response('game_input'); event.preventDefault(); return false;"} "Correction" ]


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
    (log/info (str "head english: " @(get-in question [:head :english])))
    {;:left_context_english "John and I"
     :left_context_english (morph/remove-parens (morph/get-english (get-in question [:comp :english])))
     :middle_english (morph/remove-parens (morph/get-english (get-in question [:head :english])))
     :right_context_english ""
     :left_context_italian ".."
     :middle_italian "lavoriamo"
     :right_context_italian ""}))

(defn generate-question [request]
  (let [spec
        {:head {:phrasal false}
         :comp {:phrasal false}
         :synsem {:infl :present
                  :cat :verb
                  :subcat '()}}]
    {:status 200
     :headers {"Content-Type" "application/json;charset=utf-8"
               
               "Cache-Control" "no-cache, no-store, must-revalidate"
               "Pragma" "no-cache"
               "Expires" "0"}
     :body (let [question (gen/generate spec
                                        en/grammar
                                        lex/lexicon
                                        en/cache)
                 semantics (strip-refs (get-in question [:synsem :sem]))
                 english (morph/remove-parens (morph/get-english (:english question)))
                 form (html-form question)]
             (json/write-str
              {:english english
               :lcq (:left_context_english form)
               :semantics semantics
               :question (:middle_english form)
               :rcq (:right_context_english form)}))}))

(defn generate-answers [request]
  "generate a single sentence according to the request."
  (log/info (str "generate-answers: request params: " (get-in request [:params])))
  (log/info (str "generate-answers: request semantics: " (get-in request [:params :semantics])))
  (log/info (str "cloud_id: " (get-in request [:params :cloud_id])))
  (log/info (str "semantics:" (json/read-str (get-in request [:params :semantics]))))
  (let [semantics (json/read-str (get-in request [:params :semantics])
                                 :key-fn keyword)
        generated (gen/generate
                   {:synsem {:sem semantics}
                    :head {:phrasal false}
                    :comp {:phrasal false}}
                   it/grammar
                   lex/lexicon
                   it/cache)

        italian (morph/get-italian
                 (:italian generated))]
    {:status 200
     :headers {"Content-Type" "application/json;charset=utf-8"}
     :body
     (json/write-str
      {:cloud_id (get-in request [:params :cloud_id])
       :lca (morph/remove-parens (morph/get-italian (get-in generated [:comp :italian])))
       :answer (morph/remove-parens (morph/get-italian (get-in generated [:head :italian])))
       :semantics semantics
       :rca ""
       :italian italian})}))

