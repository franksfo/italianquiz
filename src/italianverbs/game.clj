(ns italianverbs.game
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [hiccup.page :refer :all]
   [italianverbs.generate :as gen]
   [italianverbs.grammar.english :as en]
   [italianverbs.grammar.italiano :as it]
   [italianverbs.html :as html]
   [italianverbs.lexicon :as lex]
   [italianverbs.morphology :as morph]
   [italianverbs.unify :refer [strip-refs]]
   ))

(defn game []
  (html5
   [:div#game

    [:svg {:id "svgarena"}]


    [:div#rainforest

     [:div#wordbar

      [:div#q1 "wordbar"]

      [:div#q2 "not used"]

      [:div#q3 "yet"]

      ]

     
     [:div#sky ]

     [:div#ground

      [:i {:class "fa fa-tree x2" :style "left:1%; top:-10%" }]

      [:i {:class "fa fa-tree x1" :style "left:25%; top:5%" }]

      [:i {:class "fa fa-tree x2" :style "left:50%; top:-35%" }]

      [:i {:class "fa fa-tree x3" :style "left:75%;top:-50%" }]


      ] ;; end of :div#ground

     ] ;; end of :div#rainforest

    [:div#gameform
      [:input {:id "game_input" :size "50"}]

      [:button {:class "click"
                :onclick "submit_game_response('game_input');"} "Answer" ]
      
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

(defn generate-question [request]
  {:status 200
   :headers {"Content-Type" "application/json;charset=utf-8"}
   :body (let [question (gen/generate {:head {:phrasal false}
                                      :comp {:phrasal false}
                                      :synsem {:infl :present
                                               :cat :verb
                                               :subcat '()}}
                                     en/grammar
                                     lex/lexicon
                                     en/cache)
               semantics (strip-refs (get-in question [:synsem :sem]))
               english (morph/remove-parens (morph/get-english (:english question)))]
           (str "{\"english\": \"" english "\",
 \"semantics\": \"" semantics "\"}"))})

(defn generate-answers [request]
  (log/info (str "generate-answers: request params: " (get-in request [:params])))
  (log/info (str "generate-answers: request semantics: " (get-in request [:params :semantics])))
  (log/info (str "cloud_id: " (get-in request [:params :cloud_id])))
  (let [semantics (read-string (get-in request [:params :semantics]))
        italian (map-realize #(str "\"" (morph/get-italian
                                         (:italian %)) "\"")
                             (gen/generate-all
                              {:synsem {:sem semantics}
                               :head {:phrasal false}
                               :comp {:phrasal false}}
                              it/grammar
                              lex/lexicon
                              it/cache))]
    (str "{\"cloud_id\": \"" (get-in request [:params :cloud_id]) "\","
          "\"italian\": [" (string/join "," italian) "], "
          "\"semantics\": \"" semantics "\"}")))


