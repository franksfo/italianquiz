(ns italianverbs.game
  (:refer-clojure :exclude [get-in])
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
   [italianverbs.unify :refer [get-in strip-refs]]
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
    {:left_context_english (morph/remove-parens (morph/get-english (get-in question [:comp :english])))
     :middle_english (morph/remove-parens (morph/get-english (get-in question [:head :english])))
     :right_context_english ""
     :middle_italian "lavoriamo"
     :right_context_italian ""}))

(def mini-english-grammar
  (filter #(or (= (:rule %) "s-present")
               (= (:rule %) "s-future")
;               (= (:rule %) "s-aux")
;               (= (:rule %) "vp-aux")
               (= (:rule %) "s-imperfetto")
               (= (:rule %) "s-conditional"))
          en/grammar))

(def mini-italian-grammar
  (filter #(or (= (:rule %) "s-present")
               (= (:rule %) "s-future")
;               (= (:rule %) "s-aux")
;               (= (:rule %) "vp-aux")
               (= (:rule %) "s-imperfetto")
               (= (:rule %) "s-conditional"))
          it/grammar))

(defn generate-question [request]
  (let [spec
        {:head {:phrasal false}
         :comp {:phrasal false}
         :synsem {;:sem {:pred :leggere}
                  :cat :verb
                  :subcat '()}}]
    {:status 200
     :headers {"Content-Type" "application/json;charset=utf-8"
               
               "Cache-Control" "no-cache, no-store, must-revalidate"
               "Pragma" "no-cache"
               "Expires" "0"}
     :body (let [question (gen/generate spec
                                        mini-english-grammar
                                        lex/lexicon
                                        en/cache)
                 semantics (strip-refs (get-in question [:synsem :sem]))
                 english (morph/remove-parens (morph/get-english (:english question)))
                 form (html-form question)]
             (json/write-str
              {:english english
               :left_context_of_question (:left_context_english form)
               :semantics semantics
               :question (:middle_english form)
               :right_context_of_question (:right_context_english form)}))}))

(defn generate-answers [request]
  "generate a single sentence according to the semantics of the request."
  (log/info (str "generate-answers: request params: " (get-in request [:params])))
  (log/info (str "generate-answers: request semantics: " (get-in request [:params :semantics])))
  (log/info (str "cloud_id: " (get-in request [:params :cloud_id])))
  (log/info (str "semantics:" (json/read-str (get-in request [:params :semantics]))))
  (let [semantics (json/read-str (get-in request [:params :semantics])
                                 :key-fn keyword)
        generated (it/generate
                   {:synsem {:sem semantics}
                    :head {:phrasal false}
                    :comp {:phrasal false}}
                   mini-italian-grammar)

        italian (morph/get-italian
                 (:italian generated))]
    {:status 200
     :headers {"Content-Type" "application/json;charset=utf-8"}
     :body
     (json/write-str
      {:cloud_id (get-in request [:params :cloud_id])
       :group_by (str (get-in generated [:head :italian :infinitive]))
       :left_context_of_answer (morph/remove-parens (morph/get-italian (get-in generated [:comp :italian])))
       :answer (morph/remove-parens (morph/get-italian (get-in generated [:head :italian])))
       :semantics semantics
       :right_context_of_answer ""
       :italian italian})}))

