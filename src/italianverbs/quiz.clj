;; Seems like you need to restart ring to see changes to this file.
(ns italianverbs.quiz
    (:use 
     [hiccup core page-helpers]
     [somnium.congomongo])
    (:require [clojure.contrib.string :as stringc]
              [italianverbs.lexicon :as lexicon]
              [italianverbs.session :as session]
              [italianverbs.grammar :as gram]))

(def all-possible-question-types
  '(:mobili :mese :giorni :possessives :partitivo :ora :passato))

(defn wrapchoice [word & [ istrue ] ]
  ;; FIXME: url-encode word.
  (let [href_prefix "/quiz/?"
;        english word]
        english (get word :english)]
       (html [:div {:class "guess"}
	       [:h3 [:a {:href (str href_prefix "guess=" english)} english]
  	       (if istrue [:i.debug true])
	       ]])))

(defn show-choice [lexicon remaining answer show-true-before]
  (if (>= remaining 0)
      (str
       (if (= show-true-before remaining)
	   (wrapchoice answer true))
       (if (> remaining 0)
	   (let [choice (rand-int (count lexicon))]
		(str 
		 (html [:div.debug (str "remaining: " remaining)])
		 (wrapchoice (get lexicon (nth (keys lexicon) choice)))
		 (show-choice (dissoc lexicon (nth (keys lexicon) choice)) (- remaining 1)
			answer show-true-before)))))))

(defn get-next-question-id [request]
  "get the question id for the next question for this user."
  ;; TODO: improve: only return last question and get its _id.
  (let [session (session/request-to-session request)]
    (count (fetch :question :where {:session session}))))

(defn normalize-whitespace [string]
  (stringc/replace-re #"[ ]+$" "" (stringc/replace-re #"^[ ]+" "" (stringc/replace-re #"[ ]+" " " string))))

(defn store-question [question request]
  (insert! :question {:question (normalize-whitespace (get question :english))
                      :answer (normalize-whitespace (get question :italian))
                      :id (get-next-question-id request)
                      :session request}))

(defn clear-questions [session]
  (destroy! :question {:session session})
  session)

(defn set-filters [session request]
  (do
    (destroy! :filter {:session session})
    (insert! :filter {:form-params (get request :form-params)
                      :session session})
    session))


(defn each-correct [question]
  (if (= (get question :guess) (get question :answer)) '(true) nil))

(defn show-history-rows [qs count hide-answer]
  (if (first qs)
    (let
        [row (first qs)
         correctness (if (and (get row :guess) (not (= (get row :guess) "")))
                       (if (= (get row :answer) (get row :guess))
                         "correct"
                         "incorrect"))]
      (html
       [:tr 
        [:th count]
        [:td (get row :question)] 
        [:td {:class correctness} (get row :guess) ]
        [:td (if (= hide-answer false) (first (rest qs)) (get row :answer))]]
       (show-history-rows (rest qs) (- count 1) true)))))

(defn show-history []
  (let 
      [session nil ;; TODO : get session from request (as param to this fn).
       total (fetch-count :question)
       skip (if (> total 10) (- total 10) 0)
       qs (fetch :question {:session session} :sort {:id 1} :limit 10 :skip skip )]
      (html
       [:div#stats
         [:table
           [:tr [:th "Correct:"]
	        [:td (count (mapcat each-correct (fetch :question))) "/" total ]]
         ]
	 [:div {:id "controls"} {:class "controls quiz-elem"} [:a {:href "/quiz/clear/"} "Clear"]]
       ]      
       [:table
        [:tr
	  [:td]
	  [:th "question"]
          [:th "answer" ]
	  [:th "guess" ]
	]
       (show-history-rows qs (+ 1 skip) total)
       ]
       )))

(defn evaluate-guess [ guess ]
  ;; get last question.
  ;; FIXME: should filter by session!
  (let [question 
    (if (not (= (fetch :question :sort {:_id -1}) '()))
	(nth (fetch :question :sort {:_id -1} :limit 1) 0))]
	(if question
	    (update! :question question (merge question {:guess guess})))))

(defn store-guess [guess]
  "update question # question id with guess: a rewrite of (evaluate-guess)."
  (let [guess
        (normalize-whitespace guess)
        question 
        (if (not (= (fetch :question :sort {:_id -1}) '()))
          (nth (fetch :question :sort {:_id -1}) 0))]
    (update! :question question (merge question {:guess guess}))))

(defn generate [question-type]
  "maps a question-type to feature structure. right now a big 'switch(question-type)' statement (in C terms)."
  (cond
   (= question-type :passato)
   (gram/random-passato-prossimo)
   (= question-type :pp)
   (gram/pp
    {:$or [ {:italian "a"}, {:italian "di" }, {:italian "da"},
            {:italian "in" :english "in"}, {:italian "su"} ]}
    (gram/np-with-post-conditions
     {}
     gram/np-with-common-noun-and-definite-pronoun))
   (= question-type :partitivo)
   (gram/np {:number :plural
             :pronoun {:$ne true}}
            (gram/choose-lexeme {:def :part}))
   (= question-type :ora)
   (let [hour (rand-int 12)
         minute (* (rand-int 12) 5)
         ampm (if (= (rand-int 2) 0)
                "am"
                "pm")
         hour (if (= hour 0) 12 hour)]
    {:english (gram/english-time hour minute ampm)
     :italian (gram/italian-time hour minute ampm)})
   (= question-type :mobili)
   (let [fn gram/sv
         head
         (let [fn gram/vp-pp
               head (gram/choose-lexeme
                     {:cat :verb
                      :italian "essere"})
               comp
               (let [fn gram/pp
                     head (merge
                           {:already-looked-up true}
                           (gram/choose-lexeme
                            {:cat :prep
                             :furniture-prep true}))
                     comp (gram/np-with-post-conditions 
                            (get head :obj)
                            (defn fn [fs]
                              (= (get fs :def) "def")))]
                 (apply fn (list head comp)))]
           (apply fn (list head comp)))
         comp
         (gram/np-with-post-conditions 
           {:furniture true}
           (defn fn [fs]
             (= (get fs :def) "def")))]
     (merge {:question-type question-type}
            (apply fn (list head comp))))
   (= question-type :possessives)
   (let [fn gram/np-det-n-bar
         head
         (let [fn gram/n-bar
               head (gram/choose-lexeme
                     {:cat :noun
                      :common true
                      :number :singular})
               comp (gram/choose-lexeme
                     {:cat :adj
                      :gender (get head :gender)
                      :possessive true})]
           (merge {:test "possessive NPs"}
                  (apply fn (list head comp))))
         comp (gram/choose-lexeme
               {:cat :det
                :gender (get head :gender)
                :number (get head :number)
                :def :def})]
     (apply fn (list head comp)))
   (= question-type :mese)
   (gram/choose-lexeme {:month true})
   (= question-type :giorni)
   (gram/choose-lexeme {:giorni-della-settimana true})
   true
   (gram/sentence)))

(defn checked [session key]
  "return 'checked' if checkbox with key _key_ is set to true according to user's preferences."
  (let [record (fetch-one :filter :where {:session session})
        filters (if record
                  (get record :form-params))]
    (if (get filters key)
      {:checked "checked"}
      {})))

(defn checkbox-row [name key session & [label display checkbox-disabled]]
  (let [label (if label label name)]
    (html
     [:tr {:style (if display (str "display:" display))}
      [:th
       [:input (merge {:onclick "submit()" :name name :type "checkbox"}
                      (if (= checkbox-disabled "disabled") {:disabled "disabled"} {})
                      (checked session key))]]
      [:td label ] ] )))
  
(defn with-history-and-controls [session content]
  [:div
   content
    [:div {:class "history quiz-elem"}
     [:h2 "History"]
     [:table
      [:thead
       [:tr
        [:th]
        [:th "Q"]
        [:th "Guess"]
        [:th "A"]
        ]
       ]
      [:tbody
       (show-history-rows (fetch :question :where {:session session} :sort {:_id -1})
                          (count (fetch :question :where {:session session}))
                          false)
       ]
      ]]
    
    [:div {:id "controls" :class "controls quiz-elem"}
     [:h2 "Controls"]
     [:form {:method "post" :action "/quiz/filter" :accept-charset "iso-8859-1" }
      [:table
       (checkbox-row "ora" :ora session "Che ora è?")  ;; e.g. "5:30 => cinque ore e .."
       (checkbox-row "giorni" :giorni session "giorni della settimana")
       (checkbox-row "mobili" :mobili session)
       (checkbox-row "preposizioni" :preposizioni session "preposizioni" "none")
       (checkbox-row "partitivo" :partitivo session "articoli determinativi e partivi")
       (checkbox-row "mese" :mese session "le mese")
       (checkbox-row "numeri" :numeri session "numeri" "" "disabled") ;; e.g. "6.458 => sililaquattrocentocinquantotto"
       (checkbox-row "passato" :passato session "passato prossimo")  ;; e.g. "io ho fatto"
       (checkbox-row "possessives" :possessives session) ;; e.g. "il tuo cane"

       ]]
     
     [:div {:style "float:right"}
      [:form {:method "post" :action "/quiz/clear"}
       [:input.submit {:type "submit" :value "clear"}]]]]

   ;; at least for now, the following is used as empty anchor after settings are changed via controls and POSTed.
   [:div {:id "controlbottom" :style "display:none"} 
    " "
    ]
   
   ]
  
  )

(defn filter-by-criteria [list criteria]
  (if (> (count list) 0)
    (if (get criteria (first list))
      (cons (first list)
            (filter-by-criteria (rest list) criteria))
      (filter-by-criteria (rest list)
                           criteria))))

(defn possible-question-types [session]
  (let [possible-question-types all-possible-question-types
        record (fetch-one :filter :where {:session session})
        filters (if record
                  (get record :form-params))
        result (filter-by-criteria possible-question-types filters)]
    (if (> (count result) 0)
      result
      all-possible-question-types)))
        
    
(defn quiz [last-guess request]
  "choose a question type from the set of question types possible given the user's preference, and
   and then generate a question for that question type."
  (let [session (session/request-to-session request)
        ;; normalize guess: remove space from beginning and end, and lower-case.
        last-guess (stringc/trim (stringc/lower-case last-guess))
        get-next-question-id (get-next-question-id request)
        possible (possible-question-types (session/request-to-session request))
        next-question

        ;; TODO: next-question is two totally different things depending on the (if) - it's confusing.
        (if (or last-guess
                (= get-next-question-id 0))
          (generate (nth possible (rand-int (count possible))))
          (nth (fetch :question :where {:session session} :sort {:_id -1} :limit 1) 0))]

      (if last-guess (store-guess last-guess))

      (if (or last-guess
              (= get-next-question-id 0))
        (store-question next-question (session/request-to-session request)))

      (html
       (with-history-and-controls
         (session/request-to-session request)
         [:div {:class "quiz quiz-elem"}
          [:h2 (str "Domanda" " "
                    (if (or last-guess
                            (= get-next-question-id 0))
                      (+ 1 get-next-question-id)
                      get-next-question-id))]
          [:form {:name "quiz" :method "post" :action "/quiz/" :accept-charset "UTF-8"}
           [:table
            [:tr
             [:td [:h1 
                   (if (or last-guess
                           (= get-next-question-id 0))
                     (get next-question :english)
                     (str (get (nth (fetch :question :where {:session session} :sort {:_id -1} :limit 1) 0)
                          :question)))]]]
            [:tr
             [:td
              [:textarea {:cols "50" :rows "5" :name "guess" }]]]]
           [:div
            [:input.submit {:type "submit" :value "riposta"}]]]]))))

(defn url-decode [string]
  (.replaceAll string "(%20)" " "))

(defn get-params [pairs]
  (if (first pairs)
      (let [keyval (re-seq #"[^=]+" (first pairs))]
	   (merge 
	    {(first keyval) (url-decode (second keyval))}
	    (get-params (rest pairs))))
    {}))

(defn get-param-map [query-string]
  (if query-string
      (get-params (re-seq #"[^&]+" query-string))))

;; TODO : differentiate (run) and (display): see also TODO in core.clj.
(defn run [request]
  (let [query-string (get request :form-params)]
    (html
     ;; get 'guess' from query-string (e.g. from "guess=to%20eat")
     ;; pass the users's guess to (quiz), which will evaluate it.
     [:div (quiz (get query-string "guess") request)])))

(defn display [request]
  (let [query-string (get request :form-params)]
    (html
     ;; get 'guess' from query-string (e.g. from "guess=to%20eat")
     ;; pass the users's guess to (quiz), which will evaluate it.
     [:div (quiz nil request)])))

(defn filter [request]
  (let [query-string (get request :form-params)]
    (html
     ;; get 'guess' from query-string (e.g. from "guess=to%20eat")
     ;; pass the users's guess to (quiz), which will evaluate it.
     (with-history-and-controls
       (session/request-to-session request)
       [:div
        "stuff done got filtered."]))))


