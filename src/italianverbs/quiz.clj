;; Seems like you need to restart ring to see changes to this file.
;; TODO: verify using tests that a user authenticated with session 'x' cannot modify a question
;; whose session is 'y' where 'x' != 'y'.
;; (see update-question-by-id-with-guess) where this is enforced by the mongo/fetch's :where clause.
(ns italianverbs.quiz
    (:use [hiccup core page-helpers])
    (:require [clojure.contrib.string :as stringc]
              [somnium.congomongo :as mongo]
              [italianverbs.lexicon :as lexicon]
              [italianverbs.lev :as lev]
              [italianverbs.session :as session]
              [italianverbs.grammar :as gram]
              [base.html :as basehtml]
              [italianverbs.html :as html]
              [italianverbs.xml :as xml]
              [italianverbs.generate :as gen]
              [ring.util.codec :as url]
              [clojure.contrib.str-utils2 :as str-utils]))

;; to add a new question type:
;; 1. write a function (gen/mytype) that generates a random question for mytype.
;; 2. register this function in the (generate) method below.
;; 3. register :mytype with the (controls) method below.
;; 4. register :mytype with the question-type-map below.
;; 5. add :mytype to all-possible-question-types (immediately below).
;; TODO: make 1-5 a macro.
(def all-possible-question-types
  '(:mobili :mese :giorni :possessives :partitivo :ora :infinitivo :passato :futuro :presente :espressioni :oct2011 :chetempo :cucina))

(def question-type-map
  {"mobili" {:sym :mobili :desc "furniture sentences"},
   "mese" {:sym :mese :desc "months of the year"},
   "giorni" {:sym :giorni, :desc "days of the week"},
   "possessives" {:sym :possessives, :desc "possessive pronouns"},
   "partitivo" {:sym :partitivo, :desc "partitive pronouns (e.g. 'degli uomini')"},
   "ora" {:sym :ora, :desc "clock times"},
   "infinitivo" {:sym :infinitivo, :desc "infinitive verbs"},
   "passato" {:sym :passato, :desc "passato prossimo verbs"},
   "futuro" {:sym :futuro, :desc "futuro semplice verbs"},
   "presente" {:sym :presente, :desc "present tense verbs"},
   "espressioni" {:sym :espressioni, :desc "useful expressions"},
   "oct2011" {:sym :oct2011, :desc "recently-encountered vocabulary"},
   "chetempo" {:sym :chetempo, :desc "weather-related terms"}
   "cucina" {:sym :cucina, :desc "kitchen and food-related vocabulary"}})

(defn question-type [params]
  (let [type-symbol (get params "type")]
    (if type-symbol
      (if (get question-type-map type-symbol)
        (get (get question-type-map type-symbol) :sym)))))

(defn types []
  "display all possible question types."
  (str
   (xml/encoding)
   (html ;; not html, but using this function to generate xml.
    [:types
     (map (fn [key]
            [:type {:key key} (str
                  (get (get question-type-map key) :desc))])
          (keys question-type-map))])))

(defn per-user-correct [questions]
  "count of all correctly-answered questions for all session."
  (reduce
   (fn [x y]
     (+ x y))
   (map (fn [x] (if (= (get x :answer) (get x :guess)) 1 0))
        questions)))

(defn per-user-incorrect [questions]
  "count of all incorrectly-answered questions for all session."
  (reduce
   (fn [x y]
     (+ x y))
   (map (fn [x] (if (not (= (get x :answer) (get x :guess)))
                  (if (= nil (get x :guess))
                    0 1)
                  0))
        questions)))

(defn per-user-total [questions]
  "count of all incorrectly-answered questions for all session."
  (count questions))

(defn wrapchoice [word & [ istrue ] ]
  ;; TODO: url-encode word.
  (let [href_prefix "/italian/quiz/?"
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
    (count (mongo/fetch :question :where {:session session}))))

(defn normalize-whitespace [string]
  (stringc/replace-re #"[ ]+$" "" (stringc/replace-re #"^[ ]+" "" (stringc/replace-re #"[ ]+" " " string))))

(defn store-question [question request last-guess]
  (mongo/insert! :question {:question (normalize-whitespace (get question :english))
                            :answer (normalize-whitespace (get question :italian))
                            :id (get-next-question-id request)
                            :cat (get question :cat)
                            :guess last-guess
                            :gender (get question :gender)
                            :italian (get question :italian)
                            :english (get question :english)
                            :session request}))

(defn clear-questions [session]
  (mongo/destroy! :question {:session session})
  session)

(defn set-filters [session request]
  (do
    (mongo/destroy! :filter {:session session})
    (mongo/insert! :filter {:form-params (get request :form-params)
                      :session session})
    session))


(defn each-correct [question]
  (if (= (get question :guess) (get question :answer)) '(true) nil))

(defn eval-segments [segments]
  (if segments
    {:size (.size segments)
     :match (.size (remove (fn [segment]
                             (let [edit-action (get segment :action)]
                               (not (= edit-action "match"))))
                           segments))}
    {:size 0
     :match nil}))

;; TODO: use looping per:
;; http://clojure.org/functional_programming#Functional%20Programming--Recursive%20Looping
(defn format-evaluation [green2 index]
  (if green2
    (let [char (if (= (get (first green2) :action) "delete")
                 (get (first green2) :truth)
                 (get (first green2) :test))
          style (get (first green2) :action)]
      (if (> (.size green2) 0)
        (str
         "<span class='" style "'>" char "</span>"
         (if (= style "subst")
           (str "<span class='delete'>" (get (first green2) :truth) "</span>"))
         (format-evaluation
          (rest green2) (+ index 1)))))))

(defn show-history-row [row count hide-answer total next-q]
  (let [distance-from-top
        (str "dist"
             (if (< (- total count) 5)
               (if (= (- total count) 0)
                 (str (- total count) " debug")
                 (- total count))
               "n"))]
    (html
     [:tr
      {:class (str distance-from-top
                   (if (= (mod count 2) 1)
                     " odd"))}
      [:td {:rowspan "2"} (get row :question)] ]
     [:tr
      {:class (str distance-from-top
                   (if (= (mod count 2) 1)
                     " odd"))}
      
      [:td {:class "eval"}
       (if (get row :evaluation)
         (str (format-evaluation
               (get row :evaluation) 0))
         (str "" (get row :guess)))]]
     (if (not (= (get row :answer)
                 (get row :guess)))
       
       [:tr
        {:class (str distance-from-top
                     (if (= (mod count 2) 1)
                       " odd"))}
        [:td "" ]
        [:td {:class "answer"} (if (= hide-answer false) next-q (get row :answer))]
        ]))))

;; TODO: use looping per:
;; http://clojure.org/functional_programming#Functional%20Programming--Recursive%20Looping
(defn show-history-rows [qs count hide-answer total]
  (if (first qs)
    (let
        [row (first qs)
         distance-from-top
         (str "dist"
              (if (< (- total count) 5)
                (if (= (- total count) 0)
                  (str (- total count) " debug")
                  (- total count))
                "n"))]
      (html
       [:tr
        {:class (str distance-from-top
                     (if (= (mod count 2) 1)
                       " odd"))}
        [:td {:rowspan "2"} (get row :question)] ]

       [:tr
        {:class (str distance-from-top
                     (if (= (mod count 2) 1)
                       " odd"))}

        [:td {:class "eval"}
         (if (get row :evaluation)
           (str (format-evaluation
                     (get row :evaluation) 0))
           (str "" (get row :guess)))]


        ]
       (if (not (= (get row :answer)
                   (get row :guess)))
                 
         [:tr
          {:class (str distance-from-top
                       (if (= (mod count 2) 1)
                         " odd"))}
          [:td "" ]
          [:td {:class "answer"} (if (= hide-answer false) (first (rest qs)) (get row :answer))]
          ])
       
       (show-history-rows (rest qs) (- count 1) true total)))))

(defn most-recent-qid-for-user [session]
  (let [results (mongo/fetch :question :where {:session session} :sort {:_id -1} :limit 1)]
    (if (> (.size results) 0)
      (get (nth results 0) :_id))))

;; TODO: enforce session :where check.
(defn update-question-by-id-with-guess [guess qid session]
  (let [guess (normalize-whitespace guess)
        question (mongo/fetch-one :question
                                  :where {:_id (new org.bson.types.ObjectId qid)
                                          :session session})
        updated-question-map
        (merge
         question
         {:guess guess
          :evaluation ;; evaluate the user's guess against the correct response.
          (if (and guess
                   (> (.length guess) 0))
            (lev/get-green2 (get question :answer)
                            guess))})]
    (mongo/update! :question {:_id (new org.bson.types.ObjectId qid)}
                   updated-question-map)
    updated-question-map))
;; for testing/sanity checking, might want to refetch (i.e. uncomment the line below and comment out line above).
;;    (mongo/fetch-one :question :where {:_id (new org.bson.types.ObjectId qid)})))

;;  "update question with guess - a rewrite of (format-evaluation)."
(defn update-question-with-guess [guess question]
  (let [guess
        (normalize-whitespace guess)]
    (mongo/update! :question question
                   (merge {:guess guess
                           :evaluation ;; evaluate the user's guess against the correct response.
                           (if (and guess
                                    (> (.length guess) 0))
                             (lev/get-green2 (get question :answer)
                                             guess))}
                          question))))

(defn oct2011 []
  (gram/choose-lexeme {:oct2011 true}))

(defn che-tempo []
  (gram/choose-lexeme {:chetempo true}))

(defn cucina []
  (gram/np {:cucina true}
           {:def :def}))

(defn generate [question-type]
  "maps a question-type to feature structure. right now a big 'switch(question-type)' statement (in C terms)."
  (cond
   (= question-type :oct2011)
   (oct2011)
   (= question-type :cucina)
   (cucina)
   (= question-type :chetempo)
   (che-tempo)
   (= question-type :espressioni)
   (gen/espressioni)
   (= question-type :infinitivo)
   (gen/random-infinitivo)
   (= question-type :passato)
   (gen/random-passato-prossimo)
   (= question-type :futuro)
   (gen/random-futuro-semplice)
   (= question-type :presente)
   (gen/random-present)
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
   (gen/mobili)
   (= question-type :possessives)
   ;; ----det [ num [1] gen [2] ]
   ;; \
   ;;  \---n'---- poss-adj [ num [1] gen [2] ]
   ;;       \
   ;;        \--- n [ num [1] gen [2] ]
   (let [fn gram/np-det-n-bar
         head
         (let [fn gram/n-bar
               head (gram/choose-lexeme
                     {:cat :noun
                      :common true})
               comp (gram/choose-lexeme
                     {:cat :adj
                      :gender (get head :gender)
                      :number (get head :number)
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

(defn- controls [session & [ form-action onclick ] ]
  (let [action (if form-action form-action "/italian/quiz/filter")
        onclick (if onclick onclick "submit()")
        record (mongo/fetch-one :filter :where {:session session})
        filters (if record
                  (get record :form-params))
        checked (fn [session key]
                  "return 'checked' if checkbox with key _key_ is set to true according to user's preferences."
                  (if (get filters key)
                    {:checked "checked"}
                    {}))
        checkbox-col (fn checkbox-col [name key session & [label display checkbox-disabled]]
                       (let [label (if label label name)]
                         (html
                          [:th
                           [:input (merge {:onclick onclick :name name :type "checkbox"}
                                          (if (= checkbox-disabled "disabled") {:disabled "disabled"} {})
                                          (checked session key))]]
                          [:td label ])))]
    [:div {:id "controls" :class "controls quiz-elem"}
     [:h2 "i tuoi preferiti" [:span#quizbanner  [:script "show_question_types()"]  ]  ]
     ;; TODO: don't create a form if doing ajax (instead will use onclick(), where onclick() is
     ;; potentially generated by javascript.
     [:form#controls_form {:method "post" :action action :accept-charset "utf-8" }
      [:table
       [:tr
        (checkbox-col "ora" :ora session "Che ora è?")  ;; e.g. "5:30 => cinque ore e .."
        (checkbox-col "giorni" :giorni session "giorni della settimana")
        (checkbox-col "mobili" :mobili session)
        ]
       [:tr
;        (checkbox-col "preposizioni" :preposizioni session "preposizioni" "none")
        (checkbox-col "partitivo" :partitivo session "articoli determinativi e partivi")
        (checkbox-col "mese" :mese session "le mese")
        ]
       [:tr
;        (checkbox-col "numeri" :numeri session "numeri" "" "disabled") ;; e.g. "6.458 => sililaquattrocentocinquantotto"
        (checkbox-col "possessives" :possessives session) ;; e.g. "il tuo cane"
        (checkbox-col "espressioni" :espressioni session "espressioni utili") ;; e.g. "il tuo cane"
        ]
       [:tr
        (checkbox-col "oct2011" :oct2011 session "oct2011")
        (checkbox-col "chetempo" :chetempo session "che tempo è?")
        (checkbox-col "cucina" :cucina session "cucina")
        ]
       ]
      [:div {:class "optiongroup"}
       [:h4 "Verbi"]
       [:table
        [:tr
         (checkbox-col "passato" :passato session "passato prossimo")  ;; e.g. "io ho fatto"
         (checkbox-col "futuro" :futuro session "futuro semplice")  ;; e.g. "tornerai"
         (checkbox-col "presente" :presente session "presente indicativo" "")  ;; e.g. "io vado"
         (checkbox-col "infinitivo" :infinitivo session "infinitivo")  ;; e.g. "fare"
         ]
        ]
       ]
      ]
     
     ;; at least for now, the following is used as empty anchor after settings are changed via controls and POSTed.
     [:div {:id "controlbottom" :style "display:none"} 
      " "
      ]
   ]))

;; TODO: use recur:
;; see http://clojure.org/functional_programming#Functional Programming--Recursive Looping
(defn filter-by-criteria [list criteria]
  (if (> (count list) 0)
    (if (get criteria (first list))
      (cons (first list)
            (filter-by-criteria (rest list) criteria))
      (filter-by-criteria (rest list)
                           criteria))))

(defn possible-question-types [session]
  (let [possible-question-types all-possible-question-types
        record (mongo/fetch-one :filter :where {:session session})
        filters (if record
                  (get record :form-params))
        result (filter-by-criteria possible-question-types filters)]
    (if (> (count result) 0)
      result
      all-possible-question-types)))

(defn random-guess-type [session]
  (let [possible (possible-question-types session)]
    (nth possible (rand-int (count possible)))))

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

(defn previous-question [session]
  (let [most-recent-set
        (mongo/fetch :question :where {:session session} :sort {:_id -1} :limit 2)]
    ;; must be at least 2 results: if not, return nil.
    (if (> (.size most-recent-set) 1)
      (nth most-recent-set 1)
      nil)))

(defn table-row-debug-info [answered-question-tuple]
  (str
   "<tr>"
   "<td>id=" (get answered-question-tuple :_id) "</td>"
   "</tr>"))

(defn table-row [answered-question-tuple]
  (let [english (get answered-question-tuple :english)
        italian (get answered-question-tuple :italian)
        guess (get answered-question-tuple :guess)
        evaluation (get answered-question-tuple :evaluation)
        row_id (get answered-question-tuple :_id)
        eval (eval-segments evaluation)
        ;; translation of evaluation into feedback to user.
        perfect (= (get eval :size) (get eval :match))
        rowspan (if perfect 1 2)
        formatted-evaluation (format-evaluation evaluation 0)]
    ;; TODO: move HTML generation to javascript: just create a javascript call with the params:
    ;; {english,italian,formatted evaluation (itself html), debug info}.
    (let [js (str "table_row('" row_id"', " perfect ")")]
      (html
       [:tbody {:style "display:block"}
        [:tr
         [:td
          [:div {:style "display:none"}
           [:div {:id (str row_id "_en")} english ]
           [:div {:id (str row_id "_it")} italian ]
           [:div {:id (str "tr_" row_id "_js_eval")} formatted-evaluation ]
           ]
          ]
         ]
        ]
       [:tbody
        [:script js]]))))

(defn question [request]
  ;; create a new question, store in backing store, and return an HTML fragment with the question's english form
  ;; and the question id to pose question to user.
  ;; TODO: move HTML (presentation layer) to javascript: simply return javascript call to "generate_question()" with
  ;; 2 params: question text and question id.
  ;; note that request has the following keys:
  ;; :remote-addr :scheme :query-params :session :form-params :multipart-params :request-method :query-string
  ;; :route-params :content-type :cookies :uri :server-name :params :headers :content-length :server-port
  ;; :character-encoding :body
  (let [type (random-guess-type (session/request-to-session request))
        question (store-question (generate type)
                                 (session/request-to-session request) nil)]
    (str "<div id='question_text'>" (get question :question) "</div>"
         "<input id='question_id' value='" (get question :_id) "'/>")))

(defn evaluate [request format]
  (let [params (if (= (get request :request-method) :get)
                 (get request :query-params)
                 (get request :form-params))
        session (session/request-to-session request)
        guess (get params "guess")
        qid (get params "qid")
        format (if format format (get params "format"))
        content (merge
                 {:method (get request :request-method)}
                 params
                 {:session session})]
    ;; make sure qid is defined.
    (if (= nil qid) (/ 1 0)) ;; figure out how to throw exceptions in clojure.
    (if (= nil session) (/ 1 0)) ;; figure out how to throw exceptions in clojure.
    (if guess
      (let [result (update-question-by-id-with-guess guess qid session)]
         (= format "xml")
         (str
          (xml/encoding)
          (str "<container>"
               "<guess>" guess "</guess>"
               "<correctanswer>" (get result :italian) "</correctanswer>"
               "<evaluation>" (get result :evaluation) "</evaluation>"
               "</container>"))
         (= format "xmltr")
         (str
          (xml/encoding)
          "<xmltr>"
          (table-row result)
          "</xmltr>")
         true
         (= format "tr") ; default: "tr".
         (table-row result))
      (str "<error>no guess" guess "</error>"))))

(defn guess [question request format]
  (let [params (if (= (get request :request-method) :get)
                 (get request :query-params)
                 (get request :form-params))
        session (session/request-to-session request)
        stored (if (get params "id")
                 (mongo/fetch-one :question
                                  :where {:_id (new org.bson.types.ObjectId (get params "id"))
                                          :session session})
                 (store-question question (session/request-to-session request) nil))]
    (str
     (xml/encoding)
     (html
      [:container {:id (get stored :_id) :type (question-type params)}
       [:english (get stored :english)]
       [:italian (get stored :italian)]]))))

(defn preferiti [request]
  (basehtml/page "i tuoi preferiti"
                 (html
                  [:div#controls_container "if you can see this, either javascript is not enabled or your browser could not contact the server to show the quiz controls."] )
                 request
                 "show_quiz_preferences()"))

(defn quiz [request]
  (basehtml/page "Quiz"
   (html
     [:div {:class "quiz-elem"}
      [:h2 "Quiz" [:span#quizbanner [:script "show_question_types()" ]]]
      [:div#quiz_container

       [:table {:class "question"} " "
        [:tr
         [:td
          [:div#ajax_question " "
           [:script "get_next_question()" ]
           ]
          ]
         [:td
          [:input {:size "15" :id "guess_input" :type "text"}
           [:script "clear_guess_input()" ]
           ]
          ]

         [:td
          [:button {:class "click" :onclick "submit_user_response('guess_input')"} "Rispondi" ]
          ]
         ]
        ]

       [:table {:id "quiz_table" :class "quiz"} " " ]
       [:div {:style "display:none" :id "stripe_toggle"} "odd" ] ]]
     )

   request
   "ajax_quiz('quiz_container')"))

;; TODO: more usage of fake session below for more coverage of quiz stateful behavior.
(defn test []
  (let [session "e0933a66-2b37-4bc7-b4c6-400ff2e81d9a"]
    (list
     {:comment "all possible question types."
      :test all-possible-question-types}

     {:comment "quiz inside a iframe."
      :test (html/iframe "/italian/quiz/?guess=foo")}

     {:comment "fs printing"
      :test (html/fs
             {:most-recent
              (let [qs (mongo/fetch :question :where {:session session} :sort {:_id -1} :limit 1)]
                (if (> (.size qs) 0)
                  (nth qs 0)
                  "(no questions yet.)"))})})))

(defn- show-filters [session]
  (let [record (mongo/fetch-one :filter :where {:session session})
        filters (if record (get record :form-params))]
    (stringc/join " "
                  (map (fn [key]
                         (if (get filters (keyword key))
                           (html
                            [:span {:class "qtype"} key ]
                            ;; allow for turning off filters from here (eventually).
                            ;; [:span {:class "qtype remove"} "x" ]
                            )))
                       (keys question-type-map)))))

(defn show-controls [session params action header]
  "format param set in quiz.js."
  (cond (= (get params "format") "titlebar")
        (show-filters session)
        true
        (html
         (controls session nil "submit_quiz_filters('#controls_container','#controls_form');"))))
