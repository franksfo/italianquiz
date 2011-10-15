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
              [italianverbs.generate :as gen]))

;; to add a new question type:
;; 1. write a function (gen/mytype) that generates a random question for mytype.
;; 2. register this function in the (generate) method below.
;; 3. register :mytype with (controls) method below.
;; 4. add :mytype to all-possible-question-types (immediately below).
;; TODO: make 1-4 a macro.
(def all-possible-question-types
  '(:mobili :mese :giorni :possessives :partitivo :ora :infinitivo :passato :futuro :presente :espressioni :oct2011 :chetempo))

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

(defn generate [question-type]
  "maps a question-type to feature structure. right now a big 'switch(question-type)' statement (in C terms)."
  (cond
   (= question-type :oct2011)
   (oct2011)
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

(defn controls [session & [ form-action onclick ] ]
  (let [action (if form-action form-action "/italian/quiz/filter")
        onclick (if onclick onclick "submit()")
        checked (fn [session key]
                  "return 'checked' if checkbox with key _key_ is set to true according to user's preferences."
                  (let [record (mongo/fetch-one :filter :where {:session session})
                        filters (if record
                                  (get record :form-params))]
                    (if (get filters key)
                      {:checked "checked"}
                      {})))
        checkbox-row (fn checkbox-row [name key session & [label display checkbox-disabled]]
                       (let [label (if label label name)]
                         (html
                          [:tr {:style (if display (str "display:" display))}
                           [:th
                            [:input (merge {:onclick onclick :name name :type "checkbox"}
                                           (if (= checkbox-disabled "disabled") {:disabled "disabled"} {})
                                           (checked session key))]]
                           [:td label ] ] )))
        checkbox-col (fn checkbox-col [name key session & [label display checkbox-disabled]]
                       (let [label (if label label name)]
                         (html
                          [:th
                           ;; TODO: onclick should be parameterizable for use by AJAX.
                           [:input (merge {:onclick onclick :name name :type "checkbox"}
                                          (if (= checkbox-disabled "disabled") {:disabled "disabled"} {})
                                          (checked session key))]]
                          [:td label ] )))]
    [:div {:id "controls" :class "controls quiz-elem"}
     [:h2 "I controlli"]
     ;; TODO: don't create a form if doing ajax (instead will use onclick(), where onclick() is
     ;; potentially generated by javascript.
     [:form#controls_form {:method "post" :action action :accept-charset "iso-8859-1" }
      [:table
       [:tr
        (checkbox-col "ora" :ora session "Che ora è?")  ;; e.g. "5:30 => cinque ore e .."
        (checkbox-col "giorni" :giorni session "giorni della settimana")
        (checkbox-col "mobili" :mobili session)
        ]
       [:tr
        (checkbox-col "preposizioni" :preposizioni session "preposizioni" "none")
        (checkbox-col "partitivo" :partitivo session "articoli determinativi e partivi")
        (checkbox-col "mese" :mese session "le mese")
        ]
       [:tr
        (checkbox-col "numeri" :numeri session "numeri" "" "disabled") ;; e.g. "6.458 => sililaquattrocentocinquantotto"
        (checkbox-col "possessivi" :possessives session) ;; e.g. "il tuo cane"
        (checkbox-col "espressioni" :espressioni session "espressioni utili") ;; e.g. "il tuo cane"
        ]

       [:tr
        (checkbox-col "oct2011" :oct2011 session "oct2011")
        (checkbox-col "chetempo" :chetempo session "che tempo è?")
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

(defn with-history-and-controls [session content]
  (let [questions (mongo/fetch :question :where {:session session})]
    [:div
     [:div {:style "float:left;width:45%;margin-right:1em"}
      content
      (controls session)
      ]

     [:div {:class "history quiz-elem"}
      [:h2 "storia"]
      
      
      [:div {:style "float:right"}
       [:form {:method "post" :action "/italian/quiz/clear"}
        [:input.submit {:type "submit" :value "clear"}]]]
      
      [:table
       {:class "history"}
       [:thead
        ]
       [:tbody
        (let [fetch (mongo/fetch :question :where {:session session} :sort {:_id -1})
              count (count fetch)]
          (show-history-rows fetch
                             count
                             false
                             count))
        ]
       ]]
     
     [:div {:class "quiz-elem stats"}
      [:h2 "Stats"]
      [:table
       [:tr
        [:th "Correct"]
        [:td (per-user-correct questions)]
        ]
       
       [:tr
        [:th "Incorrect"]
        [:td (per-user-incorrect questions)]
        ]
       
       [:tr
        [:th "Total"]
        [:td (per-user-total questions)]
        ]
       
       ]
      ]
     
     ]
    ))

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

(defn quiz [last-guess request]
  "choose a question type from the set of question types possible given the user's preference, and
   and then generate a question for that question type."
  (let [session (session/request-to-session request)
        ;; normalize guess: remove space from beginning and end, and lower-case.
        last-guess (if last-guess (stringc/trim (stringc/lower-case last-guess)))
        get-next-question-id (get-next-question-id request)
        possible (possible-question-types (session/request-to-session request))
        next-question
        ;; TODO: next-question is two totally different things depending on the (if) - it's confusing.
        (if (or last-guess
                (= get-next-question-id 0))
          (generate (nth possible (rand-int (count possible)))))]
    
    (if last-guess (update-question-with-guess last-guess
                     (nth (mongo/fetch :question :where {:session session} :sort {:_id -1} :limit 1) 0)))
    
    (if (or last-guess
            (= get-next-question-id 0))
      (store-question next-question (session/request-to-session request) last-guess))
    
    (html
     (with-history-and-controls
       (session/request-to-session request)
       [:div {:class "quiz quiz-elem"}
        [:h2 (str "Domanda" " "
                  (if (or last-guess
                          (= get-next-question-id 0))
                    (+ 1 get-next-question-id)
                    get-next-question-id))]
        [:form {:name "quiz" :method "post" :action "/italian/old/quiz/" :accept-charset "UTF-8"}
         [:table
          [:tr
           [:td [:h1 
                 (if (or last-guess
                         (= get-next-question-id 0))
                     (get next-question :english)
                     (str (get (nth (mongo/fetch :question :where {:session session} :sort {:_id -1} :limit 1) 0)
                               :question)))]]]
            [:tr
             [:td
              [:textarea {:cols "50" :rows "2" :name "guess" }]]]]
         [:div
          {:style "float:right;padding:1em"
           }
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
    (str
     "<tbody style='display:none'  ><tr><td><div id='"  "tr_" row_id "_js_eval" "'>" formatted-evaluation  "</div></td></tr></tbody>"
     "<tbody>"
     ;; save html in an identified <div> element so it can be passed to javascript.

     ;; TODO: escape english and italian (and the other js args too for that matter : (just use hiccup))
     "<tbody><script>table_row('" row_id"','" english "','"  italian "','" perfect "');</script></tbody>"
     )))

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
        (cond
         (= format "xml")
         (str
          (xml/encoding)
          (str "<container>"
               "<guess>" guess "</guess>"
               "<correctanswer>" (get result :italian) "</correctanswer>"
               "<evaluation>" (get result :evaluation) "</evaluation>"
               "</container>"))
         (= format "tr")
         (table-row result)
         (= format "xmltr")
         (str
          (xml/encoding)
          "<xmltr>"
          (table-row result)
          "</xmltr>")
         true
         (str ;; default: html.
          (xml/encoding)
          (html/showdoctype)
          "<html>"
          (html/head)
          "<div style='width:100%;border-bottom:2px solid grey; float:left'><h1>params</h1>" (html/fs params) "</div>"
          "<table class='test'>"
          (table-row result)
          "</table>"
          "</div>"
          "<div style='width:100%;float:left'><h1>question</h1>" (html/fs content) "</div>"
          "</html>")))
      (str "<error>no guess" guess "</error>"))))

(defn guess [question request format]
  (let [params (if (= (get request :request-method) :get)
                 (get request :query-params)
                 (get request :form-params))
        session (session/request-to-session request)
        guess (get params "guess")
        format (if format format (get params "format"))
        content (merge
                 {:method (get request :request-method)}
                 params
                 {:session session}
                 {:question question})]
    (do
      (if guess
        (let [results (mongo/fetch :question :where {:session session} :sort {:_id -1} :limit 1)]
          (if (and results (> (.size results) 0))
            (update-question-with-guess guess (nth results 0)))))
      (store-question question (session/request-to-session request) guess)
      (let [top1 {:english (get (get content :question) :english)}
            previous-question (previous-question session)
            top2 {:english (get previous-question :english)
                  :italian (get previous-question :italian)
                  :guess (get previous-question :guess)
                  :evaluation (format-evaluation (get previous-question :evaluation) 0)}]
        (cond
         (= format "xml")
         (str
          (xml/encoding)
          (str "<container>"
               "<question>" (get top1 :english) "</question>"
               "<english>" (get top2 :english) "</english>"
               "<italian>" (get top2 :italian) "</italian>"
               "<guess>" (get top2 :guess) "</guess>"
               "<evaluation>" (get top2 :evaluation) "</evaluation>"
               "</container>"))
         (= format "tr")
         (table-row top2)
         (= format "xmltr")
         (str
          (xml/encoding)
          "<xmltr>"
          "<question>" (get top1 :english) "</question>"
          (table-row top2)
          "</xmltr>")
         true
         (str ;; default: html.
          (xml/encoding)
          (html/showdoctype)
          "<html>"
          (html/head)
          "<div style='width:100%;border-bottom:2px solid grey; float:left'><h1>params</h1>" (html/fs params) "</div>"
          "<div style='width:100%;border-bottom:2px solid grey; float:left'><h1>current question</h1>" (html/fs top1) "</div>"
          "<div style='width:100%;border-bottom:2px solid grey; float:left'><h1>previous question</h1>"
          "<table class='test'>"
          (table-row top2)
          "</table>"
          "</div>"
          "<div style='width:100%;float:left'><h1>question</h1>" (html/fs content) "</div>"
          "</html>"))))))

;; a minimal, self-contained quiz applet.
(defn minimal [request]
  (str
   (xml/encoding)
   (html/showdoctype)
   "<html>"
   (html/head)
   "<body onload='ajax_quiz(\"quiz_container\")'>"
   "<div><h2>A very minimal quiz interface..</h2></div>"
   "<div id='quiz_container'>quiz will go in here..</div>"
   "</body>"
   "</html>"))

;; same as (minimal), but with checkboxes for quiz preferences.
(defn quiz-with-prefs [request]
  (basehtml/page "Quiz"
   (html
    [:div {:class "quiz-elem"}
     [:h2 "Quiz" ]
     [:div#quiz_container "if you can see this, either javascript is not enabled or your browser could not contact the server to show the quiz."]
     ]
    [:div#controls_container "if you can see this, either javascript is not enabled or your browser could not contact the server to show the quiz controls."]
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
;     {:comment "html-display a triple: input (english) user guess (italian), correct response (italian)"
;      :test (html/iframe "/italian/guess/?input=you+know&guess=tu+sei")}
     {:comment "fs printing"
      :test (html/fs
             {:most-recent
              (let [qs (mongo/fetch :question :where {:session session} :sort {:_id -1} :limit 1)]
                (if (> (.size qs) 0)
                  (nth qs 0)
                  "(no questions yet.)"))})})))

(defn iframe-controls [session action]
  (str
   (html/myhtml5)
   "<html>"
   (html/head)
   (html [:body
          (controls session action)])))

(defn ajax-controls [session action]
  (html
   (controls session nil "submit_quiz_filters('#controls_container','#controls_form');")))
