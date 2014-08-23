;; TODO: verify using tests that a user authenticated with session 'x' cannot modify a question
;; whose session is 'y' where 'x' != 'y'.
;; (see update-question-by-id-with-guess) where this is enforced by the db/fetch's supplied 'where' constraint.
(ns italianverbs.quiz
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [italianverbs.generate :refer (sentence)]
            [italianverbs.grammar.english :as en]
            [italianverbs.grammar.italiano :as it]
            [italianverbs.html :as html]
            [italianverbs.lev :as lev]
            [italianverbs.lexicon :as lex]
            [italianverbs.lexicon :refer (lexicon)]
            [italianverbs.lexiconfn :as lexfn]
            [italianverbs.morphology :refer (finalize fo)]
            [italianverbs.session :as session]
            [italianverbs.unify :refer (lazy-shuffle)]
            [italianverbs.xml :as xml]
            [korma.core :as k]
            [italianverbs.korma :as db]
            ))

(def production false)
;; ^^ true: pick a pre-generated question from backing store (e.g. postgres db)
;;    false: generate a new question (much slower)

;; to add a new question type:
;; 1. write a function (gen/my-new-question-type) that generates a question.
;; 2. register this function in the (generate) method below.
;; 3. register :my-new-question-type with the (controls) method below.
;; 4. register :my-new-question-type with the question-type-map below.
;; 5. add :my-new-question-type to all-possible-question-types (immediately below).
;; TODO: make 1-5 a macro.
(def all-possible-question-types
  '(:futuro :giorni :imperfetto :mese :mobili :passato :presente))
  ;; '(:mobili :mese :giorni :possessives :partitivo :ora :infinitivo :passato :futuro :presente :espressioni :oct2011 :chetempo :cucina))

(def question-type-map
  {:futuro {:sym :futuro, :desc "futuro semplice verbs",
             :spec {:synsem {:infl :futuro}}},
   :giorni {:sym :giorni, :desc "days of the week"},
   :imperfetto {:sym :imperfetto, :desc "imperfect verbs",
                 :spec {:synsem {:infl :imperfetto}}},
   :mese {:sym :mese :desc "months of the year"},
   :mobili {:sym :mobili :desc "furniture sentences"},
   :possessives {:sym :possessives, :desc "possessive pronouns"},
   :partitivo {:sym :partitivo, :desc "partitive pronouns (e.g. 'degli uomini')"},
   :ora {:sym :ora, :desc "clock times"},
   :infinitivo {:sym :infinitivo, :desc "infinitive verbs"},
   :passato {:sym :passato, :desc "passato prossimo verbs",
             :spec {:synsem {:infl :present
                             :sem {:tense :past}}}},
   :presente {:sym :presente, :desc "present tense verbs",
              :spec {:synsem {:infl :presente}}},
   :espressioni {:sym :espressioni, :desc "useful expressions"},
   :oct2011 {:sym :oct2011, :desc "recently-encountered vocabulary"},
   :chetempo {:sym :chetempo, :desc "weather-related terms"}
   :cucina {:sym :cucina, :desc "kitchen and food-related vocabulary"}
   })

(defn question-type [params]
  (let [type-symbol (get params "type")]
    (if type-symbol
      (let [question-type-details (get question-type-map (keyword type-symbol))]
        (if question-type-details
          (get question-type-details :sym))))))

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

;; TODO: use the same normalize-whitespace as lesson.clj
(defn normalize-whitespace [string]
  string)

(defn- store-question [question-pair session-id last-guess]
  "Store a 'question pair' in the persistent backing store. A 'question pair'
  is an Italian/English pair of sentences that are translations of each other.
   (store-question) may be called from either (question)
   or (guess). In the former case, last-guess will be nil. This is the
   case where the user wants to view a new question. In the latter
   case, the user is submitting an answer to an existing question, and we are
   writing to the backing store the user's guess of the correct answer to
   this question."
  ;; TODO: separate the former and latter cases into separate functions.
  {:pre [(not (= session-id nil))]} ;; precondition: session must not be nil.
  (let [question (get question-pair :english)
        answer (get question-pair :italian)]
    (log/debug (str "question english: " question))
    (log/debug (str "question correct answer: " answer))
    (log/debug (str "question guess: " last-guess))
    (if (nil? question)
      ;; TODO: add this as a precondition.
      (do
        (log/error (str "morphological problem :english is null in: " question-pair))
        (throw (Exception. (str "morphological problem :english value is null.")))))
    (if (nil? answer)
      (do
        (log/error (str "morphological problem :italian is null in: " question-pair))
        (throw (Exception. (str "morphological problem :italian value is null.")))))

    (if (not (string? question))
      (do
        (log/error (str "morphological problem :english is not string: " question))
        (throw (Exception. (str "morphological problem :english value is not string: " question)))))
    (if (not (string? answer))
      (do
        (log/error (str "morphological problem :italian is not string: " answer))
        (throw (Exception. (str "morphological problem :italian value is not string: " answer)))))
    (db/insert! :guess {:question (normalize-whitespace question)
                        :answer (normalize-whitespace answer)
                        :italian (normalize-whitespace answer)
                        :english (normalize-whitespace question)
                        :session session-id})))

(defn clear-questions [session]
  (db/destroy! :queue {:session session})
  session)

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
  (let [results (db/fetch :queue {:session session} :sort {:id -1} :limit 1)]
    (if (> (and results (.size results) 0))
      (get (nth results 0) :id))))

(def using-mongo false)

;; TODO: use function constraints to enforce session :where check.
;; TODO: s/qid/id/
(defn update-question-by-id-with-guess [guess qid session]
  (log/debug (str "updating guess with id=" qid " and session=" session))
  (log/debug (str "(update-question-by-id-with-guess) qid: " qid))
  (let [qid (if using-mongo (new org.bson.types.ObjectId qid)
                (Integer. qid))]
    (log/debug (str "qid (post-munge): " qid))
    (let [question (db/fetch-one :guess
                                 {:id qid})
          question (dissoc question :_id)
          answer (get question :answer)
          answer (if (string? answer) answer "(no answer)")
          ;; If the user had no guess, then use the correct answer as the guess.
          guess (if (and (string? guess) (not (empty? guess))) guess answer)
          guess (normalize-whitespace guess)
          updated-question-map
          (merge
           question
           {:guess guess}
           {:evaluation (str (lev/get-green2 answer guess))}
             )]
      (log/trace (str "doing update for queue row with id: " qid " and updated-question-map:" updated-question-map))
      (db/update! :guess qid
                   updated-question-map)
      updated-question-map)))

;; for testing/sanity checking, might want to refetch (i.e. uncomment the line below and comment out line above).
;;    (db/fetch-one :question :where {:id (new org.bson.types.ObjectId qid)})))

(defn oct2011 []
  (lex/choose-lexeme {:oct2011 true}))

(defn che-tempo []
  (lex/choose-lexeme {:chetempo true}))

(defn random-sentence []
  "choose a random sentence generated via populate.clj/populate."
  (let [count (.size (db/fetch :sentences)) ;; TODO: implement efficiently (i.e. SELECT count(*) FROM ..)
        sentences (db/fetch :sentences)]
    (nth sentences (rand-int count))))

(defn question-type-to-sentence-spec [question-type]
  (cond
   (= question-type :futuro)
   {:synsem {:infl :futuro}}

   (= question-type :imperfetto)
   {:synsem {:infl :imperfetto}}

   (= question-type :passato)
   {:synsem {:infl :present
             :sem {:tense :past}}}

   (= question-type :presente)
   {:synsem {:infl :present
             :sem {:tense :present}}}
   true
   (do
     (log/warn (str "no sentence specification semantics for question type: " question-type " : just using :top for spec."))
     :top)))

(defn generate-question [question-type]
  "maps a question-type to feature structure. right now a big 'switch(question-type)' statement (in C terms)."
  (cond
   production
   (random-sentence) ;; random-sentence takes from a queue of pre-generated sentences - see (random-sentence) for more details about this queue. TODO: add support for question-type.
   true
   (let [spec (question-type-to-sentence-spec question-type)]
     (log/info (str "generate-question with type: " question-type))
     (log/info (str "generate-question with spec: " spec))
     (finalize (sentence spec)))
   (= question-type :oct2011)
   (oct2011)
   (= question-type :chetempo)
   (che-tempo)
;   (= question-type :espressioni)
;   (gen/espressioni)
;   (= question-type :infinitivo)
;   (gen/random-infinitivo)
;   (= question-type :futuro)
;   (gen/random-futuro-semplice)
;   (= question-type :ora)
;   (let [hour (rand-int 12)
;         minute (* (rand-int 12) 5)
;         ampm (if (= (rand-int 2) 0)
;                "am"
;                "pm")
;         hour (if (= hour 0) 12 hour)]
;    {:english (gram/english-time hour minute ampm)
;     :italian (gram/italian-time hour minute ampm)})
   (= question-type :mese)
   (lex/choose-lexeme {:month true})
   (= question-type :giorni)
   (lex/choose-lexeme {:giorni-della-settimana true})))

(defn- controls [session & [ form-action onclick ] ]
  "This is called by show-controls."
  (let [action (if form-action form-action "/italian/quiz/filter")
        onclick (if onclick onclick "submit()")
        record (db/fetch-one :filter {:session session})
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

    ;; TODO: move controls from here to new implementation in (quiz).
    [:div {:id "controls" :class "controls quiz-elem"}
     [:h2 "i tuoi preferiti" [:span#quizbanner  [:script "show_question_types()"]  ]  ]
     ;; TODO: don't create a form if doing ajax (instead will use onclick(), where onclick() is
     ;; potentially generated by javascript.
     [:form#controls_form {:method "post" :action action :accept-charset "utf-8" }
      [:table
;       [:tr
;        (checkbox-col "ora" :ora session "Che ora è?")  ;; e.g. "5:30 => cinque ore e .."
;        (checkbox-col "giorni" :giorni session "giorni della settimana")
;        (checkbox-col "mobili" :mobili session)
;        ]
;       [:tr
;;        (checkbox-col "preposizioni" :preposizioni session "preposizioni" "none")
;        (checkbox-col "partitivo" :partitivo session "articoli determinativi e partivi")
;        (checkbox-col "mese" :mese session "le mese")
;        ]
;       [:tr
;;        (checkbox-col "numeri" :numeri session "numeri" "" "disabled") ;; e.g. "6.458 => sililaquattrocentocinquantotto"
;        (checkbox-col "possessives" :possessives session) ;; e.g. "il tuo cane"
;        (checkbox-col "espressioni" :espressioni session "espressioni utili") ;; e.g. "il tuo cane"
;        ]
;       [:tr
;        (checkbox-col "oct2011" :oct2011 session "oct2011")
;        (checkbox-col "chetempo" :chetempo session "che tempo è?")
;        (checkbox-col "cucina" :cucina session "cucina")
;        ]
       ]
      [:div {:class "optiongroup"}
       [:h4 "Verbi"]
       [:table
        [:tr
;         (checkbox-col "infinitivo" :infinitivo session "infinitivo")  ;; e.g. "fare"
         (checkbox-col "imperfetto" :imperfetto session "imperfetto")  ;; e.g. "andava"
         (checkbox-col "futuro" :futuro session "futuro semplice")  ;; e.g. "tornerai"
         (checkbox-col "passato" :passato session "passato prossimo")  ;; e.g. "io ho fatto"
         (checkbox-col "presente" :presente session "presente indicativo" "")  ;; e.g. "io vado"

         ]
        ]
       ]
      ]

     ;; at least for now, the following is used only as an empty anchor after settings are changed via controls and POSTed.
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

(declare get-filters)

(defn possible-question-types [session]
  (let [possible-question-types all-possible-question-types
        filters (if session
                  (map #(keyword %) (keys (get-filters session))))

        debug (log/info "filtering by user's preferred filters: " filters)
        filtered-results (take 1 (shuffle filters))]
    (if (> (count filtered-results) 0)
      filtered-results
      (do
        (log/warn (str "user's filters: (" filters ") are too restrictive: failed to generate anything that matched them. Just generating from all possible types instead."))
        all-possible-question-types))))

(defn random-guess-type [session]
  (let [possible (possible-question-types session)]
    (log/info (str "possible question types: " (string/join "," possible)))
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
        (db/fetch :queue {:session session} :sort {:id -1} :limit 2)]
    ;; must be at least 2 results: if not, return nil.
    (if (> (.size most-recent-set) 1)
      (nth most-recent-set 1)
      nil)))

(defn table-row-debug-info [answered-question-tuple]
  (str
   "<tr>"
   "<td>id=" (get answered-question-tuple :id) "</td>"
   "</tr>"))

(defn table-row [answered-question-tuple]
  ;; TODO: enforce: :english and :italian may not be null.
  (log/trace (str "table-row: answered-question-tuple: " answered-question-tuple))
  (let [english (get answered-question-tuple :english)
        italian (get answered-question-tuple :italian)
        guess (get answered-question-tuple :guess)
        evaluation (get answered-question-tuple :evaluation)
        row_id (get answered-question-tuple :id)
        debug (log/trace (str "evaluation: " evaluation))

        ;; for now, until we store evaluation column as a postgres vector type:
        evaluation (if using-mongo evaluation (read-string evaluation))

        eval (eval-segments evaluation)

        debug (log/debug (str "eval: " eval))


        ;; translation of evaluation into feedback to user.
        perfect (= (get eval :size) (get eval :match))
        rowspan (if perfect 1 2)
        formatted-evaluation (format-evaluation evaluation 0)]
    ;; TODO: move HTML generation to javascript: just create a javascript call with the params:
    ;; {english,italian,formatted evaluation (itself html), debug info}.
    (let [js (str "table_row('" row_id"', " perfect ")")]
      (html
        [:tr
         [:td
          [:div {:style "display:none"}
           [:div {:id (str row_id "_en")} english ]
           [:div {:id (str row_id "_it")} italian ]
           [:div {:id (str "tr_" row_id "_js_eval")} formatted-evaluation ]
           ]
         ]
        ]
        [:tr
         [:script js]]))))

(def queue-is-on true)

(defn get-queued-question-filtered-by [session filters]
  "update the user's queue by removing any questions that do not match the filters."
  (let [queued-question (db/fetch-one :queue
                                      {:session session})]
    (if (not (nil? queued-question))
      (let [type (:type queued-question)]
        (if (empty? (set/intersection (set (list type))))
          (do
            (log/info (str "removing question: " queued-question " because its type: " type " was incompatible with user's filters: " filters "."))
            (db/destroy! :queue {:id (:id queued-question)})
            ;; try again with the remainder of the questions.
            (get-queued-question-filtered-by session filters))

          ;; this question is allowed by the user's filters: return it.
          queued-question)))))

(defn get-question-from-queue [session]
  (log/info (str "looking for queue for session: " session))
  (if queue-is-on
    (let [queued-question (get-queued-question-filtered-by session (get-filters session))]
      (if (not (nil? queued-question))
        (do
          (log/debug (str "found question with id: " (:id queued-question)))
          ;; remove from queue - now the question is only in memory and lost forever if we crash at this point.
          (db/destroy! :queue {:id (:id queued-question)})
          queued-question)
        nil)))) ;; no question found: the caller will have to generate a new one.

(defn question [request]
  ;; create a new question, store in backing store, and return an HTML fragment with the question's english form
  ;; and the question id to pose question to user.
  ;; TODO: move HTML (presentation layer) to javascript: simply return javascript call to "generate_question()" with
  ;; 2 params: question text and question id.
  ;; note that request has the following keys:
  ;; :remote-addr :scheme :query-params :session :form-params :multipart-params :request-method :query-string
  ;; :route-params :content-type :cookies :uri :server-name :params :headers :content-length :server-port
  ;; :character-encoding :body
  (let [session (session/request-to-session request)
        question-from-queue (get-question-from-queue session)]
    (let [question (if question-from-queue
                     (do
                       (log/debug "found existing question in queue; using that.")
                       (store-question question-from-queue session nil))
                     (do
                       (log/debug "nothing in queue; generating new question.")
                       (let [random-type (random-guess-type session)]
                         (log/info "storing question after generating with type: " random-type)
                         (store-question (generate-question random-type) session nil))))]
      (let [qid (:id question)]
        (log/debug (str "(question) qid: " qid))
        (str "<div id='question_text'>" (:question question) "</div>"
             "<input type='text' id='question_id' value='" qid "'/>")))))

(def fill-to 5)

(defn fillqueue [request]
  (let [session (session/request-to-session request)]
    (log/debug (str "filling queue with request: " request))
    (while
        (let [queue (db/fetch :queue {:session session})]
          (or (nil? queue)
              (< (.size queue) fill-to)))
      (let [random-guess-type (random-guess-type session) ;; chose a question type from amongst those specified by the user's preferences (accessible through session).
            debug (log/debug (str "fillqueue: going to generate sentence with type: " random-guess-type))
            question-pair (generate-question random-guess-type)
            question (get question-pair :english)
            answer (get question-pair :italian)]
        (log/info "adding question to queue.")
        (db/insert! :queue {:italian (normalize-whitespace answer)
                            :english (normalize-whitespace question)
                            :session session})))
    (log/info (str "queue is now big enough: size=" (.size (db/fetch :queue {:session session}))))))

(defn evaluate [request format]
  ;; takes form data back from the user about what their guess was.
  ;; TODO: use function constraints rather than exceptions to enforce:
  ;; qid and session being not-null.
  (log/debug (str "evaluate: request=" request))
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

    (log/debug (str "evaluate: params=" params))
    (log/debug (str "evaluate: guess=" guess))
    (log/debug (str "evaluate: qid=" guess))

    ;; make sure qid is defined.
    (if (= nil qid) (throw (Exception. (str "qid (question ID) is not defined."))))
    (if (= nil session) (throw (Exception. (str "session is not defined."))))
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
         ;; return a string that formats the guess and the evaluation as a HTML table row (a <tr>).
         (table-row result))
      ;; TODO: consider throwing exception here.
      (str "<error>no guess" guess "</error>"))))

(defn guess [question request format]
  (let [params (if (= (get request :request-method) :get)
                 (get request :query-params)
                 (get request :form-params))
        session (session/request-to-session request)
        stored (if (get params "id") ;; if id is nil, then there is no existing question: TODO: figure out under what circumstances id can be nil.
                 (db/fetch-one :guess
                               {:id (new org.bson.types.ObjectId (get params "id"))
                                :session session})
                 (store-question question (session/request-to-session request) nil))]
    (str
     (xml/encoding)
     (html
      [:container {:id (get stored :id) :type (question-type params)}
       [:english (get stored :english)]
       [:italian (get stored :italian)]]))))

;; TODO: not used: removed in favor of (prefs) : below
(defn preferiti [request]
  "this is called by the routing: /preferiti => {:body (quiz/preferiti request)} in core.clj. The controls are rendered in (defn- controls) above."
  (html/page "i tuoi preferiti"
                 (html
                  [:div#controls_container "if you can see this, either javascript is not enabled or your browser could not contact the server to show the quiz controls."] )
                 request
                 "show_quiz_preferences()"))

(declare set-filters)
(defn prefs [session request]
  "update user's preferences based on the their submitted preferences in request"
  (log/info (str "quiz/prefs request:" request))
  (set-filters session request))

(defn get-filters [session]
  (let [filter-result (db/fetch-one :filter {:session session})
        form-params (if filter-result (:form_params filter-result))]
    (if (not (empty? form-params))
      (read-string form-params))))

(defn set-filters [session form-params]
  (do
    (db/destroy! :filter {:session session})
;    (db/destroy! :queue {:session session})
    (log/info (str "setting filters to " form-params))
    (db/insert! :filter {:form_params (str form-params)
                         :session session})
    session))

(defn filter-checkbox [filter checked]
  (let [on? (= "on" (get-in checked (list filter)))
        result
        {:name filter
         :onclick "submit()"
         :type "checkbox"}]
    (if on?
      (merge result
             {:checked "true"})
      result)))

(defn filter-form [session]
  (let [checked
        (get-filters session)]
    (html
     [:form {:action "/quiz/prefs" :method "get"}
      [:table 
       [:tr 
        [:th "Passato prossimo"]
        [:td [:input (filter-checkbox "passato" checked)]]

        [:th "Futuro"]
        [:td [:input (filter-checkbox "futuro" checked)]]
        
        [:th "Imperfetto"]
        [:td [:input (filter-checkbox "imperfetto" checked)]]
        
        [:th "Present"]
        [:td [:input (filter-checkbox "present" checked)]]
        ]

       [:tr
        [:th {:colspan "8" :style "border-top:2px solid #ccc; margin-top:10px; padding-top:10px"}]
        ]
       ]
      ])))

(defn quiz [request]
  (html/page 
   "Quiz"
   (html
    [:div {:class "quiz-elem"}
     [:h2 "Quiz" [:span#quizbanner [:script "show_question_types()" ]]]
     [:div#quiz_container

      [:div.controls

       (filter-form (session/request-to-session request))

       ]

       [:div#qa
        [:table
         [:tr
          [:td
           [:div#ajax_question " "
            [:script "get_next_question()" ]
            ]
           ]

          [:td
           [:div#guess_input_div
            [:input {:size "60" :id "guess_input" :type "text"  }
             [:script "set_guess_input('Guess the Italian?')" ]
             ]
            ]
           ]
          ]]

        [:div#guess_respond_button {:style "width:100%"}
         [:button {:class "click" :onclick "submit_user_response('guess_input')"} "Rispondi" ]
         ]
        ]

       [:table {:id "quiz_table" :class "quiz"} " " ]
       [:div {:style "display:none" :id "stripe_toggle"} "odd" ] ]]
     )

   request
   "ajax_quiz('')"))

;; TODO: more usage of fake session below for more coverage of quiz stateful behavior.
;; TODO: move this from src/ into test/ directory, and from italianverbs.quiz to italianverbs.test.quiz
(defn move-this-test-to-test-namespace []
  (let [session "e0933a66-2b37-4bc7-b4c6-400ff2e81d9a"]
    (list
     {:comment "all possible question types."
      :test all-possible-question-types}

     {:comment "quiz inside a iframe."
      :test (html/iframe "/italian/quiz/?guess=foo")}

     {:comment "fs printing"
      :test (html/tablize
             {:most-recent
              (let [qs (db/fetch :question {:session session} :sort {:id -1} :limit 1)]
                (if (> (.size qs) 0)
                  (nth qs 0)
                  "(no questions yet.)"))})})))

(defn- show-filters [session]
  (let [record (db/fetch-one :filter {:session session})
        filters (if record (get record :form-params))]
    (string/join " "
                  (map (fn [key]
                         (if (get filters (keyword key)) ;; TODO: can remove (keyword) after question-type-map is converted to keywords.
                           (html
                            [:span {:class "qtype"} key ]
                            ;; allow for turning off filters from here (eventually).
                            ;; [:span {:class "qtype remove"} "x" ]
                            )))
                       (keys question-type-map)))))

(defn show-controls [session params action header]
  "Format the set of parameters in quiz.js given in params. Called by the routing: /quiz/filter => (show-controls)."
  (cond (= (get params "format") "titlebar");; for debugging only: prints params in plain text rather than html.
        (show-filters session)
        true
        (html  ;; normal app.
         (controls session nil "submit_quiz_filters('#controls_container','#controls_form');"))))

;; need to do this to get the workbook to work, for some reason.
;(def do-the-minimum (take 1 (gram/minimal-grammatical-initialization)))
