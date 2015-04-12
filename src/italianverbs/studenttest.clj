;; TODO: s/studenttest/test/
(ns italianverbs.studenttest
  (:use [hiccup core])
  (:require
   [clj-time.core :as t]
   [clj-time.format :as fmt]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
   [formative.core :as f]
   [formative.parse :as fp]
   [italianverbs.auth :refer [get-user-id haz-admin is-admin is-authenticated]]
   [italianverbs.html :as html]
   [italianverbs.korma :as db]
   [italianverbs.morphology :as morph]
   [italianverbs.question :as question]
   [italianverbs.unify :as unify]
   [korma.core :as k]))

;; to evaluate a student's results:
;; SELECT answer AS student_response,italian AS correct 
;;   FROM qsubmit
;;  INNER JOIN question ON question.id = qsubmit.question;

(declare add-questions-form)
(declare delete-form)
(declare generate-questions-form)
(declare new-test-form)
(declare select)
(declare show)
(declare show-as-rows)
(declare show-one)
(declare validate-new-test)
(def new-test-format
  {:fields [{:name :name :label "Test's Name"}]
   :validations [[:required [:name]]
                 [:min-length 0 :groups "Select one or more groups"]]})


(def routes
  (compojure/routes

   (GET "/" request
        (is-admin request
                  {:status 200
                   :body (html/page "Tests" 
                                    (show request (haz-admin))
                                    request)}))

   (GET "/:class" request
        (is-admin request
                          {:body (html/page "Tests" (show-one
                                                     (:test (:route-params request))
                                                     (haz-admin)
                                                     (get-user-id db/fetch))
                                            request)}))))

(defn insert-questions [test-params test-id index]
  (let [index-as-keyword (keyword (str index))]
    (if (and (not (empty? test-params))
             (not (nil? (get test-params index-as-keyword))))
      (let [question (get test-params index-as-keyword)]
        (log/info (str "question index: " question))
        (log/info (str "english: " (get question :english)))
        (log/info (str "italian: " (get question :italian)))
        (let [new-question
              {:test test-id
               :english (get question :english)
               :italian (get question :italian)
               :index index}]
          (db/insert! :question new-question)
          (insert-questions test-params test-id (+ 1 index))))
      (log/info (str "stopped inserting questions at index: " index)))))

(defn insert-new-test [test-params]
  (let [new-test (get test-params "name")
        params (log/info (str "input test params: " test-params))]

    ;; TODO: remove: using formative to validate rather than this custom function call.
    (if (validate-new-test new-test)
      ;; new-verb checks out ok: add it to DB.
      (do (log/info (str "Adding validated candidate test: " new-test))
          ;; TODO: add return code and append to request.
          (let [created-at (t/now)]
            (let [new-test
                  (db/insert! :test {:created (str created-at)
                                     :updated (str created-at)
                                     :name new-test})
                  new-test-id
                  (:id new-test)]
              ;; now add all questions from the test, setting the test-id for each to new-test-id.
              (log/info (str "inserting questions with test-params: " test-params))
              (insert-questions test-params new-test-id 1)
              new-test-id))))))

(defn group-by-question [params]
  (if (not (empty? params))
    (let [entire-key (first (first params))
          value (get params entire-key)]
      (if (re-find #"question\[" (str entire-key))
        ;; decompose the entire-key into constituents
        (let [index (keyword (string/replace entire-key #"^question\[(.*)\]\[(.*)\]" "$1"))
              lang  (keyword (string/replace entire-key #"^question\[(.*)\]\[(.*)\]" "$2"))]
          (unify/merge
           {index {lang value}}
           (group-by-question (dissoc params entire-key))))
        (conj
         {entire-key value}
         (group-by-question (dissoc params entire-key)))))))

;; Note that new-form does not use haz-admin to check whether to render this form: if you aren't authenticated as an admin,
;; it would be a bug for this function to be called at all.
(defn new-form [params & {:keys [problems]}]
  (let [now (java.util.Date.) ;; not using any date or time stuff in the form yet, but good to know about for later.
        defaults {:date now
                  :time now}
        groups (map #(:name %)
                    (db/fetch :tag))]
    (html
     [:div.major
      [:h2 "Create a new test"]
      [:div.testeditor
       (f/render-form (assoc (-> new-test-format
                                 (f/merge-fields [{:name :groups :label "Generate from Groups"
                                                   :type :checkboxes
                                                   :options groups}]))
                        :values (merge defaults (:form-params params))
                        :problems problems))]])))

(defn new [request]
  (fp/with-fallback #(html/page "Create a New Test" (new-form request :problems %) request)
    (let [values (fp/parse-params new-test-format (:form-params request))]
      (log/debug (str "/studenttest/new with request: " (:form-params request)))
      (let [new-test
            (insert-new-test (group-by-question (:form-params request)))]
        {:status 302
         :headers {"Location" (str "/test/" new-test "?message=created")}}))))

(defn update-questions [grouped]
  (if (not (empty? grouped))
    (let [qid (Integer. (string/replace (str (first (first grouped))) ":" ""))
          val (get grouped (first (first grouped)))
          english (:english val)
          italiano (:italian val)]
      (log/debug (str "update: " qid ";" english ";" italiano))
      (db/fetch-and-modify :question (db/object-id qid) {:english english :italian italiano} false)
      (update-questions (dissoc grouped (first (first grouped)))))))

(defn edit-all-from-form [test params]
  (let [grouped (group-by-question params)]
    (log/info "grouped by question: " grouped)
    (update-questions grouped)
    {:message "updated test."}))

(def rename-test-format
  {:fields [{:name :name :label "Name of test"}]
   :validations [[:required [:name]]]})

(defn rename-test-form [test params & {:keys [problems]}]
  (let [defaults {:name (:name test)}]
    (html [:div.testeditor
           (f/render-form (assoc rename-test-format
                            :action (str "/test/" (:id test) "/rename")
                            :values (merge defaults params)
                            :problems problems))])))

(defn validate-new-test [new-test]
  true)

(defn delete-question [id]
  (try
    (do
      (db/fetch-and-modify :question (db/object-id id) {} true)
      {:message "deleted"})
    (catch Exception e
      (do
        (log/error (str "caught exception when trying to delete question: " id " : " e))
        {:message "delete failed - see log for exception."}))))

(defn delete-questions [seq]
  (if (not (empty? seq))
    (let [question (first seq)]
      (let [qid (:_id question)]
        (log/info (str "DELETING QUESTION: " qid))
        (delete-question qid)
        (delete-questions (rest seq))))))
      
(defn delete [test-id]
  (try
    (do
      ;; delete all questions for this test first..
      (delete-questions (db/fetch :question {:test (Integer. test-id)}))
      ;; .. then delete the test itself.
      (db/fetch-and-modify :test (db/object-id test-id) {} true)
      {:message "deleted"})
    (catch Exception e
      (do
        (log/error (str "caught exception when trying to delete test: " test-id " : " e))
        {:message "delete failed - see log for exception."}))))

(defn tr [rows & [i form-column-fns]]
  (if [i (if i i 1)]
    (if (not (empty? rows))
      (let [row (first rows)]
        (html
         [:tr
          [:th i]
          [:td [:a {:href (str "/test/" (:id row)) } (:name row)]]
          (if form-column-fns
            [:td
             (form-column-fns row)])]
         (tr (rest rows) (+ 1 i) form-column-fns))))))

(defn tr-questions [questions test-id haz-admin & [index]]
  (if (and (not (nil? questions)) (not (empty? questions)))
    (let [question (first questions)
          index (if index index 1)]
      (html [:tr 
             [:th.num index]
             [:td (:english question)]
             (if haz-admin
               [:td (:italian question)]
               [:td ]) ;; else, if student, hide the answer (the italian).
             (if haz-admin
               [:td {:class "edit"}
                [:form {:method "post" :action (str "/question/" (db/primary-key (first questions))
                                                    "/delete")}
                 [:input {:type "hidden" :name "test" :value test-id}]
                 [:button {:onclick "submit()"} "delete"]]])]
            (tr-questions (rest questions) test-id haz-admin (+ 1 index))))
    ""))

(defn tr-questions-edit [questions test-id & [index]]
  (if (and (not (nil? questions)) (not (empty? questions)))
    (let [question (first questions)
          index (if index index 1)]
      (html [:tr 
             [:th.num index]
             [:td [:input {:name (str "question[" (:_id question) "][english]")
                           :value (:english question)}]]
             [:td [:input {:name (str "question[" (:_id question) "][italian]")
                           :value (:italian question)}]]]
            (tr-questions-edit (rest questions) test-id (+ 1 index))))
    ""))

(declare show-one)
(defn edit-one [test-id]
  (if (nil? test-id)
    (do
      (log/error (str "show-one was not given a test-id."))
      "")

    (let [script "/* js goes here.. */"
          test (first (db/fetch :test {:_id test-id}))]
      (log/info (str "show-one: test-id: " test-id))
      (html
       [:div {:class "major tag"}
        [:h2 [:a {:href "/test" } "Tests"] " &raquo; " (:name test)]
        (let [questions-for-test (db/fetch :question {:test (Integer. test-id)})]
          (if (and (not (nil? questions-for-test))
                   (not (empty? questions-for-test)))
            [:form {:action (str "/test/" test-id "/edit")
                    :method "post"}
             [:table.studenttest.table-striped
              [:tr
               [:script script]
               [:th]
               [:th "English"]
               [:th "Italiano"]

               (let [questions-for-test (db/fetch :question {:test (Integer. test-id)})]
                 (tr-questions-edit questions-for-test test-id))
               ]]
             [:div {:style "float:left;width:100%;margin-top:1em"}
              [:div {:style "float:left;width:25%"}
               [:input {:type "Submit" :value "Update"}]]
              [:div {:style "float:left;width:25%"}
               [:a {:href (str "/test/" test-id)}
                [:input {:type "button" :value "Cancel"}]]]]]

            [:i "No questions yet."]))]))))

(defn tr-questions-fill-in [questions test-id & [index]]
  (if (and (not (nil? questions)) (not (empty? questions)))
    (let [question (first questions)
          index (if index index 1)]
      (html [:tr 
             [:th.num index]
             [:td (:english question)]
             [:td [:input {:size 50 :name (str "question[" (:_id question) "][italian]")}]]]
            (tr-questions-fill-in (rest questions) test-id (+ 1 index))))
    ""))

(defn test-submit-form [test-id request]
  (if (nil? test-id)
    (do
      (log/error (str "show-one was not given a test-id."))
      "")
    (let [script "/* js goes here.. */"
          test (first (db/fetch :test {:_id test-id}))
          title (str "Taking test: " (:name test))
          questions-for-test (db/fetch :question {:test (Integer. test-id)})]
      (html/page title
                 [:div {:class "major tag"} 
                  [:h2 title]
                  [:form {:action (str "/test/" test-id "/take") :method "post"}
                   [:input {:name "testid" :type :hidden :value test-id}]
                   [:table.studenttest.table-striped
                    [:tr
                     [:script script]
                     [:th]
                     [:th "English"]
                     [:th "Italiano"]]
                    
                    (let [questions-for-test (db/fetch :question {:test (Integer. test-id)})]
                      (tr-questions-fill-in questions-for-test test-id false))
                   ]
                   [:div {:style "float:left;width:90%"}
                    [:input {:type "submit" :value "Done"}]
                    ]
                   ]
                  ]

                 ;; /page
                 request))))

(defn show-one [test-id haz-admin]
  (if (nil? test-id)
    (do
      (log/error (str "show-one was not given a test-id."))
      "")

    (let [script "/* js goes here.. */"
          test (first (db/fetch :test {:_id test-id}))]
      (log/info (str "show-one: test-id: " test-id))
      (html
       [:div {:class "major tag"}
        [:h2 [:a {:href "/test" } "Tests"] " &raquo; " (:name test)]

        (let [questions-for-test (db/fetch :question {:test (Integer. test-id)})]
          (if (and (not (nil? questions-for-test))
                   (not (empty? questions-for-test)))
            [:table.studenttest.table-striped
             [:tr
              [:script script]
              [:th]
              [:th "English"]
              [:th "Italiano"]
              (if (= true haz-admin)
                [:th {:class "edit"} "Delete"])]
             
             (let [questions-for-test (db/fetch :question {:test (Integer. test-id)})]
               (tr-questions questions-for-test test-id haz-admin))
             ]
           
            [:i "No questions yet."]))

        (if (= haz-admin true)
          (do
            (html
             [:h3 [:a {:href (str "/test/" test-id "/take")} "Student view"]]

             [:h3 [:a {:href (str "/test/" test-id "/submittals")} "Submittals"]]

             [:div.testeditor {:style "margin-left:0.25em;margin-top:1em;float:left;width:100%;"}
              [:button {:onclick (str "document.location='/test/" test-id "/edit'")} "Edit questions"]
              ]

            [:div.testeditor {:style "margin-left:0.25em;float:left;width:100%;"}
             [:h3 "Add questions from group"]
             ;; TODO: pass form params rather than {}         
             (generate-questions-form test {})
             ]

            [:div.testeditor {:style "margin-left:0.25em;float:left;width:100%;"}
             [:h3 "Add question" ]
             ;; TODO: pass form params rather than {}
             (add-questions-form test {})
             ])))

        (if (not (= haz-admin true))
          (html
           [:div {:style "float:left;width:100%"}
            [:h3 [:a {:href (str "/test/" test-id "/take")} "Take test"]]]))

        (if (= haz-admin true)
          [:div.testeditor {:style "margin-left:0.25em;float:left;width:100%;"}
           [:h3 "Rename test"]
           ;; TODO: for validation, pass form params rather than {}
           (rename-test-form test {})
           ])

        (if (= haz-admin true)
          [:div.testeditor {:style "margin-left:0.25em;float:left;width:100%;"}
           [:h3 "Delete test"]
           [:form {:method "post"
                   :action (str "/test/" test-id "/delete")}
            [:input.btn.btn-primary {:type "submit" :value "Delete"}]
            ]
           ])
]))))

(defn generate-and-add-question-to-test [test group]
  (let [map-of-tag (first (db/fetch :tag {:_id (db/object-id group)}))
        tag-id group
        tag (:name map-of-tag)
        verbs (:verbs map-of-tag)]
    (log/info "gen-and-add: test=" test "; group=" group)
    (let [verb (first (shuffle verbs))]
      (log/info " gen-and-add verb: " verb)
      ;; TODO: gen/ has gone away; use engine/ instead.
      (let [sentence (morph/finalize "stub")];; (gen/generate-sentence (first (shuffle verbs))))]
        (log/info " gen-and-add sentence: " sentence)
        (question/new {"testid" test
                       "italiano" (:italian sentence)
                       "english" (:english sentence)})))))

(defn generate [test group this-many]
  (log/info (str "adding to test: " test " from group: " group " this many sentences: " this-many))
  (if (> this-many 0)
    (do
      (generate-and-add-question-to-test test group)
      (generate test group (- this-many 1)))
    {:message "generated."}))

(defn rename [test-id name]
  (log/info (str "test-id: " test-id))
  (log/info (str "new-name: " name))
  (db/fetch-and-modify :test (db/object-id test-id) {:name name} false)
  {:message "renamed."})

(def add-questions-format
  {:action "/question/new"
   :fields [{:name :english}
            {:name :italiano}
            {:name :testid :type :hidden}
            ]
   :validations [[:required [:italiano :english]]]})

(defn add-questions-form [test params & {:keys [problems]}]
  (let [now (java.util.Date.) ;; not using any date or time stuff in the form yet, but good to know about for later.
        defaults {:date now
                  :time now}
        params {:testid (:id test)}]
    (html
     [:div.testeditor
      (f/render-form (assoc add-questions-format
                       :values (merge defaults params)
                       :problems problems))])))

(def generate-questions-format
  {:fields [{:name :num-questions :size 3 :label "Add this many"}]})

(defn generate-questions-form [test params & {:keys [problems]}]
  (let [defaults {:num-questions 5}
        groups (map (fn [group]
                      {:label (:name group)
                       :value (:_id group)})
                    (db/fetch :tag))]
    (html
     [:div.testeditor
      (f/render-form (assoc (-> generate-questions-format
                                (f/merge-fields [{:name :group
                                                  :label "from group"
                                                  :type :select
                                                  :options groups}]))
                       :action (str "/test/" (:id test) "/generate")
                       :values (merge defaults params)
                       :problems problems))])))

;; TODO: make 2nd param (options map) optional.
(defn table [rows & [form-column-fns]]
  (html
   (if (empty? rows)
     [:p {:style "float:left"} "no tests."]
     [:table.studenttest.table-striped
      [:tr
       [:th]
       [:th "Name"]]
      (tr rows 1 form-column-fns)])))

(defn tests-and-count-submittals []
  (k/exec-raw ["SELECT test.id,test.name,test.created,count(tsubmit.test) AS taken
                  FROM test 
             LEFT JOIN tsubmit 
                    ON tsubmit.test = test.id
              GROUP BY test.id"]
              :results))

(defn show [request haz-admin]
  (html
   [:div {:class "major"}
    [:h2 "Tests"]

    (html/table (tests-and-count-submittals)
                :columns [:name :created :taken]
                :td (fn [row key]
                      (case key
                        :name [:td [:a {:href (str "/test/" (get row :id))} (get row key)]]
                        :taken [:td.num [:a {:href (str "/test/" (get row :id) "/submittals")} (get row key)]]
                        (html/default-td row key))))

    (if haz-admin
      [:div.newlink
       
       ;; TODO: make this a nice-looking button perhaps.
       [:a {:href "/test/new"} "Create a new test"]])]))
