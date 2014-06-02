;; TODO: s/studenttest/test/
(ns italianverbs.studenttest
  (:use [hiccup core])
  (:require
   [clj-time.core :as t]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [formative.core :as f]
   [formative.parse :as fp]
   [italianverbs.html :as html]
   [italianverbs.korma :as db]
   [italianverbs.unify :as unify]))

(declare add-questions-form)
(declare delete-form)
(declare new-test-form)
(declare select)
(declare show-as-rows)
(declare validate-new-test)
(def new-test-format
  {:fields [{:name :name :label "Test's Name"}]
   :validations [[:required [:name]]
                 [:min-length 1 :groups "Select one or more groups"]]})

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

(defn tr [results haz-admin index]
  (if (and (not (nil? results)) (not (empty? results)))
    (str (html [:tr
                [:th index]
                [:td [:a {:href (str "/test/" (:_id (first results))) } (:name (first results))]]
                (if (= true haz-admin)
                  [:td {:class "edit"}
                   [:form {:method "post" :action (str "/test/" (db/primary-key (first results))
                                                       "/delete")}
                    [:button {:onclick "submit()"} "delete"]]])])
         (tr (rest results) haz-admin (+ 1 index)))
    ""))

(defn tr-questions [questions test-id haz-admin]
  (if (and (not (nil? questions)) (not (empty? questions)))
    (let [question (first questions)]
      (html [:tr 
             [:th (:index question)]
             [:td (:english question)]
             (if haz-admin
               [:td (:italian question)]
               [:td [:input]])
             (if haz-admin
               [:td {:class "edit"}
                [:form {:method "post" :action (str "/question/" (db/primary-key (first questions))
                                                    "/delete")}
                 [:input {:type "hidden" :name "test" :value test-id}]
                 [:button {:onclick "submit()"} "delete"]]])]
            (tr-questions (rest questions) test-id haz-admin)))
    ""))

(declare show-one)
(defn edit-one [test-id]
  (show-one test-id true))

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
            [:table.studenttest
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


        [:div.testeditor {:style "margin-left:0.25em;margin-top:1em;float:left;width:100%;"}
         
         [:button {:onclick (str "document.location='/test/" test-id "/edit'")} "Edit questions"]

         ]

        [:div.testeditor {:style "margin-left:0.25em;float:left;width:100%;"}
         
         [:h3 "Add question" ]
         
         (add-questions-form test {})

         ]]))))

(def add-questions-format
  (let [groups (map #(:name %)
                    (db/fetch :tag))]
    {:action "/question/new"
     :fields [{:name :italiano}
              {:name :english}
              {:name :testid :type :hidden}
              ]
     :validations [[:required [:italianowtf :english]]]}))

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

(defn show [request haz-admin]
  (html
   [:div {:class "major"}
    [:h2 "Tests"]
    [:table
     [:tr
      [:th]
      [:th "Name"]
      (if (= true haz-admin)
        [:th {:class "edit"} "Delete"])]

     (let [results (db/fetch :test)]
       (tr results haz-admin 1))]

    (if haz-admin
      [:div.newlink
       
       ;; TODO: make this a nice-looking button perhaps.
       [:a {:href "/test/new"} "Create a new test"]])]))
