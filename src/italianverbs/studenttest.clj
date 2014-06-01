;; TODO: s/studenttest/test/
(ns italianverbs.studenttest
  (:use [hiccup core])
  (:require
   [clj-time.core :as t]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [formative.core :as f]
   [formative.parse :as fp]
   [italianverbs.korma :as db]
   [italianverbs.unify :as unify]))

(declare delete-form)
(declare new-test-form)
(declare select)
(declare show-as-rows)
(declare validate-new-test)

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

(defn new [session request]
  (log/debug (str "/studenttest/new with request: " (:form-params request)))
  (insert-new-test (group-by-question (:form-params request))))

(defn validate-new-test [new-test]
  true)

(defn delete-question [id]
  (log/info (str "WTF TYRING TO DELET QUESTION : " id))
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

        [:table.studenttest
         [:tr
          [:script script]
          [:th]
          [:th "English"]
          [:th "Italian"]
          (if (= true haz-admin)
            [:th {:class "delete"} "Delete"])]
       
         (let [questions-for-test (db/fetch :question {:test (Integer. test-id)})]
           (tr-questions questions-for-test test-id haz-admin))
         ]

        (if (not (= true haz-admin))
          [:div {:style "float:left;width:90%;margin:0.5em"}
           [:input {:type "submit"}]])

        ;; TODO: be able to add new questions (maybe)
]))))

(defn show [request haz-admin]
  (html
   [:div {:class "major"}
    [:h2 "Tests"]
    [:table
     [:tr
      [:th]
      [:th "Name"]
      (if (= true haz-admin)
        [:th {:class "edit"} "Edit"])]

     (let [results (db/fetch :test)]
       (tr results haz-admin 1))]

    (if haz-admin
      [:div.newlink
       
       ;; TODO: make this a nice-looking button perhaps.
       [:a {:href "/test/new"} "Create a new test"]])]))

(def demo-form
  (let [groups (map #(:name %)
                    (db/fetch :tag))]
    {:fields [{:name :h1 :type :heading :text "Section 1"}
              {:name :name :label "Test's Name"}
              {:name :groups :label "Generate from Groups" :type :checkboxes
               :options groups}]
     :validations [[:required [:name]]
                   [:min-length 1 :groups "Select one or more groups"]]}))

(def renderer-form
  {:method "get"
   ;:renderer :inline
   :submit-label nil
   :fields [{:name :renderer
             :type :select
             :options ["bootstrap-horizontal"
                       "bootstrap-stacked"
                       "bootstrap3-stacked"
                       "table"]
             :onchange "this.form.submit()"}]})

(defn show-demo-form [params & {:keys [problems]}]
  (let [renderer (if (:renderer params)
                   (keyword (:renderer params))
                   :bootstrap-horizontal)
        now (java.util.Date.)
        defaults {:spam true
                  :date now
                  :time now}]
    (html
      {:renderer renderer}
      [:div.pull-right.well.well-small
       (f/render-form (assoc renderer-form :values params))]
      [:div.pull-left {:style "width: 50%"}
       (f/render-form (assoc demo-form
                             :renderer renderer
                             :values (merge defaults params)
                             :problems problems))])))
;; we don't check haz-admin here: if you aren't authenticated as an admin,
;; it would be a bug for this to be called.
(defn new-form [params & {:keys [problems]}]
  (let [now (java.util.Date.) ;; not using any date or time stuff in the form yet, but good to know about for later.
        defaults {:date now
                  :time now}]
    (html
     (f/render-form (assoc demo-form
                      :values (merge defaults params)
                      :problems problems)))))


