(ns italianverbs.test_submit
  (:require [clojure.string :as string]
            [clojure.tools.logging :as log]
            [hiccup.core :as hc]
            [italianverbs.html :as html]
            [italianverbs.korma :as db]
            [korma.core :as k]))

(defn insert-answers [responses test-submit-id]
  (if (not (empty? responses))
    (let [key (first (first responses))]
      (if (re-find #"^:\d+$" (str key))
        (let [questionid (Integer. (string/replace (str key) #"^:" ""))
              answer (:italian (second (first responses)))]
          (log/info (str "question id: " questionid))
          (log/info (str "anwser: " answer))
          (db/insert! :question-submit {:tsubmit test-submit-id
                                        :question questionid
                                        :answer answer})))
      (insert-answers (rest responses) test-submit-id))))

(defn new [test_id responses user]
  (log/info (str "submitting test: " test_id " for user: " user " with data: " responses))
  (let [user_id (:id (first (db/fetch :user {:username (:username user)})))]
    (if (nil? user_id)
      (do
        (log/error (str "no user in database found for user object: " user))
        {:message "error:no such user"})
      (let [tsubmit
            (db/insert! :test-submit {:test (Integer. test_id)
                                      :student (Integer. user_id)})]
        ;; iterate over form-params and add each as a question-submit for this test-submit (tsubmit).
        (insert-answers responses (:id tsubmit))
        {:testid test_id
         :submittalid (:id tsubmit)
         :message "submitted"}))))

(defn test-submittals [test-id]
  (k/exec-raw ["SELECT student.fullname AS student,student.id AS student_id,
                       tsubmit.created AS taken,tsubmit.id AS results
                  FROM tsubmit 
            INNER JOIN test
                    ON test.id = tsubmit.test 
            INNER JOIN vc_user AS student 
                    ON tsubmit.student = student.id
                 WHERE test.id=?" [test-id]]
              :results))

(defn test-submittal [submit-id]
  (k/exec-raw ["SELECT question.italian,qsubmit.answer
                  FROM qsubmit
            INNER JOIN tsubmit 
                    ON tsubmit.id = qsubmit.tsubmit
            INNER JOIN question
                    ON question.id = qsubmit.question
                 WHERE tsubmit = ?" [submit-id]]
              :results))

(defn submittals [test-id]
  "show all submittals for this test."
  (let [test (first (db/fetch :test {:_id test-id}))
        test-name (:name test)]
    (hc/html
     [:div {:class "major"}
      [:h2 [:a {:href "/test"} "Tests"] " &raquo; " [:a {:href (str "/test/" test-id)} test-name] " &raquo; Test Submittals"]
      (html/table
       (test-submittals test-id)
       :columns [:student :taken]
       :th (fn [key]
             (case key
               :results html/hide
               (html/default-th key)))
       :td (fn [row key]
             (case key
               :taken
               [:td.date [:a {:href (str "/test/" test-id "/submittals/" (get row :results))} (html/display-time (get row key))]]
               (html/default-td row key))))])))

(defn submittals-by-student [test-id student-id]
  (let [test (first (db/fetch :test {:_id test-id}))
        test-name (:name test)]
    (hc/html
     [:div {:class "major"}
      [:h2 [:a {:href "/test"} "Tests" ] " &raquo; "[:a {:href (str "/test/" test-id) } test-name ] " &raquo; My submittals"]

      (let [submittals
            (k/exec-raw ["SELECT created AS taken,id FROM tsubmit WHERE test = ? AND student = ?" [test-id student-id]]
                        :results)]
        (if (> (.size submittals) 0)
          (map (fn [submittal]
                 [:div {:style "float:left;width:40%"}
                  [:h3 (html/display-time (:taken submittal))]

                  (html/table

                   (test-submittal (:id submittal))
                   :columns [:italian :answer]
                   :td (fn [row key]
                         (case key
                           :answer
                           (let [correct (get row :italian)
                                 correctness (if (= (get row :italian)
                                                    (get row key)) "correct" "incorrect")]
                             [:td {:class correctness}
                              (get row key)])
                           (html/default-td row key))))])
               submittals)
          [:p "You haven't taken this test yet."]))])))

(defn submittal [test-id submit-id]
  "show one test submittal"
  (let [test (first (db/fetch :test {:_id test-id}))
        test-name (:name test)
        submit (first (db/fetch :tsubmit {:_id submit-id}))
        student-id (:student submit)
        student (first (db/fetch :student {:_id student-id}))]
    (hc/html
     [:div {:class "major"}
      [:h2 [:a {:href "/test"} "Tests" ] " &raquo; "[:a {:href (str "/test/" test-id) } test-name ] " &raquo; " 
       
       [:a {:href (str "/test/" test-id "/submittals")} "Test Submittals" ] " &raquo; " " Submittal " submit-id ]

      [:div {:style "float:left;width:100%"}
       [:table
        [:tr
         [:th "Student"]
         [:td (:fullname student)]]
        [:tr
         [:th "When"]
         [:td (html/display-time (:created submit))]]]]
      (html/table
       (test-submittal submit-id)
       :columns [:italian :answer]
       :td (fn [row key]
             (case key
               :answer
               (let [correct (get row :italian)
                     correctness (if (= (get row :italian)
                                        (get row key)) "correct" "incorrect")]
                 [:td {:class correctness}
                  (get row key)])
               (html/default-td row key))))])))

               



