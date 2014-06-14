(ns italianverbs.student
  (:use [hiccup core])
  (:require
   [clj-time.core :as t]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [formative.core :as f]
   [formative.parse :as fp]
   [italianverbs.html :as html]
   [italianverbs.korma :as db]
   [korma.core :as k]))

(declare table)
(declare tr)

(def enroll-form
  {:action "/student/new"
   :fields [{:name :name  :size 50 :label "Name"}
            {:name :email :size 50 :label "Email" :type :email}]
   :method "post"
   :validations [[:required [:name :email]]]})

(defn show-enroll-form [ params & {:keys [problems]}]
  (let [defaults {}]
    (html
     [:div.form {:style "float:left;width:100%;margin:0.5em"}
      [:h3 "Enroll a new student:"]
      (f/render-form (merge enroll-form
                            {:values (merge defaults (:form-params params))
                             :action "/student/new"
                             :method "post"
                             :problems problems}))])))

(defn show [ & [ request enroll-form ] ]
  (let [students
        (db/fetch :user)
        enroll-form (if enroll-form enroll-form
                        (show-enroll-form (:form-params request)))]
    (html
     [:div.major
      [:h2 "Students"]
      (table students)
      enroll-form])))

(defn table [ students & [form-column-fns]]
  (html
   (if (empty? students)
     [:p "No students." ]
     [:table.classes.table-striped
      [:tr
       [:th]
       [:th "Name"]
       [:th "Email"]]
      (tr students 1 form-column-fns)])))

(defn tr [rows & [i form-column-fns]]
  (let [i (if i i 1)]
    (if (not (empty? rows))
      (let [row (first rows)]
        (html
         [:tr
          [:th.num i]
          [:td [:a {:href (str "/student/" (:id row) )} (:fullname row)]]
          [:td [:a {:href (str "/student/" (:id row) )} (:email row)]]
          (if form-column-fns
            [:td
             (form-column-fns row)])
          ]
         
         (tr (rest rows) (+ 1 i) form-column-fns))))))

(defn classes-for-student [student-id]
  (let [student-id (Integer. student-id)]
    (k/exec-raw ["SELECT  * 
                    FROM classes 
              INNER JOIN students_in_classes
                      ON (students_in_classes.class = classes.id)
                     AND (students_in_classes.student = ?)" [student-id]]
                :results)))

(defn show-one [student-id haz-admin]
  (let [student-id (Integer. student-id)
        student (first (db/fetch :student {:_id student-id}))]
    (html
     [:div.major
      [:h2 [:a {:href "/student"} "Students"] " &raquo; " (:fullname student)]
      
      (if (= true haz-admin)
        [:div.testeditor {:style "margin-left:0.25em;float:left;width:100%;"}

         [:h3 "Classes for this student"]
         [:div {:style "float:left;width:100%"}
         (html/table (classes-for-student student-id)
                     haz-admin)]

         ;; TODO: use formative form here rather than handmade form.
         [:h3 "Update student details"]
         [:div {:style "float:left;width:100%"}
          [:form {:action (str "/student/" (:id student) "update") :method "post"}
           [:input {:label "Full Name" :name "fullname" :value (:fullname student)}]
           [:input {:label "Email" :name "email" :value (:email student)}]
           [:button {:onclick "submit()"} "Update"]]]

         [:h3 "Delete student"]
         [:div {:style "float:left;width:100%"}
          [:form {:action (str "/student/" (:id student) "/delete")
                  :method "post"}
           [:button {:onclick "submit()"} "Delete"]]]])])))

;; TODO: show this student's:
;;  - class history
;;  - test history

(defn delete [student-id]
  (let [student-id (Integer. student-id)]
    (log/info (str "deleting student: " student-id))
    (db/fetch-and-modify :student (db/object-id student-id) {} true)
    {:message "deleted student."}))

;; TODO: check for duplicate insertions.
(defn new [request]
  (log/info (str "class/new with request: " (:form-params request)))
  (fp/with-fallback #(html/page "Students"
                                (show request
                                      (show-enroll-form request :problems %))
                                request)
    (let [values (fp/parse-params enroll-form (:form-params request))]
      (let [created-at (t/now)]
        (let [new-student
              (db/insert! :student {:created (str created-at)
                                    :updated (str created-at)
                                    :fullname (get (:form-params request) "name")
                                    :username (get (:form-params request) "email") ;; for now, username is simply email.
                                    :email (get (:form-params request) "email")})
              new-student-id
              (:id new-student)]
          {:status 302
           :headers {"Location" (str "/student/" new-student-id "?message=created")}})))))

(defn delete-class-from-student [ & args])

(defn add-class-to-student [ & args])

