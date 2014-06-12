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
   [italianverbs.korma :as db]))

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

(defn show-one [student-id]
  (html
   [:div.major
    [:h2 "Students (show one)"]]))
;; TODO: show this student's:
;;  - class history
;;  - test history

(defn delete [student-id]
  (let [student-id (Integer. student-id)]
    (log/info (str "deleting student: " student-id))
    (db/fetch-and-modify :class (db/object-id student-id) {} true)
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

