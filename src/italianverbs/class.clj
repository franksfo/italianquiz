;; Note: this file is about 'class' as in 'a class of students in a classroom', 
;; not 'class' in the programming language sense.
;; TODO: find a better name for "a set of students and a teacher associated for a given period of time" than "class".
;; maybe 'course-term' or something like that.
(ns italianverbs.class
  (:refer-clojure :exclude [class]) ;; probably bad idea to exclude these, at least from the point of view of readability by other developers, since they are basic.
  (:use [hiccup core])
  (:require
   [cemerick.friend :as friend]
   [clj-time.core :as t]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
   [formative.core :as f]
   [formative.parse :as fp]
   [italianverbs.auth :refer [get-user-id haz-admin is-authenticated]]
   [italianverbs.html :as html]
   [italianverbs.korma :as db]
   [italianverbs.student :as student]
   [italianverbs.studenttest :as tests]
   [korma.core :as k]))

(declare add-test)
(declare add-user)
(declare create)
(declare delete)
(declare delete-from-class)
(declare new-form)
(declare rename)
(declare tr)
(declare remove-test)
(declare remove-user)
(declare show-one)
(declare students-in-class)
(declare students-not-in-class)
(declare tests-for-class)
(declare tests-for-class-for-student)
(declare tests-not-for-this-class)
(declare show)
(declare onload)
(declare body)

(defn onload []
  "")
(def headers {"Content-Type" "text/html;charset=utf-8"})

(defn body [title content request]
  (html/page 
   title
   (html
    [:div {:class "major"}
     [:h2 "Classes"]
     content])
   request
   {:css ""
    :jss [""]
    :onload (onload)}))

(def routes
  (compojure/routes

   (GET "/" request
        (is-authenticated request
                          {:status 200
                           :body (html/page "Classes" 
                                            (show request (haz-admin))
                                            request)}))

   (GET "/my" request
        (is-authenticated request
                          {:status 200
                           :body (html/page "My Classes" 
                                            (show request (haz-admin))
                                            request)}))


   (POST "/:class/addtest/:test" request
         (friend/authorize #{::admin}
                           (let [class (:class (:route-params request))
                                 test (:test (:route-params request))]
                             (let [result (add-test class test)]
                               {:status 302
                                :headers {"Location" (str "/class/" class "?result=" (:message result))}}))))

   (GET "/:class/removeuser/:student" request
        (friend/authorize #{::admin}
                          (let [class (:class (:route-params request))
                                student (:student (:route-params request))
                                redirect (str "/class/" class)
                                result (remove-user class student)]
                            {:status 302
                             :headers {"Location" (str redirect "?result=" (:message result))}})))

   (POST "/:class/removeuser/:student" request
         (friend/authorize #{::admin}
                           (let [class (:class (:route-params request))
                                 debug (log/info (str "PROCESSING REMOVE - THE FORM-PARAMS ARE: " (:form-params request)))
                                 student (:student (:route-params request))
                                 redirect (get (:form-params request) "redirect")
                                 redirect (if redirect redirect
                                              (str "/class/" class))
                                 result (remove-user class student)]
                             {:status 302
                              :headers {"Location" (str redirect "?result=" (:message result))}})))

   (GET "/:class/removetest/:test" request
        (friend/authorize #{::admin}
                          (let [class (:class (:route-params request))
                                test (:test (:route-params request))]
                            (let [result {:message "Ignoring and Redirecting."}]
                              {:status 302
                               :headers {"Location" (str "/class/" class "?result=" (:message result))}}))))

   (POST "/:class/removetest/:test" request
         (friend/authorize #{::admin}
                           (let [class (:class (:route-params request))
                                 test (:test (:route-params request))]
                             (let [result (remove-test class test)]
                               {:status 302
                                :headers {"Location" (str "/class/" class "?result=" (:message result))}}))))

   (GET "/:class/delete" request
        {:status 302
         :headers {"Location" (str "/class")}})

   (POST "/:class/delete" request
         (friend/authorize #{::admin}
                           (let [class (:class (:route-params request))]
                             (let [result (delete class)]
                               {:status 302
                                :headers {"Location" (str "/class?result=" (:message result))}}))))

   ;; for now, just redirect GET /class/new -> GET /class
   (GET "/new" request
        (friend/authorize #{::admin}
                          {:body (html/page "Classes"
                                            (new-form request)
                                            request)}))

   (GET "/new/" request
        (friend/authorize #{::admin}
                          {:body (html/page "Classes"
                                            (new-form request)
                                            request)}))

   (POST "/new" request
         (friend/authorize #{::admin}
                           (create request)))

   (POST "/new/" request
         (friend/authorize #{::admin}
                           (create request)))

   (POST "/:id/rename" request
         (friend/authorize #{::admin}
                           (let [class-id (:id (:route-params request))
                                 name (get (:form-params request) "name")]
                             (let [result (rename class-id name)]
                               {:status 302
                                :headers {"Location" (str "/class/" class-id "?result=" (:message result))}}))))

   (GET "/:class" request
        (is-authenticated request
                          {:body (html/page "Classes" (show-one
                                                       (:class (:route-params request))
                                                       (haz-admin)
                                                       (get-user-id db/fetch))
                                            request)}))

   (GET "/:class/" request
        {:status 302
         :headers {"Location" (str "/class/" (:class (:route-params request)))}})
   
   (GET "/:class/delete/:student/" request
        (friend/authorize #{::admin}
                          (let [tag (:class (:route-params request))
                                verb (:verb (:route-params request))]
                            (let [result {:message "redirected-no-effect"}]
                              {:status 302
                               :headers {"Location" (str "/class/" tag "/")}}))))

   (POST "/:class/delete/:student" request
         (friend/authorize #{::admin}
                           (let [class (:class (:route-params request))
                                 student (:student (:route-params request))
                                 result (delete-from-class class student)
                                message (:message result)
                                 redirect (get (:form-params request) "redirect")
                                 redirect (if redirect redirect
                                              (str "/class/" class))]
                             {:status 302
                              :headers {"Location" (str "/class/" class)}})))

   (GET "/:class/add/:student" request
        (friend/authorize #{::admin}
                          (let [class-id (:class (:route-params request))]
                            {:status 302
                             :headers {"Location" (str "/class/" class-id)}})))

   (POST "/:class/add/:student" request
         (friend/authorize #{::admin}
                           (let [class (:class (:route-params request))
                                 student (:student (:route-params request))
                                 result (add-user class student)
                                 message (:message result)
                                 redirect (get (:form-params request) "redirect")
                                 redirect (if redirect redirect
                                              (str "/class/" class))]
                             {:status 302
                              :headers {"Location" (str redirect "?result=" message)}})))))

(def class-form
  {:fields [{:name :name :size 50 :label "Class Name"}]
   :validations [[:required [:name]]
                 [:min-length 1 :tests "Select one or more tests"]]})

;; Note that new-form does not use haz-admin to check whether to render this form: if you aren't authenticated as an admin,
;; it would be a bug for this function to be called at all.
(defn new-form [params & {:keys [problems]}]
  (let [now (java.util.Date.) ;; not using any date or time stuff in the form yet, but good to know about for later.
        defaults {:date now
                  :time now}
        tests (map #(:name %)
                   (db/fetch :test))]
    (html
     [:div.major
      [:h2 "Create a new class"]
      [:div.testeditor
       (f/render-form (assoc (-> class-form
                                 (f/merge-fields [{:name :tests :label "Tests for this class"
                                                   :type :checkboxes
                                                   :options tests}]))
                        :values (merge defaults (:form-params params))
                        :action "/class/new"
                        :method "post"
                        :problems problems))]])))

(defn table [ rows ])

(defn show [ request haz-admin]
  (let [classes
        (k/exec-raw ["SELECT  classes.id,name,tests,students 
                        FROM  classes 

                   LEFT JOIN  (SELECT  classes.id AS class, count(tests_in_classes.class) AS tests
                                 FROM  classes 
                           INNER JOIN  tests_in_classes ON tests_in_classes.class = classes.id 
                             GROUP BY  classes.id) AS test_counts 
                          ON  test_counts.class = classes.id

                   LEFT JOIN  (SELECT  classes.id AS class, count(students_in_classes.class) AS students 
                                 FROM  classes 
                           INNER JOIN  students_in_classes ON students_in_classes.class = classes.id
                             GROUP BY  classes.id) AS student_counts 
                          ON  student_counts.class = classes.id
"] :results)]
    (html
     [:div {:class "major tag"}
      [:h2 "Classes"]
      (if (empty? classes)
        [:p "no classes." ]
        [:table.classes.table-striped
         [:tr
          [:th]
          [:th "Name"]
          (if haz-admin [:th "Students"])
          [:th "Tests"]
          ]

         (tr classes haz-admin)])

      (if (= true haz-admin)
        [:div {:style "float:left;width:100%"} [:a {:href "/class/new"}
               "Create a new class"]])])))

(def rename-format
  {:fields [{:name :name :size 50 :label "Name of class"}]
   :validations [[:required [:name]]]})

(defn rename-form [class params & {:keys [problems]}]
  (let [defaults {:name (:name class)}]
    (html [:div.testeditor
           (f/render-form (assoc rename-format
                            :method "post"
                            :action (str "/class/" (:id class) "/rename")
                            :values (merge defaults params class)
                            :problems problems))])))

(defn rename [class-id new-name]
  (db/fetch-and-modify :class class-id {:name new-name})
  {:message "renamed."})

(defn show-one [class-id haz-admin student-id]
  (let [class (first (db/fetch :classes {:_id class-id}))]
    (html
     [:div {:class "major tag"}
      [:h2 [:a {:href "/class"} "Classes" ] " &raquo; " (:name class)]

      (if (and student-id (not (= true haz-admin)))
        [:div
         [:h3 {:style "float:left;width:100%;margin-top:1em;margin-bottom:0;text-align:center"} "Welcome to the class!"]

         [:div {:style "float:left;width:50%;"}
          [:h4  {:style "float:left;width:50%;margin-top:1em;margin-bottom:0;text-align:center"} "Tests"]
          (html/table (tests-for-class-for-student (:id class) student-id)
                      :columns [:test :taken :id]
                      :none "No tests for this class yet."
                      :th (fn [key] (case key
                                      :id [:th ""]
                                      [:th ""]))
                      :td (fn [row key] (case key
                                          :test [:td [:a {:href (str "/test/" (get row :id))}
                                                      (get row key)]]
                                          :id [:td [:button {:onclick (str "document.location='/test/" (get row :id) "/take'")}
                                                    (if (= 0 (get row :taken))
                                                      "Take"
                                                      "Take again")]]
                                          :taken [:td.num {:style "padding-right:1em"}
                                                  (let [text
                                                        (if (= 0 (get row :taken))
                                                          "You haven't taken this test yet."
                                                          [:span "You've taken this test " [:a {:href (str "/test/" (get row :id) "/mine")}
                                                                                            (get row :taken) " time" (if (not (= 1 (get row :taken))) "s") "."]])]
                                                    text)]
                                         (html/default-td row key))))]])

      (if (= true haz-admin)
        [:div

         ;; TODO: show test results aggregated by student and test.
         
         [:h3  {:style "float:left;width:100%;margin-top:1em;margin-bottom:0;text-align:center"} "Students"]
         [:div {:style "float:left;width:100%"}
          [:div {:style "float:left;width:50%"}
           [:h3 "Students in this class"]
           (student/table (students-in-class (:id class))
                          (fn [row]
                            [:form
                             {:action (str "/class/" (:id class) "/removeuser/" (:id row))
                              :method "post"}
                             [:button {:onclick "submit()"} "Remove"]]))
           ]

          [:div.classes {:style "float:left;width:50%"}
           [:h3 "Add Students to this class"]
           (student/table (students-not-in-class (:id class))
                          (fn [row]
                            [:form
                             {:action (str "/class/" (:id class) "/add/" (:id row))
                             :method "post"}
                             [:button {:onclick "submit()"} "Add"]]))
           ]]

         [:h3  {:style "float:left;width:100%;margin-top:1em;margin-bottom:0;text-align:center"} "Tests"]
         [:div {:style "float:left;width:100%;"}
          [:div {:style "float:left;width:50%"} 
           [:h3 "Tests for this class"]
           (tests/table (tests-for-class (:id class))
                        (fn [row]
                          [:form
                           {:action (str "/class/" (:id class) "/removetest/" (:id row))
                            :method "post"}
                           [:button {:onclick "submit()"} "Remove"]]))
           ]

          [:div.classes {:style "float:left;width:50%"}
           [:h3 "Add tests to this class"]
           (tests/table (tests-not-for-this-class (:id class))
                        (fn [row]
                          [:form
                           {:action (str "/class/" (:id class) "/addtest/" (:id row))
                            :method "post"}
                          [:button {:onclick "submit()"} "Add"]]))]]
         ])

      (if (= true haz-admin)
        [:div.testeditor {:style "margin-left:0.25em;float:left;width:100%;"}
         [:h3 "Rename class"]
         ;; TODO: pass form params rather than {}
         (rename-form class {})
         ])

      (if (= true haz-admin)
        [:div.testeditor {:style "margin-left:0.25em;float:left;width:100%;"}
         [:h3 {:style "float:left"} "Delete class"]
         [:div [:form {:style "float:left;width:100%" :action (str "/class/" (:id class) "/delete")
                 :method "post"}
          [:button {:onclick "submit()"} "Delete"]]]])])))

(defn delete [ class-id ]
  (log/info (str "deleting class: " class-id))
  (db/fetch-and-modify :class (db/object-id class-id) {} true)
  {:message "deleted class"})

(defn create [request]
  (log/info (str "class/new with request: " (:form-params request)))
  (fp/with-fallback #(html/page "Create a new class" 
                                (new-form request :problems %) request)
    (let [values (fp/parse-params class-form (:form-params request))]
      (let [created-at (t/now)]
        (let [new-class
              (db/insert! :class {:created (str created-at)
                                  :updated (str created-at)
                                  :name (get (:form-params request) "name")})
              new-test-id
              (:id new-class)]
          {:status 302
           :headers {"Location" (str "/class/" new-test-id "?message=created")}})))))

(defn delete-from-class [ & args])

(defn tr [classes haz-admin & [ i ]]
  (if (not (empty? classes))
    (let [class (first classes)
          i (if i i 1)
          students-per-class (:students class)
          tests-per-class (:tests class)]
      (html
       [:tr
        [:th.num i]
        [:td [:a {:href (str "/class/" (:id class))} (:name class)]]
        (if haz-admin
          [:td.num [:a {:href (str "/class/" (:id class))} (if students-per-class students-per-class 0)]])
        [:td.num [:a {:href (str "/class/" (:id class))} (if tests-per-class tests-per-class 0)]]]
       (tr (rest classes) haz-admin (+ 1 i))))))

(defn add-user [class-id user-id]
  (let [class-id (Integer. class-id)
        user-id (Integer. user-id)]
    (try
      (do
        (k/exec-raw ["INSERT INTO students_in_classes (class,student) VALUES (?,?)" [class-id user-id]])
        {:message "Added user."})
      (catch Exception e
          (do
            (log/info (str "INSERT failed: " e))
            {:result "Adding user to class failed."})))))

(defn remove-user [class-id user-id]
  (let [class-id (Integer. class-id)
        user-id (Integer. user-id)]
    (try
      (do
        (k/exec-raw ["DELETE FROM students_in_classes WHERE class=? AND student=?" [class-id user-id]])
        {:message "Removed user."})
      (catch Exception e
          (do
            (log/info (str "Removing user from class failed: " e))
            {:result "Removing user from class failed."})))))

(defn add-test [class-id test-id]
  (let [class-id (Integer. class-id)
        test-id (Integer. test-id)]
    (try
      (do
        (k/exec-raw ["INSERT INTO tests_in_classes (class,test) VALUES (?,?)" [class-id test-id]])
        {:message "Added test."})
      (catch Exception e
          (do
            (log/info (str "INSERT failed: " e))
            {:result "Adding test to class failed."})))))

(defn remove-test [class-id test-id]
  (let [class-id (Integer. class-id)
        test-id (Integer. test-id)]
    (try
      (do
        (k/exec-raw ["DELETE FROM tests_in_classes WHERE class=? AND test=?" [class-id test-id]])
        {:message "Removed test."})
      (catch Exception e
          (do
            (log/info (str "Removing test from class failed: " e))
            {:result "Removing test from class failed."})))))

(defn students-in-class [class-id]
  (k/exec-raw ["SELECT vc_user.* 
                  FROM students_in_classes 
            INNER JOIN vc_user ON vc_user.id = students_in_classes.student 
                              AND class=?" [class-id]]
               :results))

(defn students-not-in-class [class-id]
  (k/exec-raw ["SELECT vc_user.* 
                  FROM vc_user WHERE id NOT IN (SELECT vc_user.id 
                                                  FROM students_in_classes 
                                            INNER JOIN vc_user ON vc_user.id = students_in_classes.student 
                                                              AND class=?)" [class-id]]
               :results))

(defn tests-for-class [class-id]
  (k/exec-raw ["SELECT test.* 
                  FROM tests_in_classes 
            INNER JOIN test ON test.id = tests_in_classes.test
                           AND class=?" [class-id]]
               :results))

(defn tests-for-class-for-student [class-id student-id]
  (k/exec-raw ["SELECT test.id,test.name AS test,count(tsubmit) AS taken
                  FROM tests_in_classes 
            INNER JOIN test 
                    ON test.id = tests_in_classes.test
                   AND class= ?
             LEFT JOIN tsubmit
                    ON tsubmit.student = ?
                   AND tsubmit.test = test.id
              GROUP BY tsubmit.student,test.name,test.id" [class-id student-id]]
              :results))

(defn tests-not-for-this-class [class-id]
  (k/exec-raw ["SELECT test.* FROM test 
                             WHERE id NOT IN (SELECT test.id 
                                                FROM tests_in_classes 
                                          INNER JOIN test ON test.id = tests_in_classes.test
                                                         AND class=?)" [class-id]]
               :results))


