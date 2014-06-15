(ns italianverbs.class
  (:refer-clojure :exclude [class]) ;; probably bad idea to exclude this..
  (:use [hiccup core])
  (:require
   [clj-time.core :as t]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [formative.core :as f]
   [formative.parse :as fp]
   [italianverbs.html :as html]
   [italianverbs.korma :as db]
   [italianverbs.student :as student]
   [italianverbs.studenttest :as tests]
   [korma.core :as k]))

(declare tr)
(declare students-in-class)
(declare students-not-in-class)
(declare tests-for-class)
(declare tests-not-for-this-class)

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
          [:th "Students"]
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

(defn show-one [class-id haz-admin]
  (let [class (first (db/fetch :classes {:_id class-id}))]
    (html
     [:div {:class "major tag"}
      [:h2 [:a {:href "/class"} "Classes" ] " &raquo; " (:name class)]

      (if (= true haz-admin)
        [:div

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

          [:div.classes
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

          [:div.classes
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
         [:h3 "Delete class"]
         [:form {:action (str "/class/" (:id class) "/delete")
                 :method "post"}
          [:button {:onclick "submit()"} "Delete"]]])])))

(defn delete [ class-id ]
  (log/info (str "deleting class: " class-id))
  (db/fetch-and-modify :class (db/object-id class-id) {} true)
  {:message "deleted class"})

(defn new [request]
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
        [:td.num [:a {:href (str "/class/" (:id class))} (if students-per-class students-per-class 0)]]
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

(defn tests-not-for-this-class [class-id]
  (k/exec-raw ["SELECT test.* FROM test 
                             WHERE id NOT IN (SELECT test.id 
                                                FROM tests_in_classes 
                                          INNER JOIN test ON test.id = tests_in_classes.test
                                                         AND class=?)" [class-id]]
               :results))

