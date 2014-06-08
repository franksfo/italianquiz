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
(declare students-for-class)
(declare tests-for-class)

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

(defn show [ request haz-admin]
  (let [classes
        (k/exec-raw ["SELECT classes.id,name,tests,students 
                        FROM classes 
                  LEFT JOIN (SELECT classes.id AS class, count(tests_in_classes.class) AS tests
                                FROM classes 
                          INNER JOIN tests_in_classes ON tests_in_classes.class = classes.id 
                            GROUP BY classes.id) AS test_counts ON test_counts.class = classes.id
                  LEFT JOIN (SELECT classes.id AS class, count(students_in_classes.class) AS students 
                                FROM classes 
                          INNER JOIN students_in_classes ON students_in_classes.class = classes.id
                            GROUP BY classes.id) AS student_counts ON student_counts.class = classes.id
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
      [:h2 [:a {:href "/class/"} "Classes" ] " &raquo; " (:name class)]

      (if (= true haz-admin)
        [:div
         [:h3 {:style "float:left;width:100%"} "Students in this class"]

         (student/table (students-for-class (:id class)))])

      [:h3 {:style "float:left;width:100%"} "Tests for this class"]
      
      (tests/table (tests-for-class (:id class)) {:allow-delete false})

      (if (= true haz-admin)
        [:div.testeditor {:style "margin-left:0.25em;float:left;width:100%;"}
         [:h3 "Rename class"]
         ;; TODO: pass form params rather than {}
         (rename-form class {})
         ])
      ])))

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

(defn add-student-to-class [ & args])

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
        [:td.num [:a {:href (str "/class/" (:id class))} (if tests-per-class tests-per-class 0)]]
        (if (= true haz-admin) [:td [:form {:action (str "/class/" (:id class) "/delete")
                                            :method "post"}
                                     [:button {:onclick "submit()"} "Delete"]]])]
       (tr (rest classes) haz-admin (+ 1 i))))))

(defn students-for-class [class-id]
  (k/exec-raw ["SELECT vc_user.* FROM students_in_classes 
                            INNER JOIN vc_user ON vc_user.id = students_in_classes.student 
                                              AND class=?" [class-id]]
               :results))

(defn tests-for-class [class-id]
  (k/exec-raw ["SELECT test.* FROM tests_in_classes 
                         INNER JOIN test ON test.id = tests_in_classes.test
                                        AND class=?" [class-id]]
               :results))
