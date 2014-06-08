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
   [italianverbs.studenttest :as tests]))

(declare tr-classes)

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
        (db/fetch :classes)]
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

         (tr-classes classes haz-admin)])

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

      [:h3 {:style "float:left;width:100%"} "Students in this class"]

      (student/table (db/fetch :user)) ;; where (in this class)

      [:h3 {:style "float:left;width:100%"} "Tests for this class"]

      (tests/table (db/fetch :test) {:allow-delete false})


      [:div.testeditor {:style "margin-left:0.25em;float:left;width:100%;"}
       [:h3 "Rename class"]
       ;; TODO: pass form params rather than {}
       (rename-form class {})
       ]


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

(defn tr-classes [classes haz-admin & [ i ]]
  (if (not (empty? classes))
    (let [class (first classes)
          i (if i i 1)
          students-per-class (:students class)
          tests-per-class (:students class)]
      (html
       [:tr
        [:th.num i]
        [:td [:a {:href (str "/class/" (:id class))} (:name class)]]
        [:td.num [:a {:href (str "/class/" (:id class))} students-per-class]]
        [:td.num [:a {:href (str "/class/" (:id class))} tests-per-class]]
        (if (= true haz-admin) [:td [:form {:action (str "/class/" (:id class) "/delete")
                                            :method "post"}
                                     [:button {:onclick "submit()"} "Delete"]]])]
       (tr-classes (rest classes) haz-admin (+ 1 i))))))

