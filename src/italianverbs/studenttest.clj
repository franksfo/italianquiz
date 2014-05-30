;; TODO: s/studenttest/test/
(ns italianverbs.studenttest
  (:use [hiccup core])
  (:require
   [clj-time.core :as t]
   [clojure.tools.logging :as log]
   [italianverbs.korma :as db]))

(def demo-form
  {:fields [{:name :h1 :type :heading :text "Section 1"}
            {:name :full-name}
            {:name "user[email]" :type :email}
            {:name :spam :type :checkbox :label "Yes, please spam me."}
            {:name :password :type :password}
            {:name :password-confirm :type :password}
            {:name :h2 :type :heading :text "Section 2"}
            {:name :note :type :html
             :html [:div.alert.alert-info "Please make note of this note."]}
            {:name :date :type :date-select}
            {:name :time :type :time-select}
            {:name :flavors :type :checkboxes
             :options ["Chocolate" "Vanilla" "Strawberry" "Mint"]}
            {:name :location :type :compound
             :fields [{:name :city :placeholder "City" :class "input-medium"}
                      {:name :state :type :us-state :placeholder "Select a state"}]}
            {:name :picture :type :file :title "Choose a file"}]
   :validations [[:required [:full-name "user[email]" :password]]
                 [:min-length 4 :password]
                 [:equal [:password :password-confirm]]
                 [:min-length 2 :flavors "select two or more flavors"]
                 [:complete :location]]
   :action "/test/newdebug"})

(declare delete-form)
(declare new-test-form)
(declare select)
(declare show-as-rows)
(declare validate-new-test)

(defn insert-new-test [test-params]
  "new-test should be a string."
  (let [new-test (get test-params "name")
        params (log/info (str "input test params: " test-params))]

    (if (validate-new-test new-test)
      ;; new-verb checks out ok: add it to DB.
      (do (log/info (str "Adding validated candidate test: " new-test))
          ;; TODO: add return code and append to request.
          (let [created-at (t/now)]
            (db/insert! :test {:created (str created-at)
                               :updated (str created-at)
                               :name new-test}))))))

(defn new [session request]
  (log/debug (str "/studenttest/new with request: " (:form-params request)))
  [:div
   [:h2 "REQUEST"]
   [:div {:style "font-family:monospace"}
    (:form-params request)]])
            
;  (let [new-test (insert-new-test (:form-params request))]
;    {:message (:id new-test)}))

(defn validate-new-test [new-test]
  true)

(defn delete [id]
  (db/fetch-and-modify :test (db/object-id id) {} true)
  {:message "deleted"})

(defn tr [results haz-admin]
  (if (and (not (nil? results)) (not (empty? results)))
    (str (html [:tr 
                [:td [:a {:href (str "/test/" (:_id (first results))) } (:name (first results))]]
                (if haz-admin
                  [:td {:class "edit"}
                   [:form {:method "post" :action (str "/test/" (db/primary-key (first results))
                                                       "/delete")}
                    [:button {:onclick "submit()"} "delete"]]])])
         (tr (rest results) haz-admin))
    ""))

(defn create-a-new-test []
  "create a new test button goes here.")

(defn show-one [request haz-admin]
  (let [script "/* js goes here.. */"
        test-id 5]

    (log/info (str "show-one: request is: " request))
    (html
     [:div {:class "major tag"}
      [:h2 "Tests"]

      [:table
       [:tr
        [:script script]
        [:th]
        [:th "Italian"]
        [:th "English"]
        (if (= true haz-admin)
          [:th {:class "delete"} "Delete"])]
       
       (let [results (list)] ;(db/fetch :question :where {:test test-id})]
         (tr results haz-admin))
       ]

      ;; TODO: be able to add new questions (maybe)
])))

(defn show-tests [haz-admin]
  (let [script "/* js goes here.. */"]
    (html
     [:div {:class "major tag"}
      [:h2 "Tests"]

      [:table
       [:tr
        [:script script]
        [:th "Name"]
        (if (= true haz-admin)
          [:th {:class "edit"} "Edit"])]
       
       (let [results (db/fetch :test)]
         (if (not (nil? results))
           (tr results haz-admin)
           "oops:null results."))
       
       ]
      (if (= true haz-admin)
        (create-a-new-test))
      ])))

(defn show [request haz-admin]
  (html
   [:div {:class "major"}
    [:h2 "Tests"]
    [:table
     [:tr
      [:th "Name"]
      (if (= true haz-admin)
        [:th {:class "edit"} "Edit"])]

     (let [results (db/fetch :test)]
       (tr results haz-admin))]]))

