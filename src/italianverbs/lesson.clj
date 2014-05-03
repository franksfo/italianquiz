(ns italianverbs.lesson
  (:use [hiccup core])
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [somnium.congomongo :as db])) ;; TODO: provide database abstraction over mongo and other possible backing stores.

;; "I just realized on my way home that besides the time stamp teachers
;; should be allowed to enter their verbs in sets, sort of like the
;; flashcards in quizlet, with set names. This is because most teachers
;; use textbooks and textbooks are divided in chapters or units. So
;; teachers may like to be ablo to tell the system something
;; like: "create a test with 15 verbs from units 4 and 5" rather
;; than "create a test with 15 verbs entered between 1/11 and 4/21".  If
;; it is possible to do both a time stamp and a tag or name of set that
;; would be ideal."

(defn lesson [session request]
  (html
   [:div {:class "major"}
    (show-tags)
    [:div {:style "float:left;width:100%"}
     [:a {:href "/lesson/new"} "Create a new tag.."]]]))

(defn validate-new-tag [tag]
  "see whether _tag_ is good to add to the db."
  true)

(defn tr-result [results]
  (if (not (empty? results))
    (do
      (log/info (str "TR-RESULT: " (first results)))
      (str (html [:tr [:td (:name (first results))]])
           (tr-result (rest results))))
    ""))

(defn show-tags []
  (html
   [:div {:class "major" :id "tagtable"}
    [:table
     [:tr
      [:th "Name"]
      [:th "Verbs"]]


     (let [results (db/fetch :tag)]
       (tr-result results))

     ]]))

(defn normalize-whitespace [string]
  (string/trim
   (string/replace string #"[ ]+" " ")))

(defn add-new-tag [tag]
    (db/insert! :tag {:name (normalize-whitespace tag)}))

(defn new [session request]
  (log/info (str "/lesson/new with request: " (:form-params request)))

  (if (get (:form-params request) "tag")
    (let [new-tag (get (:form-params request) "tag")]
      (if (validate-new-tag new-tag)
        (do (log/info (str "Adding validated candidate tag: " new-tag))
            (add-new-tag new-tag)))))

  (html
   [:div {:class "major"}

    (show-tags)

    [:div {:style "width:30em;height:30em;float:left"}
     "Create a new tag"

     [:form {:method "post" :action "/lesson/new"}
      [:table
       [:tr
        [:th "Name"]
        [:td
         [:input {:name "tag"}]]
        ]]
      ]
     ]
    ]))
