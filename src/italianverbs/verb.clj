(ns italianverbs.verb
  (:use [hiccup core])
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [italianverbs.db :as db]
   [italianverbs.morphology :refer (normalize-whitespace)]
   ))

(declare new-verb-form)
(declare show-verbs)
(declare show-as-rows)
(declare validate-new-verb)

(defn select [session request]
  (let [script "/* js goes here.. */"]
    (html
     [:div {:class "major verbs"}
      [:h2 "Verbs"]

      [:table
       [:tr
        [:script script]
        [:th ""]
        [:th "Verb"]
        [:th {:class "edit"} "Edit"]]
       
       (let [results (db/fetch :verb)]
         (show-as-rows results))
       ]
      (new-verb-form)
      ])))

(defn select-one [verb]
  (let [verb (db/fetch :verb :where {:_id (db/object-id verb)})]
    "yup yup"))

(defn new [session request]
  (log/debug (str "/verb/new with request: " (:form-params request)))
  (if (get (:form-params request) "verb")
    (let [new-verb (get (:form-params request) "verb")]
      (if (validate-new-verb new-verb)
        ;; new-verb checks out ok: add it to DB.
        (do (log/info (str "Adding validated candidate tag: " new-verb))
            ;; TODO: add return code and append to request.
            (db/insert! :verb {:italian (normalize-whitespace new)})))))
  ;; TODO: ..append result code to request.
  (select session request))

(defn delete [verb]
  (db/fetch-and-modify :verb {:_id (db/object-id verb)} {} :remove? true))

(defn validate-new-verb [new]
  ;; TODO: add validation: e.g. check for duplicates, etc.
  true)

(defn new-verb-form []
  [:div {:class "create"}

   [:form {:method "post" :action "/verb/new/"}
    [:table
     [:tr
      [:th "Create a new verb"]
      [:td
       [:input {:name "verb"}]]
      ]
     ]
    ]
   ])

(defn delete-form [row]
  (let [row-id (:_id row)]
    (html
     [:form {:method "post" :action (str "/verb/" (db/primary-key row) "/delete/")}
      [:button {:onclick "submit()"} "delete"]])))

(defn show-as-rows [results]
  (if (not (empty? results))
    (str (html [:tr
                [:td ]
                [:td [:a {:href (str "/verb/" (:_id (first results))"/") } (:italian (first results))]]
                [:td (delete-form (first results)) ]
                ])
         (show-as-rows (rest results)))
    ""))





