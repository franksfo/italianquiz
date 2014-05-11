(ns italianverbs.verb
  (:use [hiccup core])
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [italianverbs.db :as db]
   [italianverbs.morphology :refer (normalize-whitespace)]
   ))

(declare new-verb-form)
(declare select)
(declare show-as-rows)
(declare validate-new-verb)

(defn delete [verb]
  (db/fetch-and-modify :verb {:_id (db/object-id verb)} {} :remove? true))

(defn new [session request]
  (log/debug (str "/verb/new with request: " (:form-params request)))
  (if (get (:form-params request) "verb")
    (let [new-verb (get (:form-params request) "verb")]
      (if (validate-new-verb new-verb)
        ;; new-verb checks out ok: add it to DB.
        (do (log/info (str "Adding validated candidate verb: " new-verb))
            ;; TODO: add return code and append to request.
            (db/insert! :verb {:italian (normalize-whitespace new-verb)})))))
  ;; TODO: ..append result code to request.
  (select session request))

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
  (let [script "/* js goes here.. */"
        verb-id verb
        ;; TODO: handle case where verb-id's record is not in db.
        verb (first (db/fetch :verb :where {:_id (db/object-id verb-id)}))]

    (html
     [:div {:class "major verbs"}
      [:h2 "Verbs"]

      [:form {:method "post" :action "/verb/update/"}
       [:input {:name "id" 
                :type "hidden"
                :value verb-id}]

       [:textarea {:name "updated"
                   :cols "140" 
                   :rows "30"
                   }
        (dissoc verb :_id)]

       [:button "Update"]

       ]]

     )))

(defn extra-stuff [base-form]
  "extra stuff that gets added to every verb."
  (conj base-form
        {:synsem {:cat :verb}}))

(defn update [verb updated]
  (let [read-from-string (read-string updated)
        extra-stuff-we-add-to-every-verb (extra-stuff read-from-string)]
    (db/fetch-and-modify :verb 
                         {:_id (db/object-id verb)}
                         (conj (read-string updated)
                               extra-stuff-we-add-to-every-verb))))

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





