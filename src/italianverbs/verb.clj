(ns italianverbs.verb
  (:use [hiccup core])
  (:require
   [clj-time.core :as t]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [italianverbs.db :as db]
   [italianverbs.morphology :as morph]
   [italianverbs.morphology :refer (normalize-whitespace)]
   ))

(declare delete-form)
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
            (let [created-at (t/now)]
              (db/insert! :verb {:created (str created-at)
                                 :updated (str created-at)
                                 :italian (normalize-whitespace new-verb)}))))))
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
        [:th "Created"]
        [:th "Changed"]
        [:th {:class "edit"} "Edit"]]
       
       (let [results (db/fetch :verb)]
         (show-as-rows results))
       ]
      (new-verb-form)
      ])))

(defn show-as-rows [results]
  (if (not (empty? results))
    (str (html [:tr
                [:td ]
                [:td [:a {:href (str "/verb/" (:_id (first results))"/") } 
                      (morph/get-italian-1 (:italian (first results)))]]
                [:td (:created (first results))]
                [:td (:updated (first results))]
                [:td {:class "edit"} (delete-form (first results)) ]
                ])
         (show-as-rows (rest results)))
    ""))

(defn lookup [verb]
  (db/fetch :verb :where {:italian verb}))

(defn lookup-by-id [id]
  (first (db/fetch :verb :where {:_id id})))

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
        (dissoc
         (dissoc
          (dissoc verb :_id)
          :updated)
         :created)


         ]

       [:button "Update"]

       ]]

     )))

(defn extra-stuff [input-form original]
  "extra stuff that gets added to every verb."
  (conj input-form
        {:synsem {:cat :verb}
         :created (:created original)
         :updated (str (t/now))}))

(defn update [verb updated]
  (let [read-from-string (read-string updated)
        original (first (db/fetch :verb :where {:_id (db/object-id verb)}))
        extra-stuff-we-add-to-every-verb (extra-stuff read-from-string original)]
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




