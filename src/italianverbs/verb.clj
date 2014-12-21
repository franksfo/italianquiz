(ns italianverbs.verb
  (:refer-clojure :exclude [get-in merge resolve])
  (:use [hiccup core])
  (:require
   [cemerick.friend :as friend]
   [clj-time.format :as f]
   [clj-time.core :as t]
   [clj-time.local :as l]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
   [italianverbs.auth :refer (haz-admin)]
   [italianverbs.english :as en]
   [italianverbs.html :as html]
   [italianverbs.italiano :as it]
   [italianverbs.korma :as db]
   [italianverbs.morphology :as morph]
   [italianverbs.morphology :refer (normalize-whitespace)]
   [italianverbs.unify :refer :all]

   ))

(def generate-this-many-at-once 10)

(declare delete-form)
(declare new-verb-form)
(declare select)
(declare show-as-rows)
(declare validate-new-verb)

;; see core.clj: (GET "/gen" request
(defn onload []
  (str "gen('examples',1," generate-this-many-at-once "); gen_per_verb();")) ;; javascript to be executed at page load.

(defn delete [verb]
  (db/fetch-and-modify :verb (db/object-id verb) {} true))

(defn insert-new-verb [new-verb]
  (if (validate-new-verb new-verb)
    ;; new-verb checks out ok: add it to DB.
    (do (log/info (str "Adding validated candidate verb: " new-verb))
        ;; TODO: add return code and append to request.
        (let [created-at (t/now)]
          (db/insert! :verb {:created (str created-at)
                             :updated (str created-at)
                             :italian {:infinitive (normalize-whitespace new-verb)}})))))
(defn new [session request]
  (log/debug (str "/verb/new with request: " (:form-params request)))
  (if (get (:form-params request) "verb")
    (let [new-verb (get (:form-params request) "verb")]
      (insert-new-verb new-verb)))
  ;; TODO: ..append result code to request.
  (select session request))

(declare table-of-examples)

(defn control-panel [request haz-admin]
  (let [current-size "5,436"
        desired-size "10,000"]
    (html
     [:div#generation {:class "major"}
      [:h2 "Generation"]

;      [:div
;       [:button "Update"]]
      
      [:div#vocabulary
       [:h3 "Lexicon"]

       [:div#verbs 
        [:h4 "Verbs"]

        [:table 

          [:tr
           
           [:th ""]
           [:th {:style "width:10em"} "Italian"]
           [:th {:style "width:20em"} "Example"]

;           [:th {:style "width:10em"} "Semantics"]
           [:th {:style "width:10em"} "English"]
           [:th {:style "width:20em"} "Translation"]
           [:th {:style "width:3em"} ""]
           ]

         (map (fn [lexeme]
                [:tr.lexeme
                 
                 [:td
                  [:input {:type "checkbox"} ]]

                 [:td lexeme ]
                 [:td.example
                  [:div.gen_source {:id (str "verb_" lexeme)}  [:i {:class "fa fa-spinner fa-spin"} "" ] ]]

;                 [:td.semantics {:id (str "semantics_" lexeme)} [:i {:class "fa fa-spinner fa-spin"} "" ] ]

                 [:td {:id (str "english_verb_" lexeme)}  [:i {:class "fa fa-spinner fa-spin"} "" ] ]

                 [:td {:id (str "english_translation_" lexeme)} [:i {:class "fa fa-spinner fa-spin"} "" ]  ]

                 [:td {:id (str "reload_" lexeme)} [:button {:class "fa fa-refresh"} ]]

                 ])
         
              (let [all-verbs
                    (filter (fn [lexeme]
                              (not (empty?
                                    (filter (fn [lex]
                                              (and
                                               (= :top (get-in lex [:synsem :infl]))
                                               (or true (= :bere (get-in lex [:synsem :sem :pred])))
                                               (= :verb
                                                  (get-in lex [:synsem :cat]))))
                                            (get @it/lexicon lexeme)))))
                            (sort (keys @it/lexicon)))]
                all-verbs))]]

       [:div#noun
        [:h4 "Nouns and Pronouns"]
        [:table

         (map (fn [lexeme]
                [:tr 
                 [:th [:input {:type "checkbox"}]]
                 [:td lexeme]])
              (filter (fn [lexeme]
                        (not (empty?
                              (filter (fn [lex]
                                        (= :noun
                                           (get-in lex [:synsem :cat])))
                                      (get @it/lexicon lexeme)))))
                      (sort (keys @it/lexicon))))
         ]
        ]

       [:div#dets
        [:h4 "Determiners"]
        [:table

         (map (fn [lexeme]
                [:tr 
                 [:th [:input {:type "checkbox"}]]
                 [:td lexeme]])
              (filter (fn [lexeme]
                        (not (empty?
                              (filter (fn [lex]
                                        (= :det
                                           (get-in lex [:synsem :cat])))
                                      (get @it/lexicon lexeme)))))
                      (sort (keys @it/lexicon))))
         ]
       ]
       ]

      [:div#inflections
       [:h3 "Inflections"]
       [:table

        (map (fn [infl]
               [:tr 
                [:th [:input {:type "checkbox"}]]
                [:td infl]])
             ["Condizionale"
              "Imperfetto"
              "Presente"
              "Futuro"
              "Passato Prossimo"])]
        ]


      [:div#examples
       [:h3 "Examples"] ;; see (defn onload)
                                 
         [:table
          [:tr
           [:th]
           [:th "English"]
           [:th "Italiano"]
           ]
          (table-of-examples 1 generate-this-many-at-once)
          ]
       ]

      [:div#currentsize
       [:h3 "Corpus Size" ]
       [:table
        [:tr
         [:th "Current"]
         [:td current-size]]
        [:tr
         [:th "Desired"]
         [:td [:input {:value desired-size}]]]]]

      ]
    ))) 

(defn table-of-examples [index upto]
  (if (<= index upto)
    (html
     [:tr
      [:th (str index)]
      [:td {:id (str "example_q_" index)}]
      [:td {:id (str "example_a_" index)}]]
     
     (table-of-examples (+ 1 index) generate-this-many-at-once))))

(defn select [request haz-admin]
  (let [script "/* js goes here.. */"]
    (html
     [:div {:class "major verbs"}
      [:h2 "Verbs"]

      [:table.table-striped
       [:tr
        [:script script]
        [:th ""]
        [:th "Italiano"]
        [:th "English"]
        [:th "Created"]
        [:th "Changed"]
        (if haz-admin
          [:th {:class "edit"} "Edit"])]
       
       (let [results (db/fetch :verb)]
         (show-as-rows results haz-admin))
       ]
      (if haz-admin
        (new-verb-form))
      ])))

(defn show-as-rows [results haz-admin & [i]]
  (if (not (empty? results))
    (let [i (if i i 1)]
      (str (html [:tr
                  [:th.num i]
                  [:td [:a {:href (str "/verb/" (:_id (first results))"/") } 
                        (it/get-string (first results))]]
                  [:td [:a {:href (str "/verb/" (:_id (first results))"/") } 
                        (en/get-string (:english (first results)))]]
                  [:td [:span {:class "date"}
                        (f/unparse html/short-format (:created (first results)))]]

                  [:td [:span {:class "date"}
                        (f/unparse html/short-format (:updated (first results)))]]
                  (if haz-admin
                    [:td {:class "edit"} (delete-form (first results)) ])

                  ])
           (show-as-rows (rest results) haz-admin (+ i 1))))
    ""))

(defn lookup [verb]
  (db/fetch :verb {:italian {:infinitive verb}}))

(defn lookup-by-id [id]
  (first (db/fetch :verb {:_id id})))

(defn select-one [verb haz-admin]
  (log/info (str "LOOKING UP VERB: " verb))
  (let [script "/* js goes here.. */"
        verb-id verb
        ;; TODO: handle case where verb-id's record is not in db.
        verb (first (db/fetch :verb {:_id (db/object-id verb-id)}))]

    (html
     [:div {:class "major verbs"}
      [:h2 [:a {:href "/verb/"} "Verbs"] (str " &raquo; "
                                              (let [italian
                                                    (:italian verb)]
                                                (if (map? italian)
                                                  (:infinitive (:italian verb)))))]

      [:div {:style "float:left; width:95%;margin:0.5em"}
       (html/tablize
        (reduce #(dissoc %1 %2) verb
                '(:_id :updated :created)))]

      [:div {:style "float:left; width:95%;margin:0.5em"
             :class "code"}
       (reduce #(dissoc %1 %2) verb
               '(:_id :updated :created))]

      (if haz-admin
        [:div {:style "float:left;width:95%;margin:0.5em"}
         [:form {:method "post" :action (str "/verb/" verb-id "/update/")}
          [:input {:name "id" 
                   :type "hidden"
                   :value verb-id}]

          [:textarea {:name "updated"
                      :cols "100" 
                      :rows "10"}

           (reduce #(dissoc %1 %2) verb
                   '(:_id :updated :created))]

          [:button "Update"]]])])))


(defn fix-english [input-form]
  (if (string? (:english input-form))
    (merge
     {:english {:infinitive (:english input-form)}}
     (dissoc input-form :english))
    input-form))

(defn fix-italian [input-form]
  (if (string? (:italian input-form))
    (merge
     {:italian {:infinitive (:italian input-form)}}
     (dissoc input-form :italian))
    input-form))

(defn extra-stuff [input-form original]
  "extra stuff that gets added to every verb."
  (conj
   (fix-english (fix-italian input-form))
   {:synsem {:cat :verb}
    :created (:created original)
    :updated (str (t/now))}))

(defn update [verb updated]
  (log/info (str "updating verb: " verb " with updated=" updated))
  (let [read-from-string (read-string updated)
        original (first (db/fetch :verb {:_id (db/object-id verb)}))
        extra-stuff-we-add-to-every-verb (extra-stuff read-from-string original)]
    (log/info (str "modifying with all the extra stuff: " extra-stuff-we-add-to-every-verb))
    (db/fetch-and-modify :verb 
                         (db/object-id verb)
                         (conj (read-string updated)
                               extra-stuff-we-add-to-every-verb))
    {:message "updated"}))

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

(defn routes []
  (compojure/routes
   (GET "/" request
        (let [do-generation (fn []
                              {:body (html/page 
                                      "Generation" 
                                      (control-panel request
                                                     (haz-admin))
                                      request
                                      {:css "/css/settings.css"
                                       :js "/js/gen.js"
                                       :onload (onload)})
                               :status 200
                               :headers {"Content-Type" "text/html;charset=utf-8"}})]
                              
          (if false ;; TODO: define config variable workstation-mode.
            (friend/authorize #{::admin} do-generation)
            ;; turn off security for workstation dev
            (do-generation))))))
   
