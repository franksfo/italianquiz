(ns italianverbs.gen
  (:use [hiccup core])
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [italianverbs.grammar :refer (s-present)]
   [italianverbs.html :as html]
   [italianverbs.lesson :as lesson]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.over :refer (over)]
   [somnium.congomongo :as db])) ;; TODO: provide database abstraction over mongo and other possible backing stores.

(defn tr-result [results]
  (if (not (empty? results))
    (str (html [:tr 
                [:td [:a {:href (str "/generate/" (:_id (first results))"/") } (:name (first results))]]
                [:td {:class "number"}
                 (if (and (:verbs (first results))
                          (> (.size (:verbs (first results))) 0))
                   [:a {:href (str "/generate/" (:_id (first results))"/") } (str (.size (:verbs (first results))))]
                   "0")]])
         (tr-result (rest results)))
    ""))

(defn generate [session request]
  (html
   [:div {:class "major gen"}
    [:h2 "Generate"]
    [:table
     [:tr
      [:th "Tag"]
      [:th {:class "number"} "Verbs"]
      ]

     (let [results (db/fetch :tag)]
       (tr-result results))
     ]]))

(defn generate-sentence [verb]
  (log/info (str "generating sentence from: " verb))
  (let [verb-struct
        (let [agr (ref :top)
              infl (ref :top)]
          {:italian {:infinitive verb
                     :infl infl
                     :agr agr}
           :synsem {:cat :verb
                    :infl infl
                    :subcat {:1 {:agr agr
                                 :case :nom}
                             :2 '()}}})]
    (log/info (str "verb-struct: " verb-struct))
    (fo (first (over s-present (first (shuffle (list "io" "tu" "lui" "lei" "loro" "noi" "voi"))) verb-struct)))))

(defn tr-verbs [tag results times]
  (if (not (empty? results))
    (let [verb (first results)
          sentences (take times (repeatedly #(generate-sentence verb)))]
      (str
       (string/join ""
                    (map (fn [sentence]
                           (html [:tr
                                  [:td verb]
                                  [:td sentence]]))
                         sentences))
       (tr-verbs tag (rest results) times)))
    ""))

(defn generate-from [tag]
  (let [map-of-tag (first (db/fetch :tag :where {:_id (db/object-id tag)}))
        tag (:name map-of-tag)
        verbs (:verbs map-of-tag)]
    (html
     [:div {:class "major"}
      [:h2 (str "Generate" " &raquo; " tag)]

      [:table
       [:tr
        [:th "Verb"]
        [:th "Example"]
        ]
       
       (tr-verbs tag verbs 5)

       ]




      ])))

(defn page [header body]
  (html/page header body {:uri "/generate/"}))
