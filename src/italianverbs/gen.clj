(ns italianverbs.gen
  (:use [hiccup core])
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [italianverbs.grammar :refer (s-present)]
   [italianverbs.html :as html]
   [italianverbs.lesson :as lesson]
   [italianverbs.morphology :refer (fo get-italian-1 get-english-1)]
   [italianverbs.over :refer (over)]
   [italianverbs.unify :refer (unifyc)]
   [italianverbs.verb :as verb]
   [italianverbs.korma :as db])) ;; TODO: provide database abstraction over mongo and other possible backing stores.

(defn tr-result [results]
  (if (not (empty? results))
    (str (html [:tr 
                [:td [:a {:href (str "/generate/" (:_id (first results))"/") } (:name (first results))]]
                [:td {:class "number"}
                 (if (and (:verbs (first results))
                          (> (.size (:verbs (first results))) 0))
                   [:a {:href (str "/lesson/" (:_id (first results))"/") } (str (.size (:verbs (first results))))]
                   "0")]])
         (tr-result (rest results)))
    ""))

(defn generate [session request]
  (html
   [:div {:class "major gen"}
    [:h2 "Generate"]
    [:table
     [:tr
      [:th "Group"]
      [:th {:class "number"} "Verbs"]
      ]

     (let [results (db/fetch :tag)]
       (tr-result results))
     ]]))

(defn generate-sentence [verb]
  (log/debug (str "generating sentence from: " verb))
  (let [verb-record (verb/lookup-by-id verb)
        italian (get-in verb-record [:italian])
        log (log/debug (str "Italian struct: " italian))
        italian-struct (if (map? italian) italian :top)
        italian (if (string? italian) italian
                    (get-in italian [:infinitive]))
        log (log/debug (str "Italian struct: " italian-struct))
        english (get-in verb-record [:english])
        english (if (string? english) english
                    (get-in english [:infinitive]))
        verb-struct
        (let [agr (ref :top)
              infl (ref :top)]
          ;; TODO: handle :fail.
          (unifyc {:italian italian-struct}
                  {:italian {:infinitive italian
                             :infl infl
                             :agr agr}
                   :english {:infinitive english
                             :infl infl
                             :agr agr}
                   :synsem {:cat :verb
                            :infl infl
                    :subcat {:1 {:agr agr
                                 :case :nom}
                             :2 '()}}}))]
    (log/debug (str "verb-struct: " verb-struct))
    (first (shuffle (over s-present (first (shuffle (list "io" "tu" "lui" "lei" "loro" "noi" "voi"))) verb-struct)))))

(defn tr-verbs [tag results times sentences]
  (let [with-numbers (map (fn [num]
                            (conj
                             {:num num}
                             (nth sentences (- num 1))))
                          (range 1 (+ times 1)))]
      (str
       (string/join ""
                    (map (fn [sent-and-verb]
                           (let [verb (:verb sent-and-verb)
                                 wtf (log/info (str "IT: " (get-italian-1 (:italian (:sentence sent-and-verb)))))
                                 wtf (log/info (str "EN: " (get-english-1 (:english (:sentence sent-and-verb)))))
                                 sentence (:sentence sent-and-verb)
                                 verb-record (verb/lookup-by-id verb)
                                 italian (get-in verb-record [:italian])
                                 italian (if (string? italian) italian
                                             (get-in italian [:infinitive]))]
                             (html [:tr
                                    [:th (:num sent-and-verb)]
                                    [:td [:a {:href (str "/verb/" (get-in verb-record [:_id])  "/" ) } italian]]
                                    [:td (get-italian-1 (:italian sentence))]
                                    [:td (get-english-1 (:english sentence))]])))
                         with-numbers)))))

(defn tr-verbs-input [tag results times sentences]
  (let [with-numbers (map (fn [num]
                            (conj
                             {:num num}
                             (nth sentences (- num 1))))
                          (range 1 (+ times 1)))]
      (str
       (string/join ""
                    (map (fn [sent-and-verb]
                           (let [verb (:verb sent-and-verb)
                                 sentence (:sentence sent-and-verb)
                                 verb-record (verb/lookup-by-id verb)
                                 italian (get-in verb-record [:italian])
                                 question {:answer "Lui dorme"}
                                 italian (if (string? italian) italian
                                             (get-in italian [:infinitive]))]
                             (html [:tr
                                    [:th (:num sent-and-verb)]
                                    [:td [:a {:href (str "/verb/" (get-in verb-record [:_id])  "/" ) } italian]]
                                    [:td [:input {:name (str "question-" (:num sent-and-verb) "-italian")
                                                  :value (get-italian-1 (:italian sentence))}]]

                                    [:td [:input {:name (str "question-" (:num sent-and-verb) "-english")
                                                  :value (get-english-1 (:english sentence))}]]])))

                         with-numbers)))))

(defn generate-from [tag]
  (let [map-of-tag (first (db/fetch :tag {:_id (db/object-id tag)}))
        tag-id tag
        tag (:name map-of-tag)
        verbs (:verbs map-of-tag)
        times 5
        sentences (take times (repeatedly #(let [verb (first (take 1 (shuffle verbs)))]
                                             {:verb verb
                                              :sentence (generate-sentence verb)})))]
    (html
     [:div {:class "major"}
      [:h2 [:a {:href "/generate/" }"Generate"] " &raquo; " [:a {:href (str "/generate/" tag-id "/")} tag]]

      [:div {:style "float:left;width:95%"}

       [:form {:method "post" :action "/test/new/"}
       
        [:table
         [:tr
          [:th]
          [:th "Verb"]
          [:th "Italian"]
          [:th "English"]
          ]
       
         (tr-verbs-input tag verbs times sentences)

         ]

       [:button "Use as new test"]

       ]
       ]



      ])))

(defn page [header body]
  (html/page header body {:uri "/generate/"}))
