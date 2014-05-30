(ns italianverbs.gen
  (:use [hiccup core])
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [formative.core :as f]
   [formative.parse :as fp]
   [hiccup.page :as h]
   [italianverbs.grammar :refer (s-present)]
   [italianverbs.html :as html]
   [italianverbs.lesson :as lesson]
   [italianverbs.morphology :refer (fo get-italian-1 get-english-1)]
   [italianverbs.over :refer (over)]
   [italianverbs.unify :refer (unifyc)]
   [italianverbs.verb :as verb]
   [italianverbs.korma :as db])) ;; TODO: provide database abstraction over mongo and other possible backing stores.

(defn validate-upload [values]
  (let [filename (not-empty (get-in values [:picture "filename"]))]
    (when (and filename (not (re-find #"\.jpg$" filename)))
      {:keys [:picture] :msg "JPG files only"})))

;; starting with https://github.com/jkk/formative-demo/blob/master/src/formative_demo/web.clj#L14
;; and removing stuff.
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
   :action "/test/newdebug"
   :validator validate-upload})


(defn show-demo-form [params & {:keys [problems]}]
  (let [now (java.util.Date.)
        defaults {:spam true
                  :date now
                  :time now}]
    (f/render-form (assoc demo-form
                     :values (merge defaults params)
                     :problems problems))))


(defn layout [opts & body]
  (h/html5
    [:head
     [:title "Formative Demo"]
     [:style
      "body { margin: 2em; }"
      "h1 { margin-bottom: 20px; }"
      ".form-horizontal .field-group { margin-bottom: 10px; }"
      ".well form, .well .field-group { margin-bottom: 0; }"
      ".nav-tabs { margin-top: 20px; margin-bottom: 20px; }"
      ".heading-row h3 { margin-bottom: 5px; }"
      ".form-table { width: 100%; }"
      ".form-table th { text-align: left; }"
      ".form-table h3 { border-bottom: 1px solid #ddd; }"
      ".form-table .label-cell { vertical-align: top; text-align: right; padding-right: 10px; padding-top: 10px; }"
      ".form-table td { vertical-align: top; padding-top: 5px; }"
      ".form-table .checkbox-row label { display: inline; margin-left: 5px; }"
      ".form-table .checkbox-row .input-shell { margin-bottom: 10px; }"
      ".form-table .submit-row th, .form-table .submit-row td { padding: 30px 0; }"
      ".form-table .problem th, .form-table .problem td { color: #b94a48; background: #fee; }"]]
    [:body {:onload "prettyPrint()"}
     body]
    (h/include-js "//google-code-prettify.googlecode.com/svn/trunk/src/prettify.js")
    (h/include-js "//google-code-prettify.googlecode.com/svn/trunk/src/lang-clj.js")
    #_(h/include-js "/js/goog/base.js")
    (h/include-js "/js/main.js")
    #_"<script type=\"text/javascript\">goog.require('formative_demo.main');</script>"))

(defn submit-demo-form [params]
  (fp/with-fallback #(show-demo-form params :problems %)
    (let [values (fp/parse-params demo-form params)]
      (layout
        [:h1 "Thank you!"]
        [:pre (prn-str values)]))))


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
                                              :sentence (generate-sentence verb)})))
        
        renderer :bootstrap-horizontal
        now (java.util.Date.)
        defaults {:spam true
                  :date now
                  :time now}

        params {}
        problems {}
        
        ]
    (html
     [:div {:class "major"}
      [:h2 [:a {:href "/generate/" }"Generate"] " &raquo; " [:a {:href (str "/generate/" tag-id "/")} tag]]

       (f/render-form (assoc demo-form
                             :renderer renderer
                             :values (merge defaults params)
                             :problems problems))

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

        [:div {:style "width:100%;float:left"}
         [:input {:name "name" :value (str "test:" tag)}]

         [:button "Use as new test"]]
        
       ]
       ]



      ])))

(defn page [header body]
  (html/page header body {:uri "/generate/"}))
