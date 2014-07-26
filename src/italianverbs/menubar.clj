(ns italianverbs.menubar
  (:use
   [hiccup core page]
   [ring.util.codec :as codec])
  (:require
   [clojure.tools.logging :as log]))

(defn- menuitem [ {selected? :selected?
                   show? :show?
                   current-url :current-url
                   text :text
                   url-for-this-item :url-for-this-item
                   requires-admin :requires-admin
                   requires-authentication :requires-authentication
                   haz-admin :haz-admin
                   haz-authentication :haz-authentication}]
  (if show?
    [:div
     (if (or selected?
             (= current-url url-for-this-item))
       {:class "selected"})
     [:a {:href url-for-this-item} text]]))

(defn menubar [session-row current-url haz-authentication & [suffixes]]
  (let [roles (:roles haz-authentication)
        haz-admin? (not (nil? (:italianverbs.core/admin roles)))]

    (log/info (str "Drawing menubar with current-url=" current-url))
    (log/info (str "Menubar with suffixes: " suffixes))
    (html
     [:div {:class "menubar major"}

      (menuitem {:selected?
                 (or (and (not (nil? current-url))
                          (re-find #"/login" current-url))
                     (= current-url "/login")
                     (and (not (nil? current-url))
                          (re-find #"/about" current-url)))
                 :show? true
                 :current-url current-url 
                 :text "About" 
                 :url-for-this-item "/about"
                 :requires-admin false
                 :requires-authentication false})

      (menuitem {:selected?
                 (and (not (nil? current-url))
                      (re-find #"/class" current-url))
                 :current-url current-url
                 :text "Classes" :url-for-this-item (str "/class" (if (get suffixes :class)
                                                                    (get suffixes :class)))
                 :show? haz-admin?})

      (menuitem {:selected?
                 (and (not (nil? current-url))
                      (re-find #"/class" current-url))
                 :current-url current-url
                 :text "My Classes"
                 :url-for-this-item (str "/class/my" (if (get suffixes :class)
                                                       (get suffixes :class)))
                 :show? (and haz-authentication (not haz-admin?))})

      (menuitem {:show? haz-admin?
                 :selected? (and (not (nil? current-url))
                                 (re-find #"/student" current-url))
                 :current-url current-url
                 :text "Students"
                 :url-for-this-item (str "/student" (if (get suffixes :student)
                                                      (get suffixes :student)))})

      (menuitem {:show? true
                 :selected? (and (not (nil? current-url))
                                 (re-find #"/verb" current-url))
                 :current-url current-url
                 :text "Verbs" :url-for-this-item "/verb/"})

      (menuitem {:show? haz-authentication
                 :selected? (and (not (nil? current-url))
                                 (re-find #"/lession" current-url))
                 :current-url current-url
                 :text "Groups" :url-for-this-item "/lesson"})

      (menuitem {:show? haz-admin?
                 :selected? (and (not (nil? current-url))
                                 (re-find #"/test" current-url))
                 :current-url current-url
                 :text "Test" :url-for-this-item "/test"})

      (menuitem {:show? haz-authentication
                 :selected? (and (not (nil? current-url))
                                 (re-find #"/workbook" current-url))
                 :current-url current-url
                 :text "Workbook" :url-for-this-item "/workbook"})

      (menuitem {:show? haz-authentication
                 :selected? (and (not (nil? current-url))
                                 (re-find #"/quiz" current-url))
                 :current-url current-url
                 :text "Quiz" :url-for-this-item "/quiz"})


    ])))
