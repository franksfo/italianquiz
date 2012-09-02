(ns italianverbs.html
  (:use
   [hiccup core page]
   [ring.util.codec :as codec])
  (:require
   [clojure.set :as set]
   [italianverbs.fs :as fs]
   [clojure.string :as string]
   [clojure.contrib.str-utils2 :as str-utils]))

(defn verb-row [italian]
  (html  
   [:tr 
    [:th italian] [:td
;                   (get (lex/lookup italian) :english)
"FAIL."
                   ] 
    ]))

(defn verb-table [lexicon]
  (html [:table 
	(for [verb (sort (keys lexicon))]
	     (verb-row verb))]))

(defn fs-tr [key-val-pair]
  (let [key (first key-val-pair)
        val (second key-val-pair)]
    (str "<tr"
         (cond
          (= key :comment)
          (str " class='" key ")")
          true
          (str " class='" key ")"))
         "'>" "<th>" key "</th>  <td>" val "</td></tr>")))

(defn- url-escape
 "Like clojure.core/str but escapes ',\", ..(maybe more)."
 [x]
  (-> x str (.replace "'" "%27")))

(defn google-translate [italian]
  (str
   "<a href='"
   "http://translate.google.com/"
   "#it|en|"
   (codec/url-encode italian)
   "'"   ">"
   italian "</a>"))

(defn fs [feature-structure]
  "Format a feature structure as an  HTML table."
  (if (= java.lang.String (type feature-structure)) feature-structure
      (str "<table class='fs'>"
           (if (get feature-structure :italian)
             (str "<tr><th colspan='2' class='fs'>"
                  (google-translate (get feature-structure :italian))
                  "</th></tr>"))
           (string/join " " (seq (map fs-tr
                                      (map (fn [key]
                                             (cond
                                              (= key :_id) nil
                                              (= key :children) nil
                                              ;; features whose values are nested feature structures.
                                              (or (= key :head-debug) (= key :comp-debug)
                                                  (= key :subj)(= key :obj)
                                                  (= key :det)
                                                  (= key :question)
                                                  (= key :noun)
                                                  ;(= key :article-search)
                                                  (= key :article)
                                                  ;(= key :subject)
                                                  ;(= key :object)
                                        ;(= key :verb-phrase)
                                        ;(= key :verb)
                                                  (= key :most-recent)
                                                  (= key :head)(= key :comp)
                                                  (= key :notefs) ;; the following set is used for debugging.
                                                  (= key :adjunct)(= key :iobj)
                                                  (= key :choose)(= key :root)
                                                  (contains? (get feature-structure :type-is-fs) key)
                                                  (= key :choose-comp)(= key :choose-head))
                                              (list key
                                                    (fs (get feature-structure key)))
                                              (= key :comp) nil
                                              (= key :type-is-fs) nil
                                              true
                                              (list key
                                                    (get feature-structure key))))
                                           (set/difference (set (keys feature-structure))
                                                           (set (list :italian)))))))
           "</table>")))

(defn static-page [body & [title]]
  "create a self-contained html page (for use with file:/// urls)."
  (html
   [:html
    [:head 
     [:meta  {:Content-Type "text/html; charset=UTF-8"}]
     [:title (str title
                  (if (and title (not (= title "")))
                    ": " "")
                  "imparare l'italiano")]

     (include-css "resources/public/css/style.css")
     (include-css "resources/public/css/layout.css")
     (include-css "resources/public/css/fs.css")]


    [:body
     body]]))

;; TODO: check _parent_ type: (string,symbol,list,map) should be enough to start.
(defn tablize [arg & [path serialized]]
  (let [serialized (if (nil? serialized)
                     (fs/serialize arg)
                     serialized)]
    (cond
     (= (type arg) clojure.lang.LazySeq)
     (str
      (clojure.string/join ""
                           (map tablize arg)))
     (= (type arg) clojure.lang.PersistentList)
     (str
      (clojure.string/join ""
                           (map tablize arg)))
     (= (type arg) clojure.lang.Cons)
     (str
      (clojure.string/join ""
                           (map tablize arg)))
     (and (= (type arg) clojure.lang.PersistentArrayMap)
          (= nil (get arg :children)))
     (str
      "<div class='map'><table class='map'>"
      (clojure.string/join ""
                           (map (fn [tr]
                                  (str "<tr"
                                       (cond
                                        ;; use a custom CSS class for :comment.
                                        (= (first tr) :comment)
                                        " class='comment'"
                                        ;; ..handle other keywords that need a custom CSS class..
                                        ;; default: no custom CSS class.
                                        true "")
                                       ">"
                                       "<th>"
                                       (str (first tr))
                                       "</th>"
                                       "<td>"
                                       (tablize (second tr) (concat path (list (first tr))) serialized)
                                       "</td></tr>"))
                                (into (sorted-map) arg)))
      "</table></div>")
     (= (type arg) clojure.lang.PersistentHashSet)
     (str
      "{"
      (clojure.string/join ","
                           (map (fn [member]
                                  (tablize member (concat path (list (first member))) serialized))
                                arg))
      "}")
     (= (type arg) clojure.lang.PersistentHashMap)
     (fs arg)
     (and (= (type arg) clojure.lang.PersistentArrayMap)
          (not (= nil (get arg :children))))
     (let
         [children (get arg :children)]
       (str
        "<div class='syntax'><table class='syntax'>"
        "<tr><td style='padding-left:5%;width:90%' colspan='" (count children) "'>"
        (fs arg)
        "</td></tr>"
        "<tr>"
        ;; now show syntactic children for this parent.
        (string/join " " (map (fn [child] (str "<td>"
                                               (cond (string? child)
                                                     child
                                                     (get child :children)
                                                     (tablize child) ;; path serialized
                                                     true
                                                     (fs child))
                                               "</td>")) children))
        "</tr>"
        "</table></div>"))
     (= nil arg)
     (str "<div class='atom'><i>nil</i></div>")
     (or (= (type arg)
            java.lang.String)
         (= (type arg)
            java.lang.Integer)
         (= (type arg)
            java.lang.Double)
         (= (type arg)
            clojure.lang.Keyword)
         (= (type arg)
            org.bson.types.ObjectId)
         (= (type arg)
            java.lang.Boolean))
     (str "<span class='atom'>" arg "</div>")
     (= (type arg) clojure.lang.Ref)
     (let [is-first (fs/is-first-path serialized path 0
                                      (fs/path-to-ref-index serialized path 0))]
       (str "<div class='ref'>" (fs/path-to-ref-index serialized path 0) "</div>"
            (if (= is-first true)
              (tablize @arg path serialized (merge {arg true}))
              "")))
     true
     (str "<div class='unknown'>" "<b>don't know how to format this object : (type:" (type arg) ")</b>"  arg "</div>"))))

(defn simple-fs []
  {:foo "bar"})

(defn nested-fs []
  {:foo {:bar {:baz "biff"}}})

(defn create-anchor [package test]
  (codec/url-encode (str package ":" test)))

(defn iframe [url]
  (str "<iframe src=\""  url "\"></iframe>"))

;; TODO: look at hiccup.page-helpers/doctype
(defn showdoctype [ & [type] ]
  (cond
   (= type "html5")
   "<!DOCTYPE html>"
   true ;; default is xhtml transitional (for now).
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
	\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"))

(defn myhtml5 []
  "<!DOCTYPE html>")

