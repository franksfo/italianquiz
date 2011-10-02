(ns italianverbs.html
  (:use
   [hiccup core page-helpers]
   [somnium.congomongo]
   [ring.util.codec :as codec])
  (:require
   [clojure.set :as set]
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

(defn google-translate [italian]
  (str
   "<a href='"
   "http://translate.google.com/"
   "#it|en|"
   ;; TODO: URL:encode the following:
   italian
   "'"   ">"
   italian "</a>"))

(defn fs [feature-structure]
  "Format a feature structure as an  HTML table."
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
                                        ; uncomment for debugging.
                                          (= key :fn) nil
                                          (= key :head) nil
                                          ;; featues whose values are nested feature structures.
                                          (or (= key :head-debug) (= key :comp-debug)
                                              (= key :subj)(= key :obj)
                                              (= key :det)
;                                              (= key :head)(= key :comp)
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
       "</table>"))

;; TODO: check _parent_ type: (string,symbol,list,map) should be enough to start.
(defn tablize [arg]
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
                                     (tablize (second tr))
                                     "</td></tr>"))
                              arg))
    "</table></div>")
   (= (type arg) clojure.lang.PersistentHashSet)
   (str
    "{"
    (clojure.string/join ","
                         (map (fn [member]
                                (tablize member))
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
                                                   (tablize child)
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
   true
   (str "<div class='unknown'>" "<b>don't know how to format this object : (type:" (type arg) ")</b>"  arg "</div>")))

(defn simple-fs []
  {:foo "bar"})

(defn nested-fs []
  {:foo {:bar {:baz "biff"}}})

(defn create-anchor [package test]
  (codec/url-encode (str package ":" test)))

(defn iframe [url]
  (str "<iframe src=\""  url "\"></iframe>"))

(defn test []
  "this should contain a list of all the tests for the html package. each test can
  return a map or a list or a function. a function will be applied against an
  empty argument list"
  (list
   {:comment "a simple feature structure"
    :test (simple-fs)}
   {:comment "showing nesting: where a value is itself a feature structure."
    :test (nested-fs)}
   {:comment "anchor"
    :test (create-anchor "foo" "bar 'baz'")}
   {:comment "iframe"
    :test (iframe "/guess/")}))


