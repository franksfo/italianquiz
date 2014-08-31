(ns italianverbs.html
  (:use
   [hiccup core page]
   [ring.util.codec :as codec])
  (:require
   [cemerick.friend :as friend]
   [clj-time.coerce :as c]
   [clj-time.core :as t]
   [clj-time.format :as f]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [hiccup.element :as e]
   [hiccup.page :as h]
   [italianverbs.menubar :as menubar]
   [italianverbs.session :as session]
   [italianverbs.unify :as fs]))

(defn verb-row [italian]
  (html
   [:tr
    [:th italian] [:td "FAIL." ]
    ]))

(defn verb-table [lexicon]
  (html [:table
	(for [verb (sort (keys lexicon))]
	     (verb-row verb))]))

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

     (include-css "resources/public/css/fs.css")
     (include-css "resources/public/css/layout.css")
     (include-css "resources/public/css/quiz.css")

     (h/include-css "resources/public/css/bootstrap.min.css")
     (h/include-css "resources/public/css/prettify.css")

     (include-css "resources/public/css/style.css")

     (include-css "http://maxcdn.bootstrapcdn.com/font-awesome/4.1.0/css/font-awesome.min.css")

     ]


    [:body
     body]]))

(defn enumerate-serialized-paths [paths n]
  (if paths
    (let [path (first paths)]
      (if path
        (conj
         {(keyword (str "path" n))
          (str (string/join " " path))}
         (enumerate-serialized-paths (rest paths) (+ 1 n)))
        {}))
    {}))

(defn enumerate-serialized [arg n]
  (if arg
    (let [elem (first arg)]
      (if elem
        (conj
         {(keyword (str "p" n))
          {:paths (if (not (nil? (first elem)))
                    (enumerate-serialized-paths (first elem) 0))
           :skel (second elem)}}
         (enumerate-serialized (rest arg) (+ 1 n)))
        {}))
    {}))

(defn tablize-ser [arg]
  (enumerate-serialized arg 0))

(defn head-initial? [arg path]
  (let [retval
  (and
   (map? arg)
   
   (not (= :subcat (last path)))
   (not (= :italian (last path)))
   (not (= :english (last path)))
   
   (not (= :none (:head arg :none)))
   (not (= :none (:comp arg :none)))
   (= :none (:1 arg :none))
   (= :none (:2 arg :none))
   
   (or
    (and
     (not (= :none (fs/get-in arg '(:head :italian) :none)))
     (not (= :none (fs/get-in arg '(:comp :italian) :none))))
    (and
     (not (= :none (fs/get-in arg '(:head :english) :none)))
     (not (= :none (fs/get-in arg '(:comp :english) :none)))))
   
   (or
    (and (fs/ref= arg '(:head :italian) '(:italian :a))
         (fs/ref= arg '(:comp :italian) '(:italian :b)))
    (and (fs/ref= arg '(:head :english) '(:english :a))
         (fs/ref= arg '(:comp :english) '(:english :b)))))]
    retval))

(defn head-final? [arg path]
  (let [retval
  (and
   (map? arg)
   
   (not (= :subcat (last path)))
   (not (= :italian (last path)))
   (not (= :english (last path)))
   
   (not (= :none (:head arg :none)))
   (not (= :none (:comp arg :none)))
   (= :none (:1 arg :none))
   (= :none (:2 arg :none))
   
   (or
    (and
     (not (= :none (fs/get-in arg '(:head :italian) :none)))
     (not (= :none (fs/get-in arg '(:comp :italian) :none))))
    (and
     (not (= :none (fs/get-in arg '(:head :english) :none)))
     (not (= :none (fs/get-in arg '(:comp :english) :none)))))
   
   (or
    (and (fs/ref= arg '(:head :italian) '(:italian :b))
         (fs/ref= arg '(:comp :italian) '(:italian :a)))
    (and (fs/ref= arg '(:head :english) '(:english :b))
         (fs/ref= arg '(:comp :english) '(:english :a)))))]
    retval))

;; TODO: use multimethod based on arg's type.
(defn tablize [arg & [path serialized opts]]
 ;; set defaults.
  ;; (TODO: in which contexts are we passing an already-serialized arg?)
  ;; if not already serialized, then serialize:
  (let [serialized (if (nil? serialized)
                     (fs/serialize arg)
                     serialized) ;; .. if already serialized, use that.
        opts (if (nil? opts)
               {:as-tree true})]
    (cond
     (nil? arg) (str "<i>nil</i>")
     (= arg '()) (str "<i>&lt;&nbsp;&gt;</i>")
     (= (type arg) clojure.lang.LazySeq)
     (str
      ;; TODO: pass along additional args (path,serialized,opts)
      ;; to recursive tablize call. (TODO applies to all 3 of the
      ;; following conditional disjuncts).
      (clojure.string/join ""
                           (map (fn [each-arg]
                                  (tablize each-arg path (fs/serialize each-arg) opts))
                                (seq arg))))
     (set? arg)
     (reduce #'str
             (concat (list "<div class='set'><div class='delimiter'>{</div><div class='member'>")
                     (string/join "</div><div class='delimiter'>,</div><div class='member'>"
                                  (map (fn [each]
                                         (tablize each path (fs/serialize each) opts))
                                       (seq arg)))
                     (list "</div><div class='delimiter'>}</div></div>")))

     (or (list? arg)
         (= (type arg) clojure.lang.Cons))
     (str
      (clojure.string/join ""
                           (map (fn [each]
                                  (tablize each path (fs/serialize each) opts))
                                arg)))


     ;; displaying a phrase structure tree (2 children)
     ;; Head-initial (H C)
     (head-initial? arg path)
     (str
      "<div class='phrase'>"
      "  <table class='phrase'>"
      "    <tr>"
      "      <td class='parent2child'>&nbsp;</td><td class='parent2child parent' colspan='5'>"
      (tablize (dissoc (dissoc arg :head) :comp) path serialized opts)
      "      </td><td class='parent2child'>&nbsp;</td>"
      "    </tr>"
      "    <tr>"
      "      <td class='ref'>"

      (if (= (type (:head arg)) clojure.lang.Ref)
        (str
         "     <div class='ref'>"
         (fs/path-to-ref-index serialized (concat path '(:head)) 0)
         "     </div>"))

      "      </td>"
      "      <td class='hc'>H</td><td>"

      (tablize (if (= (type (:head arg)) clojure.lang.Ref)
                 @(:head arg)
                 (:head arg))
               (concat path '(:head)) serialized opts)
      "      </td>"
      "      <td class='ref'>"

      (if (= (type (:comp arg)) clojure.lang.Ref)
        (str
         "    <div class='ref'>"
         (fs/path-to-ref-index serialized (concat path '(:comp)) 0)
         "    </div>"))

      "      </td>"
      "      <td class='hc'>C</td><td>"
      (tablize (if (= (type (:comp arg)) clojure.lang.Ref)
                 @(:comp arg)
                 (:comp arg))
               (concat path '(:comp)) serialized opts)
      "      </td><td>&nbsp;</td>"
      "    </tr>"
      "  </table>"
      "</div>")


     ;; displaying a phrase structure tree (2 children)
     ;; Head-final (C H)
     (head-final? arg path)
     (str
      "<div class='phrase'>"
      "  <table class='phrase'>"
      "    <tr>"
      "      <td class='parent2child'>&nbsp;</td><td class='parent2child parent' colspan='5'>"
      (tablize (dissoc (dissoc arg :head) :comp) path serialized opts)
      "      </td><td class='parent2child'>&nbsp;</td>"
      "    </tr>"
      "    <tr>"
      "      <td class='ref'>"
      (if (= (type (:comp arg)) clojure.lang.Ref)
        (str
         "     <div class='ref'>"
         (fs/path-to-ref-index serialized (concat path '(:comp)) 0)
         "     </div>"))
      "      </td>"
      "      <td class='hc'>C</td><td>"
      (tablize (if (= (type (:comp arg)) clojure.lang.Ref)
                 @(:comp arg)
                 (:comp arg))
               (concat path '(:comp)) serialized opts)
      "      </td>"
      "      <td class='ref'>"
      (if (= (type (:head arg)) clojure.lang.Ref)
        (str
         "    <div class='ref'>"
         (fs/path-to-ref-index serialized (concat path '(:head)) 0)
         "    </div>"))
      "      </td>"
      "      <td class='hc'>H</td><td>"
      (tablize (if (= (type (:head arg)) clojure.lang.Ref)
                 @(:head arg)
                 (:head arg))
               (concat path '(:head)) serialized opts)
      "      </td><td>&nbsp;</td>"
      "    </tr>"
      "  </table>"
      "</div>")


     ;; displaying a phrase structure tree (2 children)
     (and
      (map? arg)

      (not (= :subcat (last path)))
      (not (= :italian (last path)))

      ;; display :extends properly (i.e. not a tree).
      ;; :extends will have features :a,:b,:c,..
      (not (= :a (last path)))
      (not (= :b (last path)))
      (not (= :c (last path)))
      (not (= :d (last path)))
      (not (= :e (last path)))
      (not (= :f (last path)))
      (not (= :g (last path)))

      (not (= :english (last path)))
      (not (= :none (:1 arg :none)))
      (not (= :none (:2 arg :none))))

     (str
      "<div class='phrase'>"
      "  <table class='phrase'>"
      "    <tr>"
      "      <td class='parent2child'>&nbsp;</td><td class='parent2child parent' colspan='3'>"
      (tablize (dissoc (dissoc arg :1) :2) path serialized opts)
      "      </td><td class='parent2child'>&nbsp;</td>"
      "    </tr>"
      "    <tr>"
      "      <td class='ref'>"
      (if (= (type (:1 arg)) clojure.lang.Ref)
        (str
         "     <div class='ref'>"
         (fs/path-to-ref-index serialized (concat path '(:1)) 0)
         "     </div>"))
      "      </td>"
      "      <td>"
      (tablize (if (= (type (:1 arg)) clojure.lang.Ref)
                 @(:1 arg)
                 (:1 arg))
               (concat path '(:1)) serialized opts)
      "      </td>"
      "      <td class='ref'>"
      (if (= (type (:2 arg)) clojure.lang.Ref)
        (str
         "    <div class='ref'>"
         (fs/path-to-ref-index serialized (concat path '(:2)) 0)
         "    </div>"))
         "      </td>"
         "      <td>"
      (tablize (if (= (type (:2 arg)) clojure.lang.Ref)
                 @(:2 arg)
                 (:2 arg))
               (concat path '(:2)) serialized opts)
      "      </td><td>&nbsp;</td>"
      "    </tr>"
      "  </table>"
      "</div>")

    ;; displaying a phrase structure tree (1 child)
     (and
      (or true (not (nil? opts)))
      (or true (= true (:as-tree opts)))
      (or (= (type arg) clojure.lang.PersistentArrayMap)
          (= (type arg) clojure.lang.PersistentHashMap)
          (= (type arg) clojure.lang.PersistentTreeMap))
      (not (= :subcat (last path)))

      (not (= :a (last path)))
      (not (= :b (last path)))
      (not (= :c (last path)))


      (not (= :none (:1 arg :none)))
      (= :none (:2 arg :none)))
     (str
      "<div class='phrase'>"
      "  <table class='phrase'>"
      "    <tr>"
      "      <td colspan="2" class='parent1child'>" (tablize (dissoc (dissoc arg :1) :2) path serialized {:as-tree false}) "</td>"
      "    </tr>"
      "    <tr>"
      "      <td>"
      (if (= (type (:1 arg)) clojure.lang.Ref)
        (str
         "    <div class='ref'>"
         (fs/path-to-ref-index serialized (concat path '(:1)) 0)
         "    </div>"))
         "      </td>"
         "      <td>"
      (tablize (if (= (type (:1 arg)) clojure.lang.Ref)
                 @(:1 arg) (:1 arg))
               (concat path (list :1))
               serialized opts)
      "      </td>"
      "    </tr>"
      "  </table>"
      "</div>")

     ;; displaying a feature structure.
     (or (= (type arg) clojure.lang.PersistentArrayMap)
         (= (type arg) clojure.lang.PersistentHashMap)
         (= (type arg) clojure.lang.PersistentTreeMap))
     (str
      "<div class='map'>"
      (if (:header arg) (str "<h2>" (:header arg) "</h2>"))
      "  <table class='map'>"
      (clojure.string/join
       ""
       (map
        (fn [tr]
          (str
           "<tr"
           (cond
            ;; TODO: more general method to allow passage of css info from other parts of code:
            (= (first tr) :paths)
            " style='white-space:nowrap;'"
            ;; use a custom CSS class for :comment.
            (= (first tr) :comment)
            " class='comment'"
            ;; use a custom CSS class for :header (i.e. hide it with display:none)
            (= (first tr) :header)
            " class='hide' style='display:none'"
            ;; ..handle other keywords that need a custom CSS class..
            ;; default: no custom CSS class.
            true "")
           ">"
           "   <th>"
           (str (first tr))
           "   </th>"
           (if (= (type (second tr)) clojure.lang.Ref)
             (str
              "<td class='ref'>"
              ;; show ref id for debugging if desired:
              (if false (str
                         "(" (second tr) ")"
                         "[ " (type @(second tr)) " ]"))
              "  <div class='ref'>"
              (fs/path-to-ref-index serialized (concat path (list (first tr))) 0)
              "  </div>"
              "</td>"
              "<td class='ref'>"
              )
             " <td class='ref' colspan='2'>")
           (tablize (second tr)
                    ;; set 'path' param for recursive call to tablize.
                    ;; Path' = Path . current_feature
                    (concat path (list (first tr)))
                    serialized
                    {:as-tree false}
                    )
           "   </td>"
           "</tr>"))
        ;; sorts the argument list in _arg__ by key name. remove :comment-plaintext and :extend.
        (remove #(= (first %) :comment-plaintext)
                (remove #(or (= (first %) :comment-plaintext)
                             (= (first %) :extend)
                             (= (first %) :serialized))
                        (into (sorted-map) arg)))
        ))
      "  </table>"
      "</div>")
     (= (type arg) clojure.lang.PersistentHashSet)
     (str
      "{"
      (clojure.string/join ","
                           (map (fn [member]
                                  (tablize member
                                           ;; set 'path' param for recursive call to tablize.
                                           ;; Path' = Path . current_feature
                                           (concat path (list (first member)))
                                           serialized
                                           {:as-tree false}
                                           ))
                                arg))
      "}")
     (= nil arg)
     (str "<div class='atom'><i>nil</i></div>")
     (= (type arg)
        java.lang.String)
     (str "<span class='string'>" arg "</span>")
     (= (type arg)
        java.lang.Long)
     (str "<span class='atom'>" arg "</span>")
     (= arg :fail)
     (str "<span class='keyword fail'>" arg "</span>")
     (= (type arg)
        clojure.lang.Keyword)
     (str "<span class='keyword'>" arg "</span>")
     (= (type arg)
        java.lang.Boolean)
     (str "<span class='boolean'>" arg "</span>")

     (or
         (= (type arg)
            java.lang.Integer)
         (= (type arg)
            java.lang.Double)
         (= (type arg)
            org.bson.types.ObjectId))
     (str "<span class='atom'>" arg "</span>")

     (and (= (type arg) clojure.lang.Ref)
          (= @arg nil))
     (str "NIL.")

     (symbol? arg)
     (str "<i>" arg "</i>")

     (= (type arg) clojure.lang.Ref)
     (let [is-first (fs/is-first-path serialized path 0
                                      (fs/path-to-ref-index serialized path 0))]
       (str (if (and (or (= (last path) :subcat)
                         (= is-first true))
                     (or false (not (= (last path) :head)))
                     (or false (not (= (last path) :comp))))
              (tablize @arg path serialized
                       (merge opts {:as-tree false})))))

     (fn? arg)
     "&lambda;"

     true
     (str "<div class='unknown'>" "<b>don't know how to tablize this object : (type:" (type arg) "</b>;value=<b>"  arg "</b>)</div>"))))

(defn tablize-key-row [key val]
  (str
   "<tr>"
   (str "<th class='complex'>" (tablize key) "</th>")
   (str "<td class='complex'>"
        "<table class='list'>"
        "<tr><td>"
        (string/join
         "</td><td class='each'>"
                                        ;"</td></tr><tr><td class='each'>"
                     (map (fn [each-val]
                            (tablize each-val))
                          val))
        "</td></tr>"
        "</table>"
        "</td>")
   "</tr>"))

;; TODO: fold into tablize
(defn tablize-with-complex-keys [arg]
  (let [keys (keys arg)
        tablized-keys (map (fn [key]
                             (tablize key))
                           keys)]
    (str
     "<table class='complex'>"
     (string/join ""
           (map (fn [key]
                  (tablize-key-row key (get arg key)))
                keys))
     "</table>")))

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

(defn welcome [username]
  (html
   [:div
    (if username
      [:div "Benvenuti, " username "."
       [:a {:href "/session/clear/"} "Logout"]
       ]
      [:a {:href "/session/set/"} "Login"]
      )]))

(defn request-to-suffixes [request]
  "menubar uses this to make the menubar links context-specific.."
  ;; ...e.g. if you are looking at a particular group, 
  ;; the 'generate' link should have that group id so that if you
  ;;  click the link, you will generate with that group"
  (let [route-params (:route-params request)]
    (log/debug (str "req-to-suff params: " route-params))
    {:generate (if (and route-params (:tag route-params))
                 (str (:tag route-params) "/"))}))

(defn powered-by [name link]
  (html
   [:div {:class "poweredby"}
    [:a {:href link}
     name ]]))

(defn footer []
  (html
   [:div {:class "poweredbox major"}
    [:h2 [:i "imparare l'Italiano"] " è alimentato da .."]
    [:table
     [:tr
      [:td {:colspan "5"}
       [:div {:class "groupinglabel"}
        (powered-by "italianquiz" "https://github.com/ekoontz/italianquiz")]

       [:table {:class "italianquiz"}
        [:tr
         [:td {:colspan "3"}
          [:div {:class "groupinglabel2"}
           [:span {:class "label"} "app layer"]
           [:table {:class "groupinglabel2"}
            [:tr
             [:td {:colspan "1"}
              (powered-by "quiz" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/quiz.clj")]
             [:td {:colspan "1"}
              (powered-by "workbook" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/workbook.clj")]
             [:td {:colspan "1"}
              "others.."]]]]]]

         [:tr
          [:td {:colspan "3"}
           (powered-by "generate" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/generate.clj")
           ]
          ]
         [:tr
          [:td {:colspan "3"}
           (powered-by "forest" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/forest.clj")
           ]
          ]
         [:tr
          [:td
           (powered-by "grammar" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/grammar.clj")]
          [:td {:rowspan "1"}
           (powered-by "lexicon" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/lexicon.clj")
           ]
          [:td {:rowspan "1"}
           (powered-by "morphology" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/morphology.clj")
           ]
          ]

         [:tr
          [:td {:colspan "3"}
           (powered-by "unify" "https://github.com/ekoontz/italianquiz/tree/master/src/italianverbs/unify.clj")
           ]
          ]

         ]]]
     [:tr
      [:td {:colspan "2"}
       (powered-by "compojure" "https://github.com/weavejester/compojure")]
      [:td {:rowspan "2" :colspan "1"}
       (powered-by "clojail" "https://github.com/flatland/clojail")]
      [:td {:rowspan "2" :colspan "2"}
       (powered-by "korma" "https://sqlkorma.com")]]
      [:tr
       [:td {:colspan "2"}
        (powered-by "ring" "https://github.com/mmcgrana/ring")]]
     [:tr
      [:td
       (powered-by "jetty" "http://jetty.codehaus.org/jetty/")]
      [:td {:colspan "3"}
       (powered-by "clojure" "http://clojure.org/")]
      [:td {:colspan "1"}
       (powered-by "postgres" "http://www.postgresql.org/")
       ]]]]))

(defn about []
   [:div {:class "major"} [:h2 "Welcome to Verbcoach."]

    [:div
     [:p
     
      "This site helps to learn Latin-based languages by \"coaching\" to conjugate verbs."

      ]]
    ])


(defn pretty-head [title]
  [:head 
   [:meta {:http-equiv "Content-Type" :content "text/html; charset=utf-8"}]
   [:link {:href "/webjars/css/normalize.css" :rel "stylesheet" :type "text/css"}]
   (include-css "/css/style.css")
   (include-css "/css/layout.css")
   (include-css "/css/fs.css")
   (include-css "/css/tag.css")
   (include-css "/css/quiz.css")
   (include-css "/css/fade.css")
   (include-css "/css/test.css")

   (h/include-css "/css/bootstrap.min.css")
   (h/include-css "/css/prettify.css")
   
   (include-css "http://maxcdn.bootstrapcdn.com/font-awesome/4.1.0/css/font-awesome.min.css")
   (include-css "/css/game.css")

   [:style {:type "text/css"} "ul { padding-left: 2em }"]
   
   [:script {:src "/webjars/js/foundation.min.js" :type "text/javascript"}]
   [:script {:type "text/javascript" :src "/js/jquery-1.6.4.min.js"}]
   [:script {:type "text/javascript" :src "/js/autogrow.js"}]

   ;; TODO: move this [:script ] to quiz.clj somehow: should keep quiz.js stuff with quiz.clj, search.js stuff with search.clj,etc.
   [:script {:type "text/javascript" :src "/js/quiz.js"}]

   [:script {:type "text/javascript" :src "/js/workbook.js"}]
   [:script {:type "text/javascript" :src "/js/search.js"}]
   
   (include-js "/js/prettify.js")
   (include-js "/js/lang-clj.js")
   (include-js "/js/main.js")
   (include-js "/js/d3.v2.min.js")
   (include-js "/js/game.js")

    ; enable this 'reset.css' at some point.
    ;  (include-css "/italian/css/reset.css")

   [:title (str title
                (if (and title (not (= title "")))
                  ": " "")
                "Verbcoach")]])

(defn pretty-body
  [& content]
  [:body
   {:onload "start_game();"}
   (into [:div {:class "columns small-12"}] content)])

(defn logged-in-content [req identity]
  [:div {:class "login major"}

    [:table {:style "border:0px"}
     [:tr
      [:th
       (str "User")]
      [:td
       (:current identity)]
      [:th
       (str "Roles")]
      [:td {:style "white-space:nowrap"}
       (string/join ","
                    (map #(string/replace
                           (string/replace (string/replace 
                                            (str %) 
                                            ":italianverbs.core/" "")
                                           #"^user" "student")
                           #"^admin" "teacher")


                         (-> identity friend/current-authentication :roles)))]
      [:td {:style "float:right;white-space:nowrap"} (e/link-to (str "/" "logout") "Log out") ""]]]])

(defn get-loggedin-user-roles []
  (-> identity friend/current-authentication :roles))

(def login-form
  [:div {:class "login major"}
   [:form {:method "POST" :action "/login"}
    [:table
     [:tr
      [:th "User"][:td [:input {:type "text" :name "username" :size "10"}]]
      [:th "Password"][:td [:input {:type "password" :name "password" :size "10"}]]
      [:td [:input {:type "submit" :class "button" :value "Login"}]]]]]])

(defn page-body [content req & [ title ]]
  (let [title (if title title "default page title")]
    (h/html5
     (pretty-head title)
     (pretty-body

      (if-let [identity (friend/identity req)]
        (logged-in-content req identity)
        login-form)

       content))))
      
;      [:div {:style "float:left;width:95%;border:1px dashed blue;"}
;       [:ul 
;        [:li (e/link-to (str "/" "role-user") "Requires the `user` role")]
;        [:li (e/link-to (str "/" "role-admin") "Requires the `admin` role")]
;        [:li (e/link-to (str "/" "requires-authentication")
;                        "Requires any authentication, no specific role requirement")]]]))))


(defn page [title & [content request onload]]
  (let [haz-admin (not (nil? (:italianverbs.core/admin (:roles (friend/current-authentication)))))]
    (page-body 
     (html
      [:div#top
       (menubar/menubar (session/request-to-session request)
                        (if request (get request :uri))
                        (friend/current-authentication)
                        (request-to-suffixes request))]


;      [:div {:style "width:auto;margin-left:3em;padding:0.25em;float:left;background:#ccc"}
;       (str "can-haz admin:" haz-admin)]

      (if (and request (:query-params request) (get (:query-params request) "result"))
        [:div {:class "fadeout"}
         (get (:query-params request) "result")])

      [:div#content content])
   request title)))

(declare tr)
(def short-format
  (f/formatter "MMM dd, yyyy HH:mm"))
(defn display-time [timestamp]
  "convert time objects of various types to our chosen datetime format."
  (cond (= (type timestamp) java.sql.Timestamp)
        (f/unparse short-format (c/from-long (.getTime timestamp)))
        (= (type timestamp) org.joda.time.DateTime)
        (f/unparse short-format timestamp)
        true
        timestamp))

(declare default-td)
(declare default-th)

(defn table [rows & {:keys [columns
                            create-new
                            haz-admin
                            th
                            td
                            none]}]
  (let [columns (if columns columns
                    (if (not (empty? rows))
                      (keys (first rows))))
        haz-admin (if haz-admin haz-admin false)
        none (if none none "No results.")
        th (if th th (fn [key] (default-th key)))
        td (if td td 
               (fn [row key] (default-td row key)))]
    (html
     (if (empty? rows)
       [:p none ]

       ;; at least one row.
       ;; TODO: since keys are the same for all rows, pass along rather
       ;; than calling (keys) on each.
       [:table.classes.table-striped
        [:tr
         (concat
          [[:th]]
          (vec (map (fn [key]
                      (th key)) columns)))]
        (tr rows haz-admin 1 columns :td td)])
    
     (if (and (= true haz-admin)
              create-new)
       [:div {:style "float:left;margin-top:0.5em;width:100%"} [:a {:href (:href create-new)}
                                               (:link-text create-new)]]))))
  
(defn tr [rows haz-admin i columns & {:keys [td]}]
  (if (not (empty? rows))
    (let [row (first rows)
          i (if i i 1)
          td (if td td (fn [row key]
                         (cond true [:td (get row key)])))
          students-per-row (:students row)
          tests-per-row (:tests row)]
      (html [:tr (concat
                  [[:th.num i]]

                  (vec (map (fn [key]
                              (td row key))
                            columns)))] ;; TODO: invariant: pass along to recursive (tr) call.

            (tr (rest rows) haz-admin (+ 1 i) columns :td td)))))


(def hide "") ;; used by callers of (html/table)
(defn default-th [key]
  [:th (string/capitalize (string/join "" (rest (seq (str key)))))]) ;; :foo => "foo" => "Foo"

(defn default-td [row key]
  (let [the-val (get row key)]
    (cond (= java.sql.Timestamp (type the-val))
          [:td [:span {:class "date"}
                (display-time the-val)]]
          (number? the-val)
          [:td.num the-val]
          true
          [:td the-val])))

(defn printfs [fs & filename]
  "print a feature structure to a file. filename will be something easy to derive from the fs."
  (let [filename (if filename (first filename) "foo.html")]  ;; TODO: some conventional default if deriving from fs is too hard.
    (spit filename (static-page (tablize fs) filename))))

(defn plain [expr]
  "simply map expr in a map with one key :plain, whose value is expr.
   workbook/workbookq will format this accordingly."
  {:plain expr})


