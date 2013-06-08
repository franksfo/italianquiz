(ns italianverbs.html
  (:use
   [hiccup core page]
   [ring.util.codec :as codec])
  (:require
   [clojure.set :as set]
   [italianverbs.unify :as fs]
   [clojure.tools.logging :as log]
   [clojure.string :as string]))

(defn verb-row [italian]
  (html
   [:tr
    [:th italian] [:td "FAIL." ]
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
  (cond
   (nil? feature-structure)
   "NIL.."
   (= java.lang.String (type feature-structure)) feature-structure
   (= clojure.lang.Keyword (type feature-structure)) feature-structure
   true
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
                                                 (fs (fs/get-in feature-structure (list key))))
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
     (include-css "resources/public/css/fs.css")
     (include-css "resources/public/css/layout.css")
     (include-css "resources/public/css/quiz.css")
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

;; TODO: use multimethod based on arg's type.
(defn tablize [arg & [path serialized opts]]
  ;; set defaults.
  ;; (TODO: in which contexts are we passing an already-serialized arg?)
  ;; if not already serialized, then serialize:
  (let [serialized (if (nil? serialized)
                     (try
                       (fs/serialize arg)
                       (catch Exception e
                         (do
                           (log/warn (str "Trying to recover from serialization error: " e " caused by arg with type: " (type arg)))
                           (fs/serialize {:a 42}))))
                     serialized) ;; .. if already serialized, use that.
        ;; ((nil? serialized) = false): tread the input arg as already-serialized.
        opts (if (nil? opts)
               {:as-tree true})
        ]
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
     (tablize (first arg) path serialized opts)
     (or (set? arg)
         (list? arg)
         (= (type arg) clojure.lang.Cons))
     (str
      (clojure.string/join ""
                           (map (fn [each-arg]
                                  (tablize each-arg path (fs/serialize each-arg) opts))
                                arg)))


     ;; displaying a phrase structure tree (2 children) (new implementation)
     ;; Head-initial (H C)
     (and
      (map? arg)

      (not (= :subcat (last path)))
      (not (= :italian (last path)))
      (not (= :english (last path)))

      ;; this long set of (nots) is to prevent matching :extend:
      ;; TODO: might be possible to remove this.
      ;; :extends will have features :a,:b,:c,..
      (not (= :a (last path)))
      (not (= :b (last path)))
      (not (= :c (last path)))
      (not (= :d (last path)))
      (not (= :e (last path)))
      (not (= :f (last path)))
      (not (= :g (last path)))
      (not (= :none (:head arg :none)))
      (not (= :none (:comp arg :none)))
      (= :none (:1 arg :none))
      (= :none (:2 arg :none))
      (not (= :none (fs/get-in arg '(:head :italian) :none)))
      (not (= :none (fs/get-in arg '(:head :italian) :none)))
      (not (= :none (fs/get-in arg '(:comp :english) :none)))
      (not (= :none (fs/get-in arg '(:comp :english) :none)))
      ;; head-initial:
      (= (:italian (:head arg))
         (:a (:italian arg)))
      (= (:italian (:comp arg))
         (:b (:italian arg))))

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
      "      <td>H</td><td>"
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
      "      <td>C</td><td>"
      (tablize (if (= (type (:comp arg)) clojure.lang.Ref)
                 @(:comp arg)
                 (:comp arg))
               (concat path '(:comp)) serialized opts)
      "      </td><td>&nbsp;</td>"
      "    </tr>"
      "  </table>"
      "</div>")


     ;; displaying a phrase structure tree (2 children) (new implementation)
     ;; Head-final (C H)
     (and
      (map? arg)

      (not (= :subcat (last path)))
      (not (= :italian (last path)))
      (not (= :english (last path)))

      ;; this long set of (nots) is to prevent matching :extend:
      ;; TODO: might be possible to remove this.
      ;; :extends will have features :a,:b,:c,..
      (not (= :a (last path)))
      (not (= :b (last path)))
      (not (= :c (last path)))
      (not (= :d (last path)))
      (not (= :e (last path)))
      (not (= :f (last path)))
      (not (= :g (last path)))

      (not (= :none (:head arg :none)))
      (not (= :none (:comp arg :none)))
      (= :none (:1 arg :none))
      (= :none (:2 arg :none))
      (not (= :none (fs/get-in arg '(:head :italian) :none)))
      (not (= :none (fs/get-in arg '(:head :italian) :none)))
      (not (= :none (fs/get-in arg '(:comp :english) :none)))
      (not (= :none (fs/get-in arg '(:comp :english) :none)))
      ;; TODO: define: fs/ref= rather than this reverse-order checking.
      ;; head-final:
      (= (:italian (:head arg))
         (:b (:italian arg)))
      (= (:italian (:comp arg))
         (:a (:italian arg))))

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
      "      <td>C</td><td>"
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
      "      <td>H</td><td>"
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
        ;; sorts the argument list in _arg__ by key name:
        (remove #(= (first %) :comment-plaintext) (into (sorted-map) arg))))
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

