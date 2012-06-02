(ns italianverbs.xml
  (:use [rdutest])
  (:require
   [clojure.string :as string]
   [clojure.contrib.str-utils2 :as str-utils]))

(defn encoding []
  (str "<?xml version='1.0' encoding='utf-8'?>"))

(defn content []
  "<xml/>")

(defn response [title & [content request]]
  (str (encoding) "<test title='" title "'" ">" content "</test>"))

(defn- xml-str
 "Like clojure.core/str but escapes < > and &."
 [x]
  (-> x str (.replace "&" "&amp;") (.replace "<" "&lt;") (.replace ">" "&gt;")))

;; serialize a map as XML
;; TODO: make it work for arbitrary keys - not specifically :english and :italian.
(defn serialize [map]
  (let [english (get map :english)
        italian (get map :italian)
        format (get map "format")
        input (get map "input")
        method (get map :method)
        guess (get map "guess")]
    (str (encoding)
         "<question>"
         "<method>" method "</method>"
         "<format>" format "</format>"
         "<input>" input "</input>"
         "<guess>" guess "</guess>"
         "<italian>" italian "</italian>"
         "<english>" english "</english>"
         "</question>")))
       
(def tests
  (list
   (rdutest
    "xml escaping"
    (xml-str (content))
    (fn [string]
      (= "&lt;xml/&gt;" string))
    :xml-escaping)
   (rdutest
    "xml guess"
    (xml-str (serialize {:italian "io sono stato"
                         :english "i went"}))
    (fn [string]
      (= "&lt;?xml version='1.0' encoding='utf-8'?&gt;&lt;question&gt;&lt;method&gt;&lt;/method&gt;&lt;format&gt;&lt;/format&gt;&lt;input&gt;&lt;/input&gt;&lt;guess&gt;&lt;/guess&gt;&lt;italian&gt;io sono stato&lt;/italian&gt;&lt;english&gt;i went&lt;/english&gt;&lt;/question&gt;" string))
    :xml-guess)))

