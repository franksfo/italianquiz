(ns italianverbs.xml
  (:use [italianverbs.rdutest])
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

;; new tests.
(def testresults
  (list
   (rdutest
    "Sanity check: test rdutest itself by assuming that '+' is correct."
    (+ 1 2)
    #(= % 3))
   (rdutest
    "XML content escaped (so that it can be printed inside an HTML document)."
    (xml-str (content))
    #(= % "&lt;xml/&gt;"))
   (rdutest
    "Serialize a Clojure map."
    (xml-str (serialize {:italian "io sono stato" :english "i went"}))
    #(not (= % "")))))

;; FIXME: move to test.clj.
(def evaluate-testresults
  (map (fn [result] {:comment (:comment result) :result (:assert-result result)})  testresults))





