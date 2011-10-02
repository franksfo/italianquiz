(ns italianverbs.xml
  (:require
   [clojure.string :as string]
   [clojure.contrib.str-utils2 :as str-utils]))

(defn encoding []
  (str "<?xml version='1.0' encoding='ISO-8859-1'?>"))

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
        guess (get map "guess")]
    (str (encoding)
         "<question>"
         "<format>" format "</format>"
         "<input>" input "</input>"
         "<guess>" guess "</guess>"
         "<italian>" italian "</italian>"
         "<english>" english "</english>"
         "</question>")))
       
(defn test []
  "this should contain a list of all the tests for the html package. each test can
  return a map or a list or a function. a function will be applied against an
  empty argument list"
  (list
   {:comment "xml content"
    :test (xml-str (content))}
   {:comment "xml guess"
    :test (xml-str (serialize
                    {:italian "io sono stato"
                     :english "i went"}))}))




