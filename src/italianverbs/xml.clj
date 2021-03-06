(ns italianverbs.xml
  (:require
   [clojure.string :as string]))

(defn encoding []
  (str "<?xml version='1.0' encoding='utf-8'?>"))

(defn content []
  "<xml/>")

(defn response [title & [content request]]
  (str (encoding) "<test title='" title "'" ">" content "</test>"))

(defn xml-str
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
