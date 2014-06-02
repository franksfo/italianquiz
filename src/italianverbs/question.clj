(ns italianverbs.question
  (:use [hiccup core])
  (:require
   [clj-time.core :as t]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [formative.core :as f]
   [formative.parse :as fp]
   [italianverbs.html :as html]
   [italianverbs.korma :as db]
   [italianverbs.unify :as unify]))

(defn new [params]
  (log/info (str "Got here with request: " params))
  (let [test-id (Integer. (get params "testid"))
        italiano (get params "italiano")
        english (get params "english")
        index 5
        new-question
        {:test test-id
         :english english
         :italian italiano
         :index index}
        result (db/insert! :question new-question)]
    "created"))


