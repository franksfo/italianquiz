(ns base.lib
  (:use [hiccup core])
  (:require
   [clojure.set :as set]
   [clojure.string :as string]))

(defn get-session-key [request]
  (let [cookies (get request :cookies)]
    (if cookies
      (let [ring-session (get cookies "ring-session")]
        (if ring-session
          (get ring-session :value))))))
