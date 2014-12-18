(ns italianverbs.editor
  (:require
   [compojure.core :as compojure :refer [defroutes routes GET PUT POST DELETE ANY]]
   [italianverbs.html :as html]
   ))

(defn do-x [request]
  {:status 200
   :body (html/page "Editor" "You chose X...." request {})})

(defn do-y [request]
  {:status 200
   :body (html/page "Editor" "You chose Y...." request {})})

(defn editor-routes []
  (routes
   (GET "/x" request
        (do-x request))
   (GET "/y" request
        (do-y request))))




