(ns italianverbs.core
  (:use [compojure.core])
  (:require [compojure.route :as route]
            [compojure.handler :as handler]))

(defroutes app-routes
  (GET "/" [] "Hello World, I'm Rags..woof woof")
  (GET "/foo" [] "Foo is here.")
  (route/not-found "Not Found"))

(def app
  (handler/site app-routes))
