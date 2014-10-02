(ns italianverbs.auth)

(require '[clojure.tools.logging :as log])
(require '[clojure.string :as str])
(require '[digest])
(require '[environ.core :refer [env]])
(require '[italianverbs.korma :as db])

(defn confirm-and-create-user [request]
  (do (log/info (str "confirm-and-create-user: " request))
      {:status 302
       :headers {"Location" "/game"}}))

