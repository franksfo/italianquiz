(ns italianverbs.studenttest
  (:require
   [clj-time.core :as t]
   [clojure.tools.logging :as log]
   [italianverbs.korma :as db]))

(declare delete-form)
(declare new-test-form)
(declare select)
(declare show-as-rows)
(declare validate-new-test)

(defn insert-new-test [test-params]
  "new-test should be a string."
  (let [new-test (:name test-params)]
    (if (validate-new-test new-test)
      ;; new-verb checks out ok: add it to DB.
      (do (log/info (str "Adding validated candidate test: " new-test))
          ;; TODO: add return code and append to request.
          (let [created-at (t/now)]
            (db/insert! :test {:created (str created-at)
                               :updated (str created-at)
                               :name new-test}))))))

(defn new [session request]
  (log/debug (str "/studenttest/new with request: " (:form-params request)))
;  (insert-new-test (:form-params request))
  (insert-new-test {:name "hello"})
  {:message "Not doing anything yet."})

(defn validate-new-test [new-test]
  true)



