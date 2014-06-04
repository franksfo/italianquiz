(ns italianverbs.test_submit
  (:require [clojure.tools.logging :as log]
            [italianverbs.korma :as db]))

(defn new [test_id request user]
  (log/info (str "submitting test: " test_id " for user: " user " with data: " request))
  (let [user_id (:id (first (db/fetch :user {:username (:username user)})))]
    (let [tsubmit
          (db/insert! :test-submit {:test (Integer. test_id)
                                    :student (Integer. user_id)})]
      ;; iterate over form-params and add each as a question-submit for this test-submit (tsubmit).
      ))
  {:message "submitted"})


  
