(ns italianverbs.test_submit
  (:require [clojure.string :as string]
            [clojure.tools.logging :as log]
            [italianverbs.korma :as db]))

(defn insert-answers [responses test-submit-id]
  (if (not (empty? responses))
    (let [key (first (first responses))]
      (if (re-find #"^:\d+$" (str key))
        (let [questionid (Integer. (string/replace (str key) #"^:" ""))
              answer (:italian (second (first responses)))]
          (log/info (str "question id: " questionid))
          (log/info (str "anwser: " answer))
          (db/insert! :question-submit {:tsubmit test-submit-id
                                        :question questionid
                                        :answer answer})))
      (insert-answers (rest responses) test-submit-id))))

(defn new [test_id responses user]
  (log/info (str "submitting test: " test_id " for user: " user " with data: " responses))
  (let [user_id (:id (first (db/fetch :user {:username (:username user)})))]
    (if (nil? user_id)
      (do
        (log/error (str "no user in database found for user object: " user))
        {:message "error:no such user"})
      (let [tsubmit
            (db/insert! :test-submit {:test (Integer. test_id)
                                      :student (Integer. user_id)})]
        ;; iterate over form-params and add each as a question-submit for this test-submit (tsubmit).
        (insert-answers responses (:id tsubmit))))
    {:message "submitted"}))



  
