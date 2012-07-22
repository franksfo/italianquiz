(ns italianverbs.test.grammar
  (:use [clojure.test])
  (:use [italianverbs.grammar]))

(deftest time-format
  (let [formatted-time
        (english-time 5 43 "pm")]
    (is (= formatted-time "5:43"))))

(deftest random-lexeme
  (let [lexeme (choose-lexeme {})]
    (is (not (= nil (:italian lexeme))))))




