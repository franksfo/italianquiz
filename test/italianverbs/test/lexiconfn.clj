(ns italianverbs.test.lexiconfn
  (:use [clojure.test]
        [italianverbs.lexiconfn])
  (:require
   [italianverbs.fs :as fs]))

;; TODO: for testing, consider some kind of mongodb mocking of some kind,
;; so that no actual mongodb connection would be necessary.

;(deftest parlare ;; A lexical entry for the word: 'parlare'
;  (let [merge-and-parla
;        (let [verb {:cat :verb}
;              human-subj {:subj {:human true}}
;              third-sing {:subj {:number :singular :person :3rd}}])]
;   (= (is true true))))


