(ns italianverbs.test.lexiconfn
  (:refer-clojure :exclude [get get-in merge resolve find])
  (:require
   [clojure.set :as set]
   [clojure.tools.logging :as log]
   [clojure.core :as core]
   [clojure.test :refer :all]
   [italianverbs.espanol :as es]
   [italianverbs.italiano :as it]
   [italianverbs.lexiconfn :refer :all]
   [italianverbs.morphology :as morph]
   [italianverbs.morphology.espanol :as es-morph]
   [italianverbs.pos :refer :all]
   [italianverbs.unify :refer :all :exclude (unify)]))

(def listified-es (listify es/lexicon-source))
(def listified-it (listify it/lexicon-source))

(deftest is-map-es (is (map? listified-es)))
(deftest is-map-en (is (map? listified-it)))

(def phonize-fn-es es-morph/phonize)
(def phonified-es (map-function-on-map-vals
                   listified-es
                   (fn [lexical-string lexical-val]
                     (phonize-fn-es lexical-val lexical-string))))

(deftest is-map-phon-es (is (map? phonified-es)))

(def lexicon-stage-2 (map-function-on-map-vals 
                      phonified-es
                      (fn [lexical-string lexeme]
                        (map (fn [lexeme]
                               (transform lexeme rules))
                             lexeme))))

(deftest is-map-lex-stage-2-es (is (map? lexicon-stage-2)))

(def italian-language-specific-rules nil)
(def language-specific-rules italian-language-specific-rules)

(def lexicon-stage-3 (if italian-language-specific-rules
                       (map-function-on-map-vals
                        lexicon-stage-2
                        (fn [lexical-string lexeme]
                          (map (fn [lexeme]
                                 (transform lexeme language-specific-rules))
                               lexeme)))
                       ;; no language-specific rules: lexicon-stage-3 == lexicon-stage-2
                       lexicon-stage-2))

(def exception-generator es-morph/exception-generator)

(def exceptions-generated (exception-generator lexicon-stage-3))

(def listified (map #(listify %)
                    exceptions-generated))

(def reduced (cond
              (empty? exceptions-generated)
              nil
              true
              (reduce #(merge-with concat %1 %2)
                      exceptions-generated)))

;(def exceptions (listify (reduce #(merge-with concat %1 %2)
;                                 (map #(listify %)
;                                      exceptions-generated))))


