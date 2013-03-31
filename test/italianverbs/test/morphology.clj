(ns italianverbs.test.morphology
  (:use [clojure.test])
  (:use [italianverbs.morphology])
  (:require
;   [clojure.contrib.logging :as log]
   [clojure.string :as string]
;   [clojure.contrib.string :as stringc]
;   [clojure.contrib.str-utils2 :as str-utils]
   ))

;(defn test []
;  (list
;   {:comment "stem verb for futuro semplice"
;    :test (stem-per-futuro "tornare")}))

(deftest future-stem
  (let [future-stem (stem-per-futuro "tornare")]
    (is (= future-stem "torner"))))

(deftest io-mangio
  (let [io-mangio ;; Conjugate 'io' + 'mangiare' => ' mangio'
        (conjugate-italian-verb {:infl "infinitive", :cat "verb", :italian "mangiare", :english "to eat"}
                                {:italian "io", :english "i", :person :1st, :number :singular})]
    (is (= io-mangio " mangio")))) ;; TODO: conjugate-italian-verb should strip whitespace.

(deftest io-preferisco
  (let [io-preferisco ;; -isco conjugation: 'io' + 'preferire' => ' preferisco'
        (conjugate-italian-verb {:infl "infinitive", :cat "verb", :isco true :italian "preferire", :english "to prefer"}
                                {:italian "io", :english "i", :person :1st, :number :singular})]
    (is 
     (= io-preferisco " preferisco"))))  ;; TODO: conjugate-italian-verb should strip whitespace.

(deftest en-plural-1 ;; english noun pluralization.
  (let [result (plural-en "girl")]
    (is (= result "girls"))))

(deftest en-plural-2 ;; english noun pluralization: x -> xes
  (let [result (plural-en "box")]
    (is (= result "boxes"))))

(deftest italian-plural
  (let [result (plural-masc "cane")]
    (is (= result "cani"))))

(deftest passato ;; Regular passato-prossimo conjugation
  (let [result (passato-prossimo "lavorare")]
    (is (= result "lavorato"))))

(deftest della-acqua
  (is (= (get-italian-stub-1 {:a {:italian "di la"}
                              :b {:italian "acqua"
                                  :agr {:gender :fem
                                        :number :sing}
                                  :cat :noun}})
         "della acqua")))

(deftest degli-uomini
  (is (= (get-italian-stub-1 {:a {:italian "di i"}
                              :b {:italian {:irregular {:plur "uomini"}
                                            :italian "uomo"}
                                  :agr {:gender :masc
                                        :number :plur}
                                  :cat :noun}})
         "degli uomini")))


(deftest some-men
  (is (= (get-english-stub-1 {:a {:english "some"}
                              :b {:english {:irregular {:plur "men"}
                                            :english "man"}
                                  :agr {:gender :masc
                                        :number :plur}
                                  :cat :noun}})
         "some men")))


