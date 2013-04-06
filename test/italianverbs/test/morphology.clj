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


(deftest acqua
  (is
   (= (get-italian-stub-1 {:italian "acqua"
                           :agr {:gender :fem
                                 :number :sing}
                           :cat :noun})
      "acqua")
  ))

(deftest della-acqua
  (is (= (get-italian-stub-1 {:a "di la"
                              :b {:italian "acqua"
                                  :agr {:gender :fem
                                        :number :sing}
                                  :cat :noun}})
         "della acqua")))



(deftest degli-uomini
  (is (= (get-italian-stub-1 {:a "di i"
                              :b {:italian "uomo"
                                  :irregular {:plur "uomini"}
                                  :agr {:gender :masc
                                        :number :plur}
                                  :cat :noun}})
         "degli uomini")))

(deftest some-men
  (is (= (get-english-stub-1 {:a "some"
                              :b {:english "man"
                                  :irregular {:plur "men"}
                                  :agr {:gender :masc
                                        :number :plur}
                                  :cat :noun}})
         "some men")))

(deftest dormo
  (is (= (get-italian-stub-1 {:agr {:case :nom
                                    :number :sing
                                    :person :1st}
                              :infinitive "dormire"
                              :infl :present})
         "dormo")))


(deftest sleeps
  (is (= (get-english-stub-1 {:agr {:case :nom
                                    :number :sing
                                    :person :3rd}
                              :infinitive "to sleep"
                              :infl :present})
         "sleeps")))



(deftest io-vado
  (is (= (get-italian-stub-1 {:agr {:case :nom
                                    :number :sing
                                    :person :1st}
                              :irregular {:present {:1sing "vado"}}
                              :infinitive "andare"
                              :infl :present})
         "vado")))


(deftest suffix-of-1
  (is (=
       (suffix-of {:irregular {:passato "andato"}
                   :infl :past
                   :agr {:person :1st :number :plur} :essere true})
       "i")))

(deftest suffix-of-2
  (is (=
       (suffix-of {:irregular {:passato "andato"}
                   :infl :past
                   :agr {:gender :fem :number :plur}
                   :essere true})
       "e")))

(deftest suffix-of-3
  (is (=
       (suffix-of {:irregular {:passato "andato"}
                   :infl :past
                   :agr {:gender :fem :number :sing}
                   :essere true})
       "a")))


(deftest conjugate-irreg-passato-1
  (is (=
       (get-italian-stub-1 
        {:infl :past
         :irregular {:passato "andato"}
         :agr {:gender :fem :number :sing}
         :essere true})
       "andata")))

(deftest conjugate-irreg-passato-2
  (is (=
       (get-italian-stub-1 
        {:infl :past
         :irregular {:passato "andato"}
         :agr {:gender :fem :number :plur}
         :essere true})
       "andate")))


(deftest conjugate-irreg-passato-2
  (is (=
       (get-italian-stub-1 
        {:infl :past
         :irregular {:passato "andato"}
         :agr {:number :plur
               :gender :masc}
         :essere true})
       "andati")))

(deftest conjugate-irreg-passato-3
  (is (=
       (get-italian-stub-1 
        {:infl :past
         :irregular {:passato "andato"}
         :agr {:number :plur}
         :essere true})
       "andati")))


(deftest conjugate-irreg-passato-4
  "not specific enough: retain as map rather than conjugating."
  (is (=
       (get-italian-stub-1 
        {:infl :past
         :irregular {:passato "andato"}
         :essere true})
       {:infl :past
        :irregular {:passato "andato"}
        :essere true})))



