(ns italianverbs.test.morphology
  (:use [clojure.test])
  (:use [italianverbs.morphology])
  (:require
   [clojure.string :as string]))

(deftest acqua
  (is
   (= (get-italian-1 {:italian "acqua"
                           :agr {:gender :fem
                                 :number :sing}
                           :cat :noun})
      "acqua")
  ))

(deftest della-acqua
  (is (= (get-italian-1 {:a "di la"
                              :b {:italian "acqua"
                                  :agr {:gender :fem
                                        :number :sing}
                                  :cat :noun}})
         "della acqua")))



(deftest degli-uomini
  (is (= (get-italian-1 {:a "di i"
                              :b {:italian "uomo"
                                  :irregular {:plur "uomini"}
                                  :agr {:gender :masc
                                        :number :plur}
                                  :cat :noun}})
         "degli uomini")))

(deftest some-men
  (is (= (get-english-1 {:a "some"
                              :b {:english "man"
                                  :irregular {:plur "men"}
                                  :agr {:gender :masc
                                        :number :plur}
                                  :cat :noun}})
         "some men")))

(deftest dormo
  (is (= (get-italian-1 {:agr {:case :nom
                                    :number :sing
                                    :person :1st}
                              :infinitive "dormire"
                              :infl :present})
         "dormo")))


(deftest sleeps
  (is (= (get-english-1 {:agr {:case :nom
                                    :number :sing
                                    :person :3rd}
                              :infinitive "to sleep"
                              :infl :present})
         "sleeps")))



(deftest io-vado
  (is (= (get-italian-1 {:agr {:case :nom
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
       (get-italian-1 
        {:infl :past
         :irregular {:passato "andato"}
         :agr {:gender :fem :number :sing}
         :essere true})
       "andata")))

(deftest conjugate-irreg-passato-2
  (is (=
       (get-italian-1 
        {:infl :past
         :irregular {:passato "andato"}
         :agr {:gender :fem :number :plur}
         :essere true})
       "andate")))


(deftest conjugate-irreg-passato-2
  (is (=
       (get-italian-1 
        {:infl :past
         :irregular {:passato "andato"}
         :agr {:number :plur
               :gender :masc}
         :essere true})
       "andati")))

(deftest conjugate-irreg-passato-3
  (is (=
       (get-italian-1 
        {:infl :past
         :irregular {:passato "andato"}
         :agr {:number :plur}
         :essere true})
       "andati")))


(deftest conjugate-irreg-passato-4
  "not specific enough: retain as map rather than conjugating."
  (is (=
       (get-italian-1 
        {:infl :past
         :irregular {:passato "andato"}
         :essere true})
       {:infl :past
        :irregular {:passato "andato"}
        :essere true})))



(deftest plural-noun-singular
  "il studente => lo studente"
  (is (=
       (get-italian
        "il"
        "studente")
       "lo studente")))

(deftest plural-noun-plural
  "i studente => gli studenti"
  (is (=
       "gli studenti"
       (get-italian
        "i"
        {:italian "studente"
         :agr {:person :3rd
               :gender :masc
               :number :plur}}))))


(deftest masculine-pp
  "a il mare => al mare"
  (is (=
       "al mare"
       (get-italian
        "a"
        "il mare"))))

(deftest english-proper-noun
  (is (= "rome"
         (get-english-1 {:english {:english "rome"}}))))


(deftest io-andro
  (is (= "andrò"
         (get-italian-1
          {:agr {:number :sing :person :1st}
           :irregular {:futuro {:1sing "andrò"}}
           :infinitive "andare"
           :infl :futuro}))))

