(ns italianverbs.test.morphology
  (:use [clojure.test])
  (:use [italianverbs.morphology])
  (:require
   [clojure.string :as string]))
(if true
  (do
(deftest phrase-finished-1
  (is (phrase-is-finished?
       {:italian "acqua"}))

  (is (phrase-is-finished?
       {:italian
        {:a {:infinitive "essere"}
         :b {:a "il"
             :b {:italian "professore"}}}})))

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
         "dell'acqua")))



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


(deftest io-sogno
  (is (= (get-italian-1 {:a "io"
                         :b {:essere false
                             :infinitive "sognare"
                             :agr {:case :nom
                                   :person :1st
                                   :number :sing}
                             :infl :present}})
         "io sogno")))


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

(deftest tu-bevevi
  (is (= "bevevi"
         (get-italian-1
          {:agr {:number :sing :person :2nd}
           :infinitive "bevere"
           :infl :imperfetto}))))

(deftest was-sleeping
  (is (= "was sleeping"
         (get-english-1
          {:agr {:number :sing :person :1st}
           :infinitive "to sleep"
           :infl :imperfetto}))))

(deftest were-sleeping
  (is (= "were sleeping"
         (get-english-1
          {:agr {:number :sing :person :2nd}
           :infinitive "to sleep"
           :infl :imperfetto}))))

(deftest io-sono
  (is (= "sono"
         (get-italian-1
          {:agr {:number :sing
                 :person :1st}
           :cat :verb
           :infl :present
           :irregular {:present {:1sing "sono"}}}))))

(deftest to-speak-this-word
  (is (= "to speak this word"
         (get-english
          {:irregular {:past "spoken"}
           :infl :infinitive
           :cat :verb
           :agr {:person :2nd
                 :case :nom
                 :number :sing}
           :infinitive "to speak"}
          {:a "this"
           :b {:agr
               {:number :sing
                :person :3rd
                :gender :fem
                :case {:not :nom}}
               :english "word"
               :cat :noun}}))))

(deftest wants-to-speak-this-word
  (is (=
       "wants to speak this word"
       (get-english
        {:cat :verb
         :modal true
         :infinitive "to want to"
         :irregular {:present {:1sing "want to"
                               :2sing "want to"
                               :3sing "wants to"
                               :1plur "want to"
                               :2plur "want to"
                               :3plur "want to"}
                     :past "wanted to"}
         :agr {:person :3rd
               :case :nom
               :number :sing}
         :infl :present}
        {:a {:irregular {:past "spoken"}
             :infl :infinitive
             :cat :verb
             :agr {:person :top
                   :case :nom
                   :number :top}
             :infinitive "to speak"}
         :b {:a "this"
             :b {:agr
                 {:number :sing
                  :person :3rd
                  :gender :fem
                  :case {:not :nom}}
                 :english "word"
                 :cat :noun}}}))))

(if true
  (do
(deftest you-want-to-speak
  (is (= "you want to speak this word"
         (get-english-1
          {:a "you"

           :b {

               :a {:cat :verb
                   :modal true
                   :infinitive "to want to"
                   :irregular {:present {:1sing "want to"
                                         :2sing "want to"
                                         :3sing "wants to"
                                         :1plur "want to"
                                         :2plur "want to"
                                         :3plur "want to"}
                               :past "wanted to"}
                   :agr {:person :2nd
                         :case :nom
                         :number :sing}
                   :infl :present}

               :b {:a {:irregular {:past "spoken"}
                       :infl :infinitive
                       :cat :verb
                       :agr {:person :2nd
                             :case :nom
                            :number :sing}
                       :infinitive "to speak"}
                   :b {:a "this"
                       :b {:agr
                           {:number :sing
                            :person :3rd
                            :gender :fem
                            :case {:not :nom}}
                           :english "word"
                           :cat :noun}}
                   }
               :agr {:person :2nd
                     :case :nom
                     :number :sing}
               :infl :present
               }
           }

          ))))

(deftest we-can-drink-that-water
  (is (= "we can drink that water"
         (get-english-1
          {:a "we"

           :b {:a {:cat :verb
                   :modal true
                   :infinitive "to be able to"
                   :irregular {:present {:1sing "can"
                                         :3plur "can"
                                         :1plur "can"
                                         :2plur "can"
                                         :3sing "can"
                                         :2sing "can"}
                               :past "could"}
                   :agr {:person :1st
                         :case :nom
                         :number :plur}
                   :infl :present}

               :b {:a {:irregular {:past "drunk"}
                       :infl :infinitive
                       :cat :verb
                       :agr {:person :1st
                             :case :nom
                             :number :plur}
                       :infinitive "to drink"}
                   :b {:a "that"
                       :b {:agr {:number :sing
                                 :person :3rd
                                 :gender :fem
                                 :case {:not :nom}}
                           :english "water"
                           :cat :noun}}
                   }
               :agr {:person :1st
                     :case :nom
                     :number :plur}
               :infl :present
               }
           }
          ))))


))

(deftest leggeva
  (is (= "leggeva"
         (get-italian-1
          {:irregular {:passato "letto"}
           :infl :imperfetto
           :essere false
           :agr {:person :3rd
                 :case {:not :acc}
                 :number :sing}
           :infinitive "leggere"}))))

(deftest was-reading
  (is (= "was reading"
         (get-english-1
          {:irregular {:past "read"}
           :infl :imperfetto
           :agr {:person :3rd
                 :case {:not :acc}
                 :number :sing}
           :infinitive "to read"}))))

(deftest having-to
  (is (= "was having to"
         (get-english-1
          {:irregular {:past "had to"
                       :present {:2plur "have to"
                                 :3sing "has to"
                                 :2sing "have to"
                                 :1sing "have to"
                                 :3plur "have to"
                                 :1plur "have to"}}
           :infl :imperfetto
           :agr {:person :3rd
                 :case {:not :acc}
                 :number :sing}
           :modal true
           :infinitive "to have to"}))))




