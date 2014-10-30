(ns italianverbs.test.morphology
  (:refer-clojure :exclude [get-in merge resolve str])
  (:use [clojure.test]))


(require '[clojure.string :as string])
(require '[clojure.string :refer (trim)])
(require '[italianverbs.morphology :refer :all])
(require '[italianverbs.morphology.english :as english])
(require '[italianverbs.morphology.italiano :as italiano])
(require '[italianverbs.unify :refer :all])

(deftest phrase-finished-1
  (is (italiano/phrase-is-finished?
       {:italiano "acqua"}))

  (is (italiano/phrase-is-finished?
       {:italiano
        {:a {:italiano "essere"}
         :b {:a "il"
             :b {:italiano "professore"}}}})))

(deftest acqua
  (is
   (= (italiano/get-string-1 {:italiano "acqua"
                               :agr {:gender :fem
                                     :number :sing}
                               :cat :noun})
      "acqua")
   ))

(deftest della-acqua
  (is (= (italiano/get-string-1 {:a "di la"
                         :b {:italiano "acqua"
                             :agr {:gender :fem
                                   :number :sing}
                             :cat :noun}})
         "dell'acqua")))



(deftest degli-uomini
  (is (= (italiano/get-string-1 {:a "di i"
                         :b {:italiano "uomo"
                             :irregular {:plur "uomini"}
                             :agr {:gender :masc
                                   :number :plur}
                             :cat :noun}})
         "degli uomini")))

(deftest some-men
  (is (= (english/get-string-1 {:a "some"
                         :b {:english "man"
                             :irregular {:plur "men"}
                             :agr {:gender :masc
                                   :number :plur}
                             :cat :noun}})
         "some men")))

(deftest dormo
  (is (= (italiano/get-string-1 {:agr {:case :nom
                               :number :sing
                               :person :1st}
                         :italiano "dormire"
                         :infl :present})
         "dormo")))


(deftest sleeps
  (is (= (english/get-string-1 {:agr {:case :nom
                               :number :sing
                               :person :3rd}
                         :infinitive "to sleep"
                         :infl :present})
         "sleeps")))



(deftest io-vado
  (is (= (italiano/get-string-1 {:agr {:case :nom
                               :number :sing
                               :person :1st}
                         :irregular {:present {:1sing "vado"}}
                         :italiano "andare"
                         :infl :present})
         "vado")))


(deftest io-sogno
  (is (= (italiano/get-string-1 {:a "io"
                         :b {:essere false
                             :italiano "sognare"
                             :agr {:case :nom
                                   :person :1st
                                   :number :sing}
                             :infl :present}})
         "io sogno")))


(deftest suffix-of-1
  (is (=
       (italiano/suffix-of {:irregular {:passato "andato"}
                            :infl :past
                            :agr {:person :1st :number :plur} :essere true})
       "i")))

(deftest suffix-of-2
  (is (=
       (italiano/suffix-of {:irregular {:passato "andato"}
                            :infl :past
                            :agr {:gender :fem :number :plur}
                            :essere true})
       "e")))

(deftest suffix-of-3
  (is (=
       (italiano/suffix-of {:irregular {:passato "andato"}
                            :infl :past
                            :agr {:gender :fem :number :sing}
                            :essere true})
       "a")))


(deftest conjugate-irreg-passato-1
  (is (=
       (italiano/get-string-1 
        {:infl :past
         :irregular {:passato "andato"}
         :agr {:gender :fem :number :sing}
         :essere true})
       "andata")))

(deftest conjugate-irreg-passato-2
  (is (=
       (italiano/get-string-1 
        {:infl :past
         :irregular {:passato "andato"}
         :agr {:gender :fem :number :plur}
         :essere true})
       "andate")))


(deftest conjugate-irreg-passato-2
  (is (=
       (italiano/get-string-1 
        {:infl :past
         :irregular {:passato "andato"}
         :agr {:number :plur
               :gender :masc}
         :essere true})
       "andati")))

(deftest conjugate-irreg-passato-3
  (is (=
       (italiano/get-string-1
        {:infl :past
         :irregular {:passato "andato"}
         :agr {:number :plur}
         :essere true})
       "andati")))

(deftest plural-noun-singular
  "il studente => lo studente"
  (is (=
       (italiano/get-string
        "il"
        "studente")
       "lo studente")))

(deftest plural-noun-plural
  "i studente => gli studenti"
  (is (=
       "gli studenti"
       (italiano/get-string
        "i"
        {:italiano "studente"
         :agr {:person :3rd
               :gender :masc
               :number :plur}}))))


(deftest masculine-pp
  "a il mare => al mare"
  (is (=
       "al mare"
       (italiano/get-string
        "a"
        "il mare"))))

(deftest english-proper-noun
  (is (= "rome"
         (english/get-string-1 {:english {:english "rome"}}))))


;; regular future
(deftest io-dormiro
  (is (= "dormirò"
         (italiano/get-string-1
          {:agr {:number :sing :person :1st}
           :italiano "dormire"
           :infl :futuro}))))

;; irregular future
(deftest io-andro
  (is (= "andrò"
         (italiano/get-string-1
          {:agr {:number :sing :person :1st}
           :irregular {:futuro {:1sing "andrò"}}
           :italiano "andare"
           :infl :futuro}))))

(deftest tu-bevevi
  (is (= "bevevi"
         (italiano/get-string-1
          {:agr {:number :sing :person :2nd}
           :italiano "bevere"
           :infl :imperfetto}))))

(deftest was-sleeping
  (is (= "was sleeping"
         (english/get-string-1
          {:agr {:number :sing :person :1st}
           :infinitive "to sleep"
           :infl :imperfetto}))))

(deftest were-sleeping
  (is (= "were sleeping"
         (english/get-string-1
          {:agr {:number :sing :person :2nd}
           :infinitive "to sleep"
           :infl :imperfetto}))))

(deftest io-sono
  (is (= "sono"
         (italiano/get-string-1
          {:agr {:number :sing
                 :person :1st}
           :cat :verb
           :infl :present
           :irregular {:present {:1sing "sono"}}}))))

(deftest to-speak-this-word
  (is (= "to speak this word"
         (english/get-string
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
       (english/get-string
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
         (english/get-string-1
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
         (english/get-string-1
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
         (italiano/get-string-1
          {:irregular {:passato "letto"}
           :infl :imperfetto
           :essere false
           :agr {:person :3rd
                 :case {:not :acc}
                 :number :sing}
           :italiano "leggere"}))))

(deftest was-reading
  (is (= "was reading"
         (english/get-string-1
          {:irregular {:past "read"}
           :infl :imperfetto
           :agr {:person :3rd
                 :case {:not :acc}
                 :number :sing}
           :infinitive "to read"}))))

(deftest having-to
  (is (= "was having to"
         (english/get-string-1
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

;; partial morphology: what to show when there's not
;; enough constraints to conjugate words

(deftest see
  (is (= "to see"
         (english/get-string-1
          {:irregular {:past "seen"}
           :infl :present
           :agr {:case {:not :acc}}
           :infinitive "to see"}))))

(deftest to-see-me
  (is (= "to see me"
         (english/get-string-1
          {:a {:irregular {:past "seen"}
               :infl :present
               :agr {:case {:not :acc}}
               :infinitive "to see"}
           :b "me"}))))

(deftest were
  (is (= "[were]"
         (english/get-string-1
          {:cat :verb
           :infinitive "to be"
           :irregular {:past {:1sing "was"
                              :2sing "were"
                              :3sing "was"
                              :1plur "were"
                              :2plur "were"
                              :3plur "were"
                              }}
           :agr {:number :top
                 :case {:not :acc}
                 :gender :top}
           :infl :past}))))

(deftest were-more-small-than-you-all
  (is (= "[were] more small than you all"
         (english/get-string-1
          {:b "more small than you all"
           :a {:cat :verb :infinitive "to be"
               :irregular {:present {:1sing "am" :3plur "are" :1plur "are" :2plur "are" :3sing "is" :2sing "are"}
                           :past {:1sing "was" :3sing "was" :2plur "were" :1plur "were" :3plur "were" :participle "been" :2sing "were"}}
               :agr {:number :top
                     :case {:not :acc}
                     :gender :top}
               :infl :past}}))))

(deftest vp-pron
  (is (= "to see her"
         (english/get-string-1
          {:a {:irregular {:past "seen"}, :infl :top, :agr {:case {:not :acc}}, :infinitive "to see"}, :b "her"}))))


(deftest professori
  (is (= "professori"
         (italiano/get-string-1 {:agr {:gender :masc
                               :person :3rd
                               :case :top
                               :number :plur}
                         :cat :noun
                         :italiano "professore"}))))

(deftest fatto
  (is (= "fatto"
         (italiano/get-string-1 {:cat :verb
                         :irregular {:present {:1sing "facio"
                                               :3plur "fanno"
                                               :1plur "facciamo"
                                               :2plur "fate"
                                               :3sing "fa"
                                               :2sing "fai"}
                                     :futuro {:1sing "farò"
                                              :3plur "faranno"
                                              :1plur "faremo"
                                              :2plur "farete"
                                              :3sing "farà"
                                              :2sing "farai"}
                                     :imperfetto {:1sing "facevo"
                                                  :3plur "facevano"
                                                  :1plur "facevamo"
                                                  :2plur "facevate"
                                                  :3sing "faceva"
                                                  :2sing "facevi"}
                                     :passato "fatto"}
                         :essere false
                         :agr {:gender :masc
                               :person :3rd
                               :number :sing
                               :case :nom}
                         :italiano "fare"
                         :infl :past}))))

;; adjectival agreement with number and gender.
(deftest brutte
  (is (= "brutte"
         (italiano/get-string-1 {:cat :adjective
                         :agr {:number :plur
                               :gender :fem}
                         :italiano "brutto"}))))

(deftest brutti
  (is (= "brutti"
         (italiano/get-string-1 {:cat :adjective
                         :agr {:number :plur
                               :gender :top}
                         :italiano "brutto"}))))

(deftest could-have-past
  (is (= "could have made this red wine"
         (english/get-string
          {:a {:modal true
               :irregular {:past "could have"}
               :cat :verb
               :infinitive "to be able to"
               :infl :past}
           :b {:a {:infinitive "to make"
                   :cat :verb
                   :irregular {:past "made"}
                   :infl :infinitive}
               :b "this red wine"}}))))


(deftest could-have-past-participle
  (is (= "could have done this homework"
         (english/get-string
          {:a {:modal true
               :irregular {:past "could have"}
               :cat :verb
               :infinitive "to be able to"
               :infl :past}
           :b {:a {:infinitive "to do"
                   :cat :verb
                   :irregular {:past-participle "done"
                               :past "did"}
                   :infl :infinitive}
               :b "this homework"}}))))

(deftest fatto-Milano
  (is (= "fatto Milano"
         (string/trim 
          (italiano/get-string
           {:infl :past
            :a {:cat :verb
                :italiano "fare"
                :essere false
                :irregular {:present {:1sing "facio"
                                      :3plur "fanno"
                                      :1plur "facciamo"
                                      :2plur "fate"
                                      :3sing "fa"
                                      :2sing "fai"}
                            :futuro {:1sing "farò"
                                     :3plur "faranno"
                                     :1plur "faremo"
                                     :2plur "farete"
                                     :3sing "farà"
                                     :2sing "farai"}
                            :imperfetto {:1sing "facevo"
                                         :3plur "facevano"
                                         :1plur "facevamo"
                                         :2plur "facevate"
                                         :3sing "faceva"
                                         :2sing "facevi"}
                            :passato "fatto"}
                :agr {:case {:not :acc}}
                :infl :past}
            :b {:agr {:case {:not :acc}}
                :italiano "Milano"}})))))

(deftest i-was-less-naive-than-you
  (is (= "I was less naive than you"
         (string/trim (english/get-string
                 {:a "I",
                  :b {:a {:infl :present,
                          :irregular {:past {:2plur "were",
                                             :3sing "was",
                                             :1sing "was",
                                             :2sing "were",
                                             :participle "been",
                                             :3plur "were",
                                             :1plur "were"},
                                      :present {:2plur "are",
                                                :3sing "is",
                                                :2sing "are",
                                                :1sing "am",
                                                :3plur "are",
                                                :1plur "are"}},
                          :agr {:person :1st,
                                :number :sing,
                                :case :nom},
                          :infinitive "to be",
                          :hidden true,
                          :cat :verb}
                      :b {:a {:cat :verb,
                              :infinitive "to be",
                              :irregular {:past {:2plur "were",
                                                 :3sing "was",
                                                 :1sing "was",
                                                 :2sing "were",
                                                 :participle "been",
                                                 :3plur "were",
                                                 :1plur "were"},
                                          :present {:2plur "are",
                                                    :3sing "is",
                                                    :2sing "are",
                                                    :1sing "am",
                                                    :3plur "are",
                                                    :1plur "are"}},
                              :agr {:person :1st,
                                     :number :sing,
                                     :case :nom},
                              :infl :past}

                          :b {:a "less",
                              :b {:a {:cat :adjective, :english "naive"},
                                  :b {:a "than",
                                      :b "you"}}},
                          :infl :past,
                          :agr :top,
                          },
                      :infl :present,
                      :agr {:person :1st,
                            :number :sing,
                            :case :nom},
                      }
                  }
                 )))))

(deftest bianchi
  (is (= "bianchi"
         (string/trim (italiano/get-string
                 {:irregular {:fem {:plur "bianche"},
                              :masc {:plur "bianchi"}},
                  :agr {:person :3rd,
                        :number :plur,
                        :gender :masc,
                        :case :nom},
                  :italiano "bianco",
                  :cat :adjective})))))

(deftest i-cani-sono-bianchi
  (is (= "i cani sono bianchi"
         (string/trim (italiano/get-string
                       {:a {:a "i",
                            :b {:agr {:person :3rd,
                                      :number :plur,
                                      :gender :masc,
                                      :case :nom},
                                :italiano "cane",
                                :cat :noun}},
                        :b {:a {:cat :verb,
                                :italiano "essere",
                                :irregular {:imperfetto {:2plur "eravate",
                                                         :3sing "era",
                                                   :2sing "eri",
                                                         :1sing "ero",
                                                         :3plur "erano",
                                                         :1plur "eravamo"},
                                            :passato "stato",
                                            :present {:2plur "siete",
                                                      :3sing "è",
                                                      :2sing "sei",
                                                      :1sing "sono",
                                                      :3plur "sono",
                                                      :1plur "siamo"},
                                            :futuro {:2plur "sarete",
                                                     :3sing "sarà",
                                                     :2sing "sarai",
                                                     :1sing "sarò",
                                                     :3plur "saranno",
                                                     :1plur "saremo"}},
                                :essere true,
                                :agr {:person :3rd,
                                      :number :plur,
                                      :gender :masc,
                                      :case :nom},
                                :infl :present},
                            :infl :present,
                      :agr {:person :3rd,
                            :number :plur,
                            :gender :masc,
                            :case :nom},
                            :b {:irregular {:fem {:plur "bianche"},
                                            :masc {:plur "bianchi"}},
                                :agr {:person :3rd,
                                      :number :plur,
                                      :gender :masc,
                                      :case :nom},
                          :italiano "bianco",
                                :cat :adjective}}})))))

(deftest essere-format-1
  (let [essere-test
        {:synsem {:subcat {:1 {:cat :noun}, :2 {:sem {:discrete false, :activity false}, :cat :adjective, :subcat {:1 {:cat :noun}, :2 ()}}}, :essere true, :sem {:discrete false, :activity false}, :cat :verb}, :english {:irregular {:present {:1sing "am", :3plur "are", :1plur "are", :2plur "are", :3sing "is", :2sing "are"}, :past {:1sing "was", :3sing "was", :2plur "were", :1plur "were", :3plur "were", :participle "been", :2sing "were"}}, :infinitive "to be"}, :italiano {:essere true, :irregular {:present {:1sing "sono", :3plur "sono", :1plur "siamo", :2plur "siete", :3sing "è", :2sing "sei"}, :futuro {:1sing "sarò", :3plur "saranno", :1plur "saremo", :2plur "sarete", :3sing "sarà", :2sing "sarai"}, :imperfetto {:1sing "ero", :3plur "erano", :1plur "eravamo", :2plur "eravate", :3sing "era", :2sing "eri"}, :passato "stato"}, :italiano "essere"}, :notes "essere-adjective"}]
    (is (= (italiano/get-string-1 (get-in essere-test '(:italiano)))
           "essere"))
    (is (= (english/get-string-1 (get-in essere-test '(:english)))
           "to be"))))

(deftest essere-format-2
  (let [essere-test {:english {:irregular {:present {:1sing "am", :3plur "are", :1plur "are", :2plur "are", :3sing "is", :2sing "are"}, :past {:1sing "was", :3sing "was", :2plur "were", :1plur "were", :3plur "were", :participle "been", :2sing "were"}}, :infl :present, :agr {:case {:not :acc}}, :infinitive "to be", :hidden true}, :italiano {:italiano "essere", :infl :present, :aux true, :essere true, :agr {:case {:not :acc}}, :irregular {:present {:1sing "sono", :3plur "sono", :1plur "siamo", :2plur "siete", :3sing "è", :2sing "sei"}, :futuro {:1sing "sarò", :3plur "saranno", :1plur "saremo", :2plur "sarete", :3sing "sarà", :2sing "sarai"}, :imperfetto {:1sing "ero", :3plur "erano", :1plur "eravamo", :2plur "eravate", :3sing "era", :2sing "eri"}, :passato "stato"}}, :synsem {:cat :verb, :essere true, :aux true, :sem {:tense :past, :subj :top}, :subcat {:1 {:cat :noun, :sem :top, :agr {:case {:not :acc}}}, :2 {:infl :past, :subcat {:1 {:cat :noun, :sem :top, :agr {:case {:not :acc}}}, :2 ()}, :cat :verb, :sem {:tense :past, :subj :top}, :essere true}}, :infl :present}, :notes "essere-aux"}]
    (is (= (italiano/get-string-1 (get-in essere-test '(:italiano)))
           "essere"))))

(deftest did-well-past
  (let [did-well-past {:cat :verb,
                       :b {:english "well"},
                       :a {:irregular {:present {:2plur "do",
                                                 :3sing "does",
                                                 :2sing "do",
                                                 :1sing "do",
                                                 :3plur "do",
                                                 :1plur "do"},
                                       :past "did",
                                       :past-participle "done"},
                           :agr {:gender :masc,
                                 :number :sing,
                                 :person :2nd,
                                 :case :nom},
                           :infl :infinitive,
                           :infinitive "to do"},
                       :infl :past,
                       :agr {:gender :masc,
                             :number :sing,
                             :person :2nd,
                             :case :nom}}]
    (is (= (english/get-string-1 did-well-past) "did well"))))

(deftest le-professoresse-robuste
  (let [lpr {:a "le"
            :b {:a {:agr {:person :3rd, 
                          :gender :fem,
                          :case :top, 
                          :number :plur}, 
                    :initial true, 
                    :italiano "professoressa", 
                    :cat :noun}
                :b {:initial false, 
                    :italiano "robusto", 
                    :cat :adjective, 
                    :agr {:person :3rd, 
                          :gender :fem, 
                          :case :top, 
                          :number :plur}}
                :initial false}, 
            :person :3rd, 
            :gender :fem, 
            :case :top, 
            :number :plur}]
    (is (= (trim (italiano/get-string lpr)) "le professoresse robuste"))))


(deftest la-professoressa-corta
  (let [lpr {:a "la"
            :b {:a {:agr {:person :3rd, 
                          :gender :fem,
                          :case :top, 
                          :number :sing}, 
                    :initial true, 
                    :italiano "professoressa", 
                    :cat :noun}
                :b {:initial false, 
                    :italiano "corto", 
                    :cat :adjective, 
                    :agr {:person :3rd, 
                          :gender :fem, 
                          :case :top, 
                          :number :sing}}
                :initial false}, 
            :person :3rd, 
            :gender :fem, 
            :case :top, 
            :number :sing}]
    (is (= (trim (italiano/get-string lpr)) "la professoressa corta"))))

(deftest la-casa-nuova
  (let [la-casa-nuova {:a "la"
                       :b {:a {:agr {:case :top, 
                                     :number :sing, 
                                     :person :3rd, 
                                     :gender :fem},
                               :initial true, 
                               :italiano "casa", 
                               :cat :noun}
                           :b {:italiano "nuovo", 
                               :initial false, 
                               :cat :adjective, 
                               :agr {:case :top
                                     :number :sing
                                     :gender :fem
                                     :person :3rd}}
                           :initial false}, 
                       :person :3rd, 
                       :gender :fem, 
                       :case :top, 
                       :number :sing}]
    (is (= (string/trim (italiano/get-string la-casa-nuova)) "la casa nuova"))))

(deftest ai-tuoi-stradali 
  (let [ai-tuoi-stradali {:a {:italiano "a", :initial true}, 
                          :b {:b {:italiano "stradale", :cat :noun, 
                                  :agr {:gender :masc, :person :3rd, 
                                        :case :top, :number :plur}, 
                                  :initial false},
                              :initial false, 
                              :a "i tuoi"}}]
    (is (= (string/trim (italiano/get-string ai-tuoi-stradali))
           "ai tuoi stradali"))))

(deftest io-sono-stato-alto
  ;; TODO: test also: "io sono stato alto di voi"
  (let [io-sono-stato-alto
        ;; TODO: pretty-print (or better, find way to auto-pretty print):
        {:a {:initial true, :italiano "io"},
         :b {:b {:b {:italiano "alto",
                     :cat :adjective,
                     :agr {:case :nom,
                           :number :sing,
                           :gender :top,
                           :person :1st},
                     :initial false},
                 :a {:essere true,
                     :infl :past,
                     :initial true,
                     :irregular {:passato "stato",
                                 :present {:2plur "siete",
                                           :3sing "è",
                                           :2sing "sei",
                                           :1sing "sono",
                                           :3plur "sono",
                                           :1plur "siamo"}},
                     :italiano "essere",
                     :agr {:case :nom,
                           :number :sing,
                           :gender :top,
                           :person :1st}},
                 :initial false},
             :a {:agr {:case :nom,
                       :number :sing,
                       :gender :top,
                       :person :1st},
                 :italiano "essere",
                 :essere true,
                 :aux true,
                 :initial true,
                 :irregular {:passato "stato",
                             :present {:2plur "siete",
                                       :3sing "è",
                                       :2sing "sei",
                                       :1sing "sono",
                                       :3plur "sono",
                                       :1plur "siamo"},
                             :futuro {:2plur "sarete",
                                      :3sing "sarà",
                                      :2sing "sarai",
                                      :1sing "sarò",
                                      :3plur "saranno",
                                      :1plur "saremo"}},
                 :infl :present},
             :initial false}}]
    (is (= (string/trim (italiano/get-string io-sono-stato-alto))
           "io sono stato alto"))))

(deftest object-agreement
  "if the object is a pronoun, then agreement is with it, rather than with the subject.
so in this example, will be feminine rather than masculine."
  (let [ricordata
        {:agr {:number :sing,
               :gender :masc
               :case :nom,
               :person :1st},
         :essere false,
         :italiano "ricordare"
         :initial false,
         :obj-agr {:number :sing,
                   :gender :fem,
                   :person :3rd,
                   :case :acc},
         :infl :past}]
    (is (= (string/trim (italiano/get-string ricordata))
           "ricordata"))))

(deftest analyze-1
  (let [analysis (italiano/analyze "dormirà"
                                   (fn [canonical-form]
                                     '({})) nil)]
    (is (= (get-in (first analysis) [:italiano :agr :number])
           :sing))))
