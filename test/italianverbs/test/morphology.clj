(ns italianverbs.test.morphology
  (:use [clojure.test])
  (:use [italianverbs.morphology])
  (:require
   [clojure.string :as string]))

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

;; partial morphology: what to show when there's not
;; enough constraints to conjugate words

(deftest see
  (is (= "to see"
         (get-english-1
          {:irregular {:past "seen"}
           :infl :present
           :agr {:case {:not :acc}}
           :infinitive "to see"}))))

(deftest to-see-me
  (is (= "to see me"
         (get-english-1
          {:a {:irregular {:past "seen"}
               :infl :present
               :agr {:case {:not :acc}}
               :infinitive "to see"}
           :b "me"}))))

(deftest were
  (is (= "[were]"
         (get-english-1
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
         (get-english-1
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
         (get-english-1
          {:a {:irregular {:past "seen"}, :infl :top, :agr {:case {:not :acc}}, :infinitive "to see"}, :b "her"}))))


(deftest professori
  (is (= "professori"
         (get-italian-1 {:agr {:gender :masc
                               :person :3rd
                               :case :top
                               :number :plur}
                         :cat :noun
                         :italian "professore"}))))

(deftest fatto
  (is (= "fatto"
         (get-italian-1 {:cat :verb
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
                         :infinitive "fare"
                         :infl :past}))))

;; adjectival agreement with number and gender.
(deftest brutte
  (is (= "brutte"
         (get-italian-1 {:cat :adjective
                         :agr {:number :plur
                               :gender :fem}
                         :italian "brutto"}))))

(deftest brutti
  (is (= "brutti"
         (get-italian-1 {:cat :adjective
                         :agr {:number :plur
                               :gender :top}
                         :italian "brutto"}))))

(deftest could-have-past
  (is (= "could have made this red wine"
         (get-english
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
         (get-english
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
         (strip (get-italian
                 {:infl :past
                  :a {:cat :verb
                      :infinitive "fare"
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
                      :italian "Milano"}})))))

(deftest i-was-less-naive-than-you
  (is (= "I was less naive than you"
         (strip (get-english
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
         (strip (get-italian
                 {:irregular {:fem {:plur "bianche"},
                              :masc {:plur "bianchi"}},
                  :agr {:person :3rd,
                        :number :plur,
                        :gender :masc,
                        :case :nom},
                  :italian "bianco",
                  :cat :adjective})))))

(deftest i-cani-sono-bianchi
  (is (= "i cani sono bianchi"
         (strip (get-italian
                 {:a {:a "i",
                      :b {:agr {:person :3rd,
                                :number :plur,
                                :gender :masc,
                                :case :nom},
                          :italian "cane",
                          :cat :noun}},
                  :b {:a {:cat :verb,
                          :infinitive "essere",
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
                          :italian "bianco",
                          :cat :adjective}}})))))

(deftest essere-format-1
  (let [essere-test
        {:synsem {:subcat {:1 {:cat :noun}, :2 {:sem {:discrete false, :activity false}, :cat :adjective, :subcat {:1 {:cat :noun}, :2 ()}}}, :essere true, :sem {:discrete false, :activity false}, :cat :verb}, :english {:irregular {:present {:1sing "am", :3plur "are", :1plur "are", :2plur "are", :3sing "is", :2sing "are"}, :past {:1sing "was", :3sing "was", :2plur "were", :1plur "were", :3plur "were", :participle "been", :2sing "were"}}, :infinitive "to be"}, :italian {:essere true, :irregular {:present {:1sing "sono", :3plur "sono", :1plur "siamo", :2plur "siete", :3sing "è", :2sing "sei"}, :futuro {:1sing "sarò", :3plur "saranno", :1plur "saremo", :2plur "sarete", :3sing "sarà", :2sing "sarai"}, :imperfetto {:1sing "ero", :3plur "erano", :1plur "eravamo", :2plur "eravate", :3sing "era", :2sing "eri"}, :passato "stato"}, :infinitive "essere"}, :notes "essere-adjective"}]
    (is (= (get-italian-1 (get-in essere-test '(:italian)))
           "essere"))
    (is (= (get-english-1 (get-in essere-test '(:english)))
           "to be"))))

(deftest essere-format-2
  (let [essere-test {:english {:irregular {:present {:1sing "am", :3plur "are", :1plur "are", :2plur "are", :3sing "is", :2sing "are"}, :past {:1sing "was", :3sing "was", :2plur "were", :1plur "were", :3plur "were", :participle "been", :2sing "were"}}, :infl :present, :agr {:case {:not :acc}}, :infinitive "to be", :hidden true}, :italian {:infinitive "essere", :infl :present, :aux true, :essere true, :agr {:case {:not :acc}}, :irregular {:present {:1sing "sono", :3plur "sono", :1plur "siamo", :2plur "siete", :3sing "è", :2sing "sei"}, :futuro {:1sing "sarò", :3plur "saranno", :1plur "saremo", :2plur "sarete", :3sing "sarà", :2sing "sarai"}, :imperfetto {:1sing "ero", :3plur "erano", :1plur "eravamo", :2plur "eravate", :3sing "era", :2sing "eri"}, :passato "stato"}}, :synsem {:cat :verb, :essere true, :aux true, :sem {:tense :past, :subj :top}, :subcat {:1 {:cat :noun, :sem :top, :agr {:case {:not :acc}}}, :2 {:infl :past, :subcat {:1 {:cat :noun, :sem :top, :agr {:case {:not :acc}}}, :2 ()}, :cat :verb, :sem {:tense :past, :subj :top}, :essere true}}, :infl :present}, :notes "essere-aux"}]
    (is (= (get-italian-1 (get-in essere-test '(:italian)))
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
    (is (= (get-english-1 did-well-past) "did well"))))

(deftest le-professoresse-robuste
  (let [lpr {:a "le"
            :b {:a {:agr {:person :3rd, 
                          :gender :fem,
                          :case :top, 
                          :number :plur}, 
                    :initial true, 
                    :italian "professoressa", 
                    :cat :noun}
                :b {:initial false, 
                    :italian "robusto", 
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
    (is (= (get-italian lpr) "le professoresse robuste"))))


(deftest la-professoressa-corta
  (let [lpr {:a "la"
            :b {:a {:agr {:person :3rd, 
                          :gender :fem,
                          :case :top, 
                          :number :sing}, 
                    :initial true, 
                    :italian "professoressa", 
                    :cat :noun}
                :b {:initial false, 
                    :italian "corto", 
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
    (is (= (get-italian lpr) "la professoressa corta"))))

(deftest la-casa-nuova
  (let [la-casa-nuova {:a "la"
                       :b {:a {:agr {:case :top, 
                                     :number :sing, 
                                     :person :3rd, 
                                     :gender :fem},
                               :initial true, 
                               :italian "casa", 
                               :cat :noun}
                           :b {:italian "nuovo", 
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
    (is (= (string/trim (get-italian la-casa-nuova)) "la casa nuova"))))

(deftest ai-tuoi-stradali 
  (let [ai-tuoi-stradali {:a {:italian "a", :initial true}, 
                          :b {:b {:italian "stradale", :cat :noun, 
                                  :agr {:gender :masc, :person :3rd, 
                                        :case :top, :number :plur}, 
                                  :initial false},
                              :initial false, 
                              :a "i tuoi"}}]
    (is (= (string/trim (get-italian ai-tuoi-stradali))
           "ai tuoi stradali"))))

(deftest io-sono-stato-alto
  ;; TODO: test also: "io sono stato alto di voi"
  (let [io-sono-stato-alto
        ;; TODO: pretty-print (or better, find way to auto-pretty print):
        {:a {:initial true, :italian "io"},
         :b {:b {:b {:italian "alto",
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
                     :infinitive "essere",
                     :agr {:case :nom,
                           :number :sing,
                           :gender :top,
                           :person :1st}},
                 :initial false},
             :a {:agr {:case :nom,
                       :number :sing,
                       :gender :top,
                       :person :1st},
                 :infinitive "essere",
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
    (is (= (string/trim (get-italian io-sono-stato-alto))
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
         :infinitive "ricordare"
         :initial false,
         :obj-agr {:number :sing,
                   :gender :fem,
                   :person :3rd,
                   :case :acc},
         :infl :past}]
    (is (= (string/trim (get-italian ricordata))
           "ricordata"))))
