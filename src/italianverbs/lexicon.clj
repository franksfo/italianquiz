(ns italianverbs.lexicon
  (:use [italianverbs.lexiconfn]
        [rdutest])
  (:require
   [italianverbs.fs :as fs]))

;; WARNING: clear blows away entire lexicon in backing store (mongodb).
(clear)

(let [word {:morph "unspecified-morphology"} ;; 'word' not used currently.
      verb
      (let [number-agreement (ref :top)
            person-agreement (ref :top)]
        {:cat :verb :infl :infinitive
         :number number-agreement
         :person person-agreement
         :subj {:number number-agreement
                :person person-agreement
                :case {:not :acc}}
         :passato-aux2
         {:root {:italian "avere"
                 :cat :verb}
          :number number-agreement
          :person person-agreement}})
      animate {:animate true}
      det {:cat :det}
      human (fs/merge animate {:human true})
      masc {:gender :masc}
      transitive (fs/merge verb {:obj {:case {:not :nom}}})
      present {:infl :present}
      singular {:number :singular}
      plural {:number :plural}

      ;; TODO: graduate common-noun content into noun.
      noun {:cat :noun}
      third-person {:person :3rd :cat :noun}
      third-sing (fs/merge third-person {:number :singular})

      ;; 'morph-noun' and 'take-article' are defined in generate.clj.
      common-noun
      (fs/merge third-person
            (let [number-agreement (ref :top)
                  gender-agreement (ref :top)]
              {:comp {:cat :det
                      ;; determiner must agree with number of noun:
                      :number number-agreement
                      :gender gender-agreement
                      }
               :morph "morph-noun"
               :common true
               :gender gender-agreement
               :number number-agreement}))

      takes-masc-sing-determiner {:comp {:gender :masc :number :singular}}

      pronoun (fs/merge noun {:pronoun true :comp nil :human true})
      speakable (fs/merge common-noun {:speakable true})
      readable (fs/merge common-noun {:readable true})
      edible (fs/merge common-noun {:edible true})
      mass {:mass true :comp {:def {:not :indef}}} ; you can say 'the pasta', but not 'a pasta'.

      third-sing-subj {:subj third-sing}

      artifact (fs/merge common-noun {:artifact true})
      masc {:gender :masc}
      fem {:gender :fem}

      avere
      (let [avere (add "avere" "to have"
                        transitive
                        {:subj {:cat :noun :human true}
                         :obj {:cat :noun}})]
        (add "ho" "have"
             avere
             {:root avere}
             {:infl :present}
             {:subj {:number :singular
                     :person :1st}})
        (add "hai" "have"
             avere
             {:root avere}
             {:infl :present}
             {:subj {:number :singular
                     :person :2nd}})
        (add "ha" "has"
             avere
             {:root avere}
             {:infl :present}
             {:subj {:number :singular
                     :person :3rd}})
        (add "abbiamo" "have"
             avere
             {:root avere}
             {:infl :present}
             {:subj {:number :plural
                     :person :1st}})
        (add "avete" "have"
             avere
             {:root avere}
             {:infl :present}
             {:subj {:number :plural
                     :person :2nd}})
        (add "hanno" "have"
             avere
             {:root avere}
             {:infl :present}
             {:subj {:number :plural
                     :person :3rd}}))

      calcio (add "calcio" "soccer"
                  third-sing masc mass
                  {:comp :nil!
                   :sport true})
      
      cane (add "cane" "dog"
                common-noun masc
                {:animate true
                 :number :singular})

;; needs supports for reflexive pronouns: "mi chiamo gino".
;      (add "chiamare" "to be named"
;           {:cat :verb :infl :infinitive
;            :subj human})


      dei (add "dei" "some" masc plural det {:def :indef})
      
      dimenticare (add "dimenticare" "to forget"
                       transitive
                       {:subj animate
                        :obj {:cat :noun}})

      donna (add "donna" "woman" (fs/copy common-noun) fem human singular)

      ;; add exception because of english: woman->women.
      donne (add "donne" "women" donna plural {:root donna})

      essere
      (let [essere (add "essere" "to be"
                        transitive
                        {:subj {:cat :noun}
                         :obj {:cat :noun}})]
        (add "sono" "am"
             essere
             {:root essere}
             {:infl :present}
             {:subj {:number :singular
                     :person :1st}})
        (add "sei" "are"
             essere
             {:root essere}
             {:infl :present}
             {:subj {:number :singular
                     :person :2nd}})
        (add "è" "is"
             essere
             {:root essere}
             {:infl :present}
             {:subj {:number :singular
                     :person :3rd}})
        (add "siamo" "are"
             essere
             {:root essere}
             {:infl :present}
             {:subj {:number :plural
                     :person :1st}})
        (add "siete" "are"
             essere
             {:root essere}
             {:infl :present}
             {:subj {:number :plural
                     :person :2nd}})
        (add "sono" "are"
             essere
             {:root essere}
             {:infl :present}
             {:subj {:number :plural
                     :person :3rd}}))


      fare (let [fare (add "fare" "to make"
                           verb
                           {:subj (fs/merge noun {:human true})
                            :obj artifact})]
             
             (add "facio" "make"
                  fare
                  {:root fare}
                  present
                  {:subj {:number :singular
                          :person :1st}})
                 
             (add "fai" "make"
                  fare
                  {:root fare}
                  present
                  {:subj {:number :singular
                          :person :2nd}})
                
             (add "fa" "makes"
                  fare
                  {:root fare}
                  present
                  {:subj {:number :singular
                          :person :3rd}})
             
             (add "facciamo" "make"
                  fare
                  {:root fare}
                  present
                  {:subj {:number :plural
                          :person :1st}})
             
             (add "fate" "make"
                  fare
                  {:root fare}
                  present
                  {:subj {:number :plural
                          :person :2nd}})

             (add "fanno" "make"
                  fare
                  {:root fare}
                  present
                  {:subj {:number :plural
                          :person :3rd}})

             
             )

     giocare (add "giocare" "to play"
                  verb
                  {:subj human
                   :obj (fs/merge noun
                              {:sport true})})
     
     il (add "il" "the" {:gender :masc :number :singular :cat :det
                         :def :def})
     i (add "i" "the" masc plural det {:def :def})
     
     io (add "io" "i" 
             human
             pronoun
             {:person :1st :number :singular :case :nom})

     
     la (add "la" "the" {:gender :fem :number :singular :cat :det
                         :def :def})

     lavorare (add "lavorare" "to work"
                   verb
                   {:subj human})
     

     le (add "le" "the" {:gender :fem :number :plural :cat :det
                         :def :def})
     
     letto (add "letto" "bed"
                artifact masc
                {:furniture true
                 :ruggable true}) ;; ruggable: can be placed on top of a rug.

     libro (add "libro" "book"
                artifact readable masc)

     leggere (add "leggere" "to read"
                  transitive
                  {:subj (fs/merge noun {:human true})
                   :obj (fs/merge noun {:readable true})})

     lei (add "lei" "she" 
              human pronoun fem
              {:person :3rd :number :singular :case :nom })

     loro (add "loro" "they" 
             human
             pronoun
             {:person :3rd :number :plural :case :nom})
     
     lui (add "lui" "he" 
              human pronoun masc
              {:person :3rd :number :singular :case :nom })
     
     mangiare (add "mangiare" "to eat"
                   transitive
                   {:subj (fs/merge noun {:animate true})
                    :obj edible}) 
     mi (add "mi" "me"
             pronoun
             {:person :1st :number :singular :case :acc})

     noi (add "noi" "we" 
             human
             pronoun
             {:person :1st :number :plural :case :nom})
     
      pane (add "pane" "bread"
               artifact mass
               {:edible true
                :gender :masc})
      
      parlare (add "parlare" "to speak"
                    verb
                   {:subj (fs/merge noun {:human true})
                    :obj speakable})


      parola (add "parola" "word"
                  common-noun
                  {:readable true
                   :speakable true
                   :gender :fem})

      pasta (add "pasta" "pasta"
                artifact mass
                {:edible true
                 :gender :fem})

      poltrona (add "poltrona" "easy chair"
                  fem artifact
                  {:holdable true ;; barely holdable (if you're strong or there's more than one of you) :)
                   :furniture true
                   :ruggable true}) ;; ruggable: can be placed on top of a rug.

      ragazza (add "ragazza" "girl" (fs/copy common-noun) fem human)

      ragazzo (add "ragazzo" "guy" (fs/copy common-noun) masc human)
      sedia (add "sedia" "chair"
                  fem artifact
                  {:holdable true 
                   :furniture true
                   :ruggable true}) ;; ruggable: can be placed on top of a rug.
      
      tavolo (add "tavolo" "table"
                  masc artifact
                  {:holdable true ;; barely holdable (if you're strong or there's more than one of you) :)
                   :furniture true
                   :ruggable true}) ;; ruggable: can be placed on top of a rug.

      tavolino (add "tavolino" "coffee table"
                  masc artifact
                  {:holdable true ;; barely holdable (if you're strong or there's more than one of you) :)
                   :furniture true
                   :ruggable true}) ;; ruggable: can be placed on top of a rug.
      
      tu (add "tu" "you" 
              human
              pronoun
              {:person :2nd :number :singular :case :nom})

      un (add "un" "a" {:gender :masc :number :singular :cat :det
                        :def :indef})
      
      una (add "una" "a" {:gender :fem :number :singular :cat :det
                          :def :indef})

      uomo (add "uomo" "man" (fs/copy common-noun) masc human)
      ;; exception because in english man->men.
      uomini (add "uomini" "men" uomo plural {:root uomo})
      
      voi (add "voi" "you all" 
              human
              pronoun
              {:person :2nd :number :plural :case :nom})
      
      ]
  (def variables
    {:common-noun common-noun :takes-masc-sing-determiner takes-masc-sing-determiner :hanno (lookup "hanno")
     :calcio (lookup "calcio") :fanno (lookup "fanno")
     :artifact artifact :masc masc :readable readable
     :letto letto :libro libro}))

(add "gennario" "january"
     {:month true})


(add "febbraio" "february"
     {:month true})
(add "marzo" "march"
     {:month true})
(add "aprile" "april"
     {:month true})
(add "maggio" "may"
     {:month true})
(add "giugno" "june"
     {:month true})
(add "luglio" "july"
     {:month true})
(add "agosto" "august"
     {:month true})
(add "settembre" "september"
     {:month true})
(add "ottobre" "october"
     {:month true})
(add "novembre" "november"
     {:month true})
(add "dicembre" "december"
     {:month true})

(add "lunedì" "monday"
     {:giorni-della-settimana true})
(add "martedì" "tuesday"
     {:giorni-della-settimana true})
(add "mercoledì" "wednesday"
     {:giorni-della-settimana true})
(add "giovedì" "thursday"
     {:giorni-della-settimana true})
(add "venerdì" "friday"
     {:giorni-della-settimana true})
(add "sabato" "saturday"
     {:giorni-della-settimana true})
(add "domenica" "sunday"
     {:giorni-della-settimana true})

;; prepotizioni (prepositions)
(add "in" "to"
	   {:cat :prep
        :obj {:case {:not :nom}
              :andare-in true}})

(add "in" "in"
     {:cat :prep
      :action-occurring-in true
      :obj {:case {:not :nom}
            :english-in true
            :place true}})

(add "in" "at"
     {:cat :prep
      :action-occurring-in true
      :obj {:case {:not :nom}
            :english-at true
            :place true}})

(add "a" "to"
     {:cat :prep
      :obj {:case {:not :nom}
            :andare-a true}})

(add "a" "to"
     {:cat :prep
      :obj {:case {:not :nom}
            :andare-al true}})

(add "di" "of"
     {:cat :prep
      :obj {:case {:not :nom}}})

(add "da" "from"
     {:cat :prep
      :obj {:case {:not :nom}
            :place true}})

(add "a" "to"
     {:cat :prep
      :obj {:case {:not :nom}
            :animate true}})

(add "con" "with"
     {:cat :prep
      :obj {:case {:not :nom}
            :human true}})

(add "per" "for"
     {:cat :prep
      :benefactive true
      :obj {:case {:not :nom}
            :animate true}})

(add "per" "for"
     {:cat :prep
      :benefactive true
      :obj {:case {:not :nom}
            :human true}})

(add "su" "on"
     {:cat :prep
      :furniture true
      :obj {:case {:not :nom}}})

(add "proprio accanto a" "right next to"
     {:cat :prep
      :furniture true
      :landscape-prep true}) ;; e.g. "the post office is right next to the bank".

;; furniture prepositions.
;; note that the query language currently might not support nested constraints like [:obj [:furniture true]]

(add "a destra de" "to the right of"
     {:cat :prep
      :furniture true
      :obj {:case {:not :nom}
            :furniture true}})

(add "a sinistra de" "to the left of"
     {:cat :prep
      :furniture true
      :obj {:case {:not :nom}
            :furniture true}})

(add "accanto a" "next to"
     {:cat :prep
      :furniture true
      :obj {:case {:not :nom}
            :furniture true}})

(add "dentro" "inside"
     {:cat :prep
      :furniture true
      :subj {:holdable true}
      :obj {:case {:not :nom}
            :furniture true}})

(add "dietro" "behind"
     {:cat :prep
      :furniture true
      :obj {:case {:not :nom}
            :furniture true}})

(add "davanti a" "in front of"
     {:cat :prep
      :furniture true
      :obj {:furniture true}})

;; "le sedie sono intorno al tavolino
;;  (the chairs are around the the table)"
(add "intorno a" "around"
     {:cat :prep
      :furniture true
      :subj {:number :plural
             :furniture true}
      :obj {:number :sing
            :furniture true}})

(add "sopra" "above"
     {:cat :prep
      :furniture true
      :obj {:case {:not :nom}
            :furniture true}})

(add "sotto" "under"
     {:cat :prep
      :furniture true
      :obj {:case {:not :nom}
            :furniture true}})

(add "su" "on"
     {:cat :prep
      :furniture true
      :obj {:case {:not :nom}
            :furniture true}})

;; end of lexicon.
;; begin tests.

(def localtests ;; so as not to collide with lexiconfn/tests.
  (list
   (rdutest
    "A lexical entry for the word: 'parlare'."
    (lookup "parlare")
    (fn [parlare]
      (= (:italian parlare) "parlare"))
    :test-lex-parla)
   :calcio
   (rdutest
    "calcio is a noun that does not take an article."
    (lookup "calcio")
    (fn [calcio]
      (or (= :nil! (:comp calcio))
          (= "nil!" (:comp calcio))))
    :calcio)

   ;;
   ;; [:cat :noun
   ;;  :number [1] :singular
   ;;  :comp [:cat :det
   ;;         :number [1] ] ]

   (rdutest
    "test that number and gender agreement of nouns, as implemented using references, works."
    ;; :number is shared by the paths (:number) and (:comp :number).
    (lookup "cane")
    (fn [dog]
      (and

       ;; sanity checks: not related to reentrances.
       (not (nil? dog))
       ;; Ideally these subtests would work for the keyword,
       ;; since lexicon.clj uses keywords for symbols.
       ;; But for now, we have to test for "det" because of
       ;; database serialization.
       (or (= (get-in dog (list :cat))
              :noun)
           (= (get-in dog (list :cat))
              "noun"))
       (or (= (get-in dog (list :comp :cat))
              :det)
           (= (get-in dog (list :comp :cat))
              "det"))

       ;; test referential equality:
       (= (type (get-in dog '(:number))) clojure.lang.Ref)

       (= (get-in dog '(:number))
          (get-in dog '(:comp :number)))

       ;; as above with respect to keyword vs string.
       (or (= @(get-in dog '(:number)) :singular)
           (= @(get-in dog '(:number)) "singular"))))

    :noun-agreement)

   (rdutest
    "test that number and person agreement of verbs, as implemented using references, works."
    ;; :number is shared by the paths (:number) and (:comp :number).
    ;; :person is shared by the paths (:person) and (:comp :person).
    (lookup "avere")
    (fn [to-have]
      (and

       ;; sanity checks: not related to reentrances.
       (not (nil? to-have))
       ;; Ideally these subtests would work for the keyword,
       ;; since lexicon.clj uses keywords for symbols.
       ;; But for now, we have to test for "det" because of
       ;; database serialization.
       (or (= (get-in to-have (list :cat))
              :verb)
           (= (get-in to-have (list :cat))
              "verb"))
       (or (= (get-in to-have (list :obj :cat))
              :noun)
           (= (get-in to-have (list :obj :cat))
              "noun"))

       ;; test referential equality:
       (= (type (get-in to-have '(:number))) clojure.lang.Ref)

       (= (get-in to-have '(:number))
          (get-in to-have '(:subj :number)))

       ;; subject and verb must agree in person..
       (= (get-in to-have '(:person))
          (get-in to-have '(:subj :person)))

       ;;.. and number.
       (= (get-in to-have '(:number))
          (get-in to-have '(:subj :number)))))
       :verb-agreement)
   ))

