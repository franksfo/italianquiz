(ns italianverbs.lexicon
  (:use [italianverbs.lexiconfn]
        [italianverbs.rdutest])
  (:require [italianverbs.fs :as fs]
            [italianverbs.search :as search]))

;; WARNING: clear blows away entire lexicon in backing store (mongodb).
(clear)

;; this (merge) doesn't work: need to learn to write wrappers.
;; until then, using fs/m below.
(defn merge [& [args]]
  (fs/merge-like-core args))

(let [verb {:cat :verb :infl :infinitive :subj {:case {:not :acc}}}
      animate {:animate true}
      det {:cat :det}
      human (fs/m animate {:human true})
      masc {:gender :masc}
      transitive (fs/m verb {:obj {:case {:not :nom}}})
      present {:infl :present}
      singular {:number :singular}
      plural {:number :plural}
      
      noun {:cat :noun}
      pronoun (fs/m noun) ;; these do not take a determiner.
      speakable (fs/m noun {:speakable true})
      readable (fs/m noun {:readable true})
      edible (fs/m noun {:edible true})

      third-sing {:number :singular :person :3rd :cat :noun}
      third-sing-subj {:subj third-sing}
      common-noun (fs/m third-sing {:det true})
      artifact (fs/m common-noun {:artifact true})
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


      
      calcio (add "il calcio" "soccer"
                  common-noun masc
                  {:det false
                   :sport true})
      
      cane (add "cane" "dog"
                common-noun
                {:animate true
                 :gender :masc})

;; needs supports for reflexive pronouns: "mi chiamo gino".
;      (add "chiamare" "to be named"
;           {:cat :verb :infl :infinitive
;            :subj human})
      
      dimenticare (add "dimenticare" "to forget"
                       transitive
                       {:subj animate
                        :obj {:cat :noun}})

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
                           {:subj (fs/m noun {:human true})
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
                    :obj (fs/m noun
                               {:sport true})})
      
      il (add "il" "the" {:gender :masc :number :singular :cat :det
                          :def :def})
      
      la (add "la" "the" {:gender :fem :number :singular :cat :det
                          :def :def})

      le (add "le" "the" {:gender :fem :number :plural :cat :det
                          :def :def})

      i (add "i" "the" masc plural det {:def :def})
      
      io (add "io" "i" 
              human
              pronoun
              {:person :1st :number :singular :case :nom})


      letto (add "letto" "bed"
                  common-noun masc artifact
                  {:furniture true
                   :ruggable true}) ;; ruggable: can be placed on top of a rug.

      libro (add "libro" "book"
                 artifact readable masc
                 {:on {:ruggable true}})

      leggere (add "leggere" "to read"
                   transitive
                   {:subj (fs/m noun {:human true})
                    :obj (fs/m noun {:readable true})})

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
                    {:subj (fs/m noun {:animate true})
                     :obj edible})

      mi (add "mi" "me"
              noun
              {:person :1st :number :singular :case :acc})

      noi (add "noi" "we" 
              human
              pronoun
              {:person :1st :number :plural :case :nom})
      
      pane (add "pane" "bread"
                artifact
                {:edible true
                 :gender :masc})

      pane (add "pasta" "pasta"
                artifact
                {:edible true
                 :gender :fem})

      parlare (add "parlare" "to speak"
                   verb
                   {:subj (fs/m noun {:human true})
                    :obj speakable})

      parola (add "parola" "word"
                  common-noun
                  {:readable true
                   :speakable true
                   :gender :fem})

      poltrona (add "poltrona" "easy chair"
                  common-noun fem artifact
                  {:holdable true ;; barely holdable (if you're strong or there's more than one of you) :)
                   :furniture true
                   :ruggable true}) ;; ruggable: can be placed on top of a rug.

      sedia (add "sedia" "chair"
                  common-noun fem artifact
                  {:holdable true 
                   :furniture true
                   :ruggable true}) ;; ruggable: can be placed on top of a rug.
      
      tavolo (add "tavolo" "table"
                  common-noun masc artifact
                  {:holdable true ;; barely holdable (if you're strong or there's more than one of you) :)
                   :furniture true
                   :ruggable true}) ;; ruggable: can be placed on top of a rug.

      tavolino (add "tavolino" "coffee table"
                  common-noun masc artifact
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

      voi (add "voi" "you all" 
              human
              pronoun
              {:person :2nd :number :plural :case :nom})

      
      
      ]
  ) ;; top-level (let).

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


(def localtests ;; so as not to collide with lexiconfn/tests.
  {:parlare
   (rdutest
    "A lexical entry for the word: 'parlare'."
    (lookup "parlare")
    (fn [parlare]
      (= (:italian parlare) "parlare"))
    :parla)})



