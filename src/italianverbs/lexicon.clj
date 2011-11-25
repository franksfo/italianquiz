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
      transitive {:obj {:case {:not :nom}}}
      present {:infl :present}
      animate {:animate true}
      noun {:cat :noun}
      pronoun (fs/m noun) ;; these do not take a determiner.
      speakable (fs/m noun {:speakable true})
      readable (fs/m noun {:readable true})
      edible (fs/m noun {:edible true})
      human (fs/m animate {:human true})
      third-sing {:number :singular :person :3rd :cat :noun}
      third-sing-subj {:subj third-sing}
      noun {:cat :noun}
      common-noun (fs/m third-sing {:det true})
      artifact (fs/m common-noun {:artifact true})
      masc {:gender :masc}
      fem {:gender :fem}

      calcio (add "calcio" "soccer"
                  (fs/m common-noun masc
                        {:sport true}))
      
      cane (add "cane" "dog"
                (fs/m common-noun
                      {:animate true}
                      {:gender :masc}))

;; needs supports for reflexive pronouns: "mi chiamo gino".
;      (add "chiamare" "to be named"
;           {:cat :verb :infl :infinitive
;            :subj human})
      
      dimenticare (add "dimenticare" "to forget"
                       (fs/m verb transitive
                       {:subj animate
                        :obj {:cat :noun}}))

      fare (add "fare" "to make"
                (fs/m
                 verb
                 {:subj (fs/m noun {:human true})
                  :obj artifact
                  }))

      fa (add "fa" "makes"
              (fs/m
               fare
               {:root fare}
               present
               {:subj {:number :singular
                       :person :3rd}}))

      facio (add "facio" "make"
                 (fs/m
                  fare
                  {:root fare}
                  present
                  {:subj {:number :singular
                          :person :1st}}))

      facio (add "fai" "make"
                 (fs/m
                  fare
                  {:root fare}
                  present
                  {:subj {:number :singular
                          :person :2nd}}))
      
      giocare (add "giocare" "to play"
                   (fs/m verb
                   {:subj human
                    :obj (fs/m common-noun
                              {:sport true})}))
      
      il (add "il" "the" {:gender :masc :number :singular :cat :det
                       :def :def})

      la (add "la" "the" {:gender :fem :number :singular :cat :det
                       :def :def})

      
      io (add "io" "i" 
              (fs/m
               human
               pronoun
               {:person :1st :number :singular :case :nom}))

      libro (add "libro" "book"
                (fs/m artifact readable masc))

      leggere (add "leggere" "to read"
                    (fs/m
                     transitive verb
                     {:subj (fs/m noun {:human true})
                      :obj (fs/m noun {:readable true})
                      }))

      lei (add "lei" "she" 
              (fs/m
               human pronoun fem
               {:person :3rd :number :singular :case :nom }))

      lui (add "lui" "he" 
              (fs/m
               human pronoun masc
               {:person :3rd :number :singular :case :nom }))

      
      mangiare (add "mangiare" "to eat"
                    (fs/m
                     transitive verb
                     {:subj (fs/m noun {:animate true})
                      :obj edible
                      }))

      mi (add "mi" "me"
              (fs/m
               noun
               {:person :1st :number :singular :case :acc}))
  
      pane (add "pane" "bread"
                (fs/m artifact
                      {:edible true
                       :gender :masc}))

      pane (add "pasta" "pasta"
                (fs/m artifact
                      {:edible true
                       :gender :fem}))

      parlare (add "parlare" "to speak"
                   (fs/m
                    verb
                    {:subj (fs/m noun {:human true})
                     :obj speakable
                     }))

      parola (add "parola" "word"
                (fs/m common-noun
                      {:readable true
                       :speakable true
                       :gender :fem}))

      tu (add "tu" "you" 
              (fs/m
               human
               pronoun
               {:person :2nd :number :singular :case :nom}))

      
      
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
        :obj {:case {:not :nom}}})



(def localtests ;; so as not to collide with lexiconfn/tests.
  {:parlare
   (rdutest
    "A lexical entry for the word: 'parlare'."
    (lookup "parlare")
    (fn [parlare]
      (= (:italian parlare) "parlare"))
    :parla)})



