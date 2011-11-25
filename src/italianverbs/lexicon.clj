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
      
      cane (add "cane" "dog"
                (fs/m common-noun
                      {:animate true}
                      {:gender :masc}))

      fare (add "fare" "to make"
                (fs/m
                 verb
                 {:subj (fs/m noun {:human true})
                  :obj artifact
                  }))

      facio (add "facio" "i make"
                 (fs/m
                  fare
                  {:root fare}
                  present
                  {:subj {:number :singular
                          :person :1st}}))
     
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
                (fs/m artifact readable
                      {:gender :masc}))

      leggere (add "leggere" "to read"
                    (fs/m
                     transitive verb
                     {:subj (fs/m noun {:human true})
                      :obj (fs/m noun {:readable true})
                      }))
      
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
                (fs/m noun
                      {:speakable true
                       :gender :fem}))

      
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

(def localtests ;; so as not to collide with lexiconfn/tests.
  {:parlare
   (rdutest
    "A lexical entry for the word: 'parlare'."
    (lookup "parlare")
    (fn [parlare]
      (= (:italian parlare) "parlare"))
    :parla)})



