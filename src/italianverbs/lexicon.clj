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

(let [verb {:cat :verb :infl :infinitive}
      present {:infl :present}
      animate {:animate true}
      noun {:cat :noun}
      speakable (fs/m noun {:speakable true})
      edible (fs/m noun {:edible true})
      human (fs/m animate {:human true})
      artifact (fs/m noun {:artifact true})
      third-sing {:number :singular :person :3rd}
      third-sing-subj {:subj third-sing}
      noun {:cat :noun}
      common-noun {:person :3rd :cat :noun}

      cane (add "cane" "dog"
                (fs/m noun
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

      io (add "io" "i" 
              (fs/m
               human
               noun
               {:person :1st :number :singular :case :nom}))

      libro (add "libro" "book"
                (fs/m artifact
                      {:gender :masc}))

      leggere (add "leggere" "to read"
                    (fs/m
                     verb
                     {:subj (fs/m noun {:human true})
                      :obj {:readable true}
                      }))
      
      mangiare (add "mangiare" "to eat"
                    (fs/m
                     verb
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
  
;  (let [noun {:person :3rd
;              :cat :noun}]
;    (add "cane" "dog"
;         (fs/mergec
;          noun
;          {:animate true}))))

;    (add-with-plural "cane" "dog"
;      (fs/mergec 
;       noun
;       {:number :singular
;        :gender :masc
;        :animate true
;        :common true})))

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



