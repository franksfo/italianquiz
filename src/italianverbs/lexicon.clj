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

(let [verb {:cat :verb}
      animate {:animate true}
      human (fs/m animate {:human true})
      animate-subj {:subj animate}
      human-subj {:subj human}
      third-sing {:number :singular :person :3rd}
      third-sing-subj {:subj third-sing}

      parlare (add "parlare" "to speak"
                   (fs/m
                    verb
                    human-subj
                    {
                     :obj {:speakable true}
                     }))

      mangiare (add "mangiare" "to eat"
                    (fs/m
                     verb
                     animate-subj
                     {
                      :obj {:edible true}
                      }))

      parla (add "parla" "he speaks"
                 (fs/m
                  parlare
                  third-sing-subj
                  {:root parlare}))

      parlo (add "parlo" "i speak"
                 (fs/m
                  parlare
                  {:person :1st
                   :number :singular}))
  
      gennario (add "gennario" "january"
                    {:month true})

      noun {:person :3rd :cat :noun}
      cane (add "cane" "dog"
                (fs/m noun
                      {:animate true}
                      {:gender :masc}))

      io (add "io" "i" 
              (fs/m
               human
               noun
               {:person :1st :number :singular :case :nom}))

      mi (add "mi" "me"
              (fs/m
               noun
               {:person :1st :number :singular :case :acc}))]
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


(def localtests ;; so as not to collide with lexiconfn/tests.
  {:parla
   (rdutest
    "A lexical entry for the word: 'parlare'."
    (lookup "parlare")
    (fn [parlare]
      (= (:italian parlare) "parlare"))
    :parla)})



