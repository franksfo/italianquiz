(ns italianverbs.lexicon
  (:use [italianverbs.lexiconfn]
        [italianverbs.rdutest])
  (:require [italianverbs.fs :as fs]
            [italianverbs.search :as search]))

;; WARNING: clear blows away entire lexicon in backing store (mongodb).
(clear)

(let [verb {:cat :verb}
      animate-subj {:subj {:animate true}}
      human-subj (fs/merge-like-core
                  animate-subj
                  {:subj {:human true}})
      third-sing {:subj {:number :singular :person :3rd}}

      parlare (add "parlare" "to speak"
                   (fs/merge-like-core
                    verb
                    human-subj
                    {
                     :obj {:speakable true}
                     }))

      mangiare (add "mangiare" "to eat"
                    (fs/merge-like-core
                     verb
                     animate-subj
                     {
                      :obj {:edible true}
                      }))

      parla (add "parla" "speaks"
                 (fs/merge-like-core
                  parlare
                  third-sing
                  {:root parlare}))]

  (add "gennario" "january"
       {:month true})

  (let [noun {:person :3rd :cat :noun}]
    (add "cane" "dog" noun)

;    (let [foo (fs/mergec noun {:case :nom})] foo)
    
    (add "io" "i" 
         (fs/merge-like-core
          noun {:case :nom}))

    (add "mi" "me"
         (fs/merge-like-core
          noun
          {:person :1st :number :singular :case :acc}))



;    )

  









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

  )

(def localtests ;; so as not to collide with lexiconfn/tests.
  {:parla
   (rdutest
    "A lexical entry for the word: 'parlare'."
    (lookup "parlare")
    (fn [parlare]
      (= (:italian parlare) "parlare"))
    :parla)})



