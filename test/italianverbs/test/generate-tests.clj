;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(def generate-tests
  (list
   
   



   (rdutest
    "Make sure every transitive verb has at least one noun that satisfies its :obj spec."
    (check-objs)
    (fn [pairs]
      (not (some (fn [pair] (= (:objs pair) 0)) pairs)))
    :every-trans-verb-has-a-possible-object)

   (rdutest
    "essere vp"
    (let [root-verb (lookup "essere")]
      (conjugate-vp (fs/merge root-verb {:infl :present})
                    (lookup "io")
                    (conjugate-np (lookup "libro") {:def :indef})))
    (fn [vp]
      (= (:italian vp) "sono un libro"))
    :sono-un-tavolo)

   (rdutest
    "essere sentence"
    (let [root-verb (lookup "essere")
          vp (conjugate-vp (fs/merge root-verb {:infl :present})
                           (lookup "voi")
                           (conjugate-np (lookup "libro") {:def :indef}))]
      (conjugate-sent vp (lookup "voi")))
    (fn [sentence]
      (= (:italian sentence) "voi siete un libro"))
    :io-sono-un-tavolo)

   (rdutest
    "ruggable noun: find a noun for a thing that can sit on a rug (e.g. a table)"
    (search/random-lexeme {:cat :noun :ruggable true})
    (fn [noun]
      (not (nil? noun))))

   (rdutest
    "furniture sentence"
    (mobili)
    (fn [sentence]
      (= (:furniture (:head (:object (:verb-phrase sentence)))) true))
    :mobili)

   (rdutest
    "random-present-related test part 1: get a random infinitive verb."
    (search/random-lexeme {:cat :verb :infl :infinitive})
    (fn [verb]
      (and (or (= (:cat verb) :verb)
               (= (:cat verb) "verb"))
           (or (= (:infl verb) :infinitive)
               (= (:infl verb) "infinitive"))))
    :get-root-verb)

   (rdutest
    "random-present-related test part 1: get a random infinitive verb."
    (let [infinitive (search/random-lexeme {:cat :verb :infl :infinitive})]
      (search/random-lexeme (:subj infinitive)))
    (fn [subject]
      (and (not (= (get subject :case) "acc"))
           (not (= (get subject :case) :acc))))
    :get-subject)

                                        ;     (rdutest
                                        ;      "random present svo sentence"
                                        ;      (random-present)
                                        ;      (fn [sentence]
                                        ;        (and (not (= (:case (:noun (:object (:verb-phrase sentence)))) :nom))
                                        ;             (not (= (:case (:noun (:subject (:verb-phrase sentence)))) :acc))))
                                        ;      :random-svo)

   (rdutest
    "sports-vp: select a verb whose object is {:sport true}"
    (random-verb-for-svo {:obj {:sport true}})
    (fn [verb]
      (= (:sport (:obj verb)) true))
    :sports-vp)

   ;; random-lexeme seems to be hitting a limit of some kind..
   (rdutest
    "random-lexeme with a lot of stuff."
    (search/random-lexeme {:root {:gender :masc
                           :comp {:cat :det}
                           :animate true
                           :common true
                           :number :singular
                           :italian "uomo"
                           :person :3rd
                           :english "man"}})
    (fn [retval]
      (not (= retval nil)))
    :random-lexeme1)

   (rdutest
    "random-lexeme with even more stuff."
    (search/random-lexeme {:root {:gender :masc
                           :comp {:cat :det}
                           :animate true
                           :common true
                           :number :singular
                           :italian "uomo"
                           :person :3rd
                           :english "man"
                           :cat :noun}})
    (fn [retval]
      (not (= retval nil)))
    :random-lexeme2)
     
     ;; TODO more random-morph tests.
;     (rdutest
;      "random-morph: test for pluralization exceptions: uomo->uomini"
;      (random-morph (fs/unify (search/random-lexeme {:common true :italian "uomo"}) {:use-number :plural}))
;      (fn [noun]
;        (and (= (:english noun) "men")
;             (= (:italian noun) "uomini")))
;      :random-morph)

     ;; like the above, but only english is exceptional (women) : italian is regular (donne).
;     (rdutest
;      "random-morph: test for pluralization exceptions: donna->donne"
;      (random-morph (fs/unify (search/random-lexeme {:common true :english "woman"}) {:use-number :plural}))
;      (fn [noun]
;        (and (= (:english noun) "women")
;             (= (:italian noun) "donne")))
;      :random-morph)

;     (rdutest
;      "random-determiner: generate a random determiner (il,i,lo,le,..)"
;      (random-morph (search/random-lexeme {:cat :det}))
;      (fn [det]
;        (or 
;         (= (:italian det) "dei")
;         (= (:italian det) "i")
;         (= (:italian det) "il")
;         (= (:italian det) "le")
;         (= (:italian det) "un")
;         (= (:italian det) "una")
;         (= (:italian det) "la")))
;      :random-det)

     ;; generate a complement whose generation is constrained by a head.
;     (rdutest
;      "Complement (determiner) must agree with its head (noun)."
;      (let [head (search/random-lexeme {:italian "cane" :cat :noun})
;            comp (fs/unify (search/random-lexeme (:comp head)))]
;        comp)
;      (fn [comp]
;        (or
;         (= (:italian comp) "il")
;         (= (:italian comp) "un")))
;      :det-agreement)
     
     ;; We constrain the generation sufficiently that only one italian expression matches it ('un cane')."
;     (rdutest
;      "random noun phrase composed of a determiner and a noun: 'un cane'."
;     (let [head (random-morph (search/random-lexeme {:english "dog"}) {:use-number :singular})]
;       (random-phrase head
;                      {:def :def}))
;     (fn [np]
;       (= (:italian np) "il cane"))
;     :random-np-specify-complement-singular)

;     (rdutest
;      "random phrase using default 2nd argument of (random-phrase). (the default 2nd argument used will be (:comp head)."
;      (let [head (random-morph (search/random-lexeme {:english "dog"}) {:use-number :singular})]
;        (random-phrase head))
;      (fn [np]
;        (or 
;         (= (:italian np) "un cane")
;         (= (:italian np) "il cane")))
;      :random-np-default-complement-singular)

;     (rdutest
;      "random phrase using default 2nd argument of (random-phrase). (the default 2nd argument used will be (:comp head)."
;      (random-phrase (random-morph (search/random-lexeme {:english "dog"})))
;      (fn [np]
;        (or 
;         (= (:italian np) "un cane")
;         (= (:italian np) "il cane")
;         (= (:italian np) "dei cani")
;         (= (:italian np) "i cani")))
;      :random-np-default-complement)

     
;     (rdutest
;      "the word 'soccer' does not take an article in both english and italian (i.e. 'calcio', not 'il calcio')"
;      (random-present {:obj {:sport true}}) ;; 'giocare' is the only +sport verb, and 'calcio' is the only +sport noun.
;      (fn [sentence]
;        (and (= (:italian (:object (:verb-phrase sentence))) "calcio")
;             (= (:english (:object (:verb-phrase sentence))) "soccer")))
;      :soccer)

;     (rdutest
;      "la parola 'pasta' prende solo un articolo definitivo, non un'indefinitivo."
;      (conjugate-np (search/random-lexeme {:italian "pasta"}))
;      (fn [np]
;        (and (not (= (:english np) "a pasta"))
;             (not (= (:italian np) "una pasta"))))
;      :mass-nouns)

;     (rdutest
;      "intransitive verbs (e.g. 'lavorare (to work)' have no object."
;      (random-present {:italian "lavorare"})
;      (fn [sent]
;        (= (:obj sent) nil))
;      :intransitive)

;     (rdutest
;      "past tense conjugation"
;      (random-past {:root {:italian "lavorare"}
;                    :subj {:root "io"}
;                    :infl :passato-prossimo})
;      (fn [sent]
;        (= (:italian sent) "io ho lavorato"))
;      :lavorato)
     
     
     ))

;(defn test []
;  "this should contain a list of all the tests for the generate package. each test can
;  return a map or a list or a function. a function will be applied against an
;  empty argument list"
;  (list
;   {:comment "random 'passato prossimo' sentence."
;    :test (random-passato-prossimo)};;

;   {:comment "random 'futuro semplice' sentence."
;    :test (random-futuro-semplice)}

;   {:comment "sceglieremo (we will choose)"
;    :test (random-futuro-semplice {:italian "sceglieremo"})}

;   {:comment "testing english translation of future modal verbs e.g. 'dovere' (must)"
;    :test (random-futuro-semplice
;           {:italian "dovrai"})}

;   {:comment "testing english translation of future modal verbs e.g. 'potere' (can)"
;    :test (random-futuro-semplice
;           {:italian "potrai"})}))

