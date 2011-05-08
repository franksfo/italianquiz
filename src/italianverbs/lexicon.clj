(ns italianverbs.lexicon
  (:use [italianverbs.lexiconfn]))
;; useful abbreviations
(def noun
  {:cat :noun
   :det {:cat :det}
   :person :3rd})

(def mass-noun
  {:det {:cat :det
         :def :def}})

(def pronoun
  {:cat :noun
   :animate true
   :pronoun true ;; TODO: fix overlap with [:det nil]
   :human true
   :det nil}) ;; pronouns don't take a determiner.

(def nominative
  {:case :nom})
(def accusative
  {:case :acc})
(def propernoun
  (merge noun
         {:det nil})) ;; propernouns also don't take a determiner.
(def firstp
  {:person :1st})
(def secondp
  {:person :2nd})
(def thirdp
  {:person :3rd})
(def sing
  {:number :singular})
(def plural
  {:number :plural})
(def present
  {:cat :verb :infl :present})
(def choose-vp-inf
  {:obj {:cat :verb}})
(def human
  {:human true
   :animate true})
(def place
  {:place true})
(def city
  (merge place
         {:andare-a true}))
(def region
  (merge place
         {:andare-in true}))

;; WARNING: clear blows away entire lexicon in backing store (mongodb).
(clear)

;; BEGIN LEXICON

;; TODO: do a map over pairs rather than this..
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

;; prepotizioni (prepositions)
(add "in" "to"
	   {:cat :prep
	    
        :obj {:case {:$ne :nom}
              :andare-in true}})

(add "in" "in"
     {:cat :prep
      :action-occurring-in true
      
      :obj {:case {:$ne :nom}
            :english-in true
            :place true}})

(add "in" "at"
     {:cat :prep
      :action-occurring-in true
      
      :obj {:case {:$ne :nom}
            :english-at true
            :place true}})

(add "a" "to"
	   {:cat :prep
	    
        :obj {:case {:$ne :nom}
              :andare-a true}})

(add "a" "to"
	   {:cat :prep
        :obj {:case {:$ne :nom}
              :andare-al true}})

(add "di" "of"
	   {:cat :prep
        :obj {:case {:$ne :nom}}})

(add "da" "from"
	   {:cat :prep
	    
        :obj {:case {:$ne :nom}
              :place true}})

(add "a" "to"
	   {:cat :prep
	    
        :obj {:case {:$ne :nom}
              :animate true}})

(add "con" "with"
	   {:cat :prep
	    
        :obj {:case {:$ne :nom}
              :human true}})

(add "per" "for"
     {:cat :prep
      :benefactive true
      
      :obj {:case {:$ne :nom}
            :animate true}})

(add "per" "for"
     {:cat :prep
      :benefactive true
      
      :obj {:case {:$ne :nom}
            :human true}})

(add "su" "on"
	   {:cat :prep
        :obj {:case {:$ne :nom}}})

;; verbs
(add "dimenticare" "to forget"
           {:cat :verb :infl :infinitive
            :subj {:animate true}
            :obj {:cat :noun}})

;; FIXME: should also allow "at".
(def adjunct-in-a-place
  {:action-occurring-in true
   :obj.place true})

(add "agitare" "to shake"
           {:cat :verb :infl :infinitive
            :subj {:animate true}
            :obj {:cat :noun :holdable true}
            :adjunct adjunct-in-a-place})

(add "mostrare" "to show"
           {:cat :verb :infl :infinitive
            :subj {
                   :human true
                   }
            :obj {:cat :noun}
            :iobj {:obj.animate true}
            :adjunct adjunct-in-a-place})

(def dire (add "dire" "to say"
                     {:cat :verb :infl :infinitive
                      :obj {:cat :noun :sayable true}
                      :iobj {:obj.animate true
                             :benefactive true}
                      :subj {:human true}
                      :adjunct adjunct-in-a-place}))

(add-infl "dico" (list firstp sing present
		       {:root dire}))
(add-infl "dici" (list secondp sing present
		       {:root dire}))
(add-infl "dice" (list thirdp sing present
		       {:root dire}))
(add-infl "diciamo" (list firstp plural present
		       {:root dire}))
(add-infl "dite" (list secondp plural present
		       {:root dire}))
(add-infl "dicono" (list thirdp plural present
		       {:root dire}))

(add "scrivere" "to write"
           {:cat :verb :infl :infinitive
            :subj {:human true}
            :obj {:writable true
                  :cat :noun}
            :iobj {:obj.human true
                   :benefactive true}
            :adjunct adjunct-in-a-place})

(add "correggere" "to correct"
           {:cat :verb :infl :infinitive
            :subj {:human true}
            :obj {:cat :noun :human true}
            :adjunct adjunct-in-a-place})

(add "leggere" "to read"
           {:cat :verb
            :infl :infinitive
            :subj {:human true}
            :obj {:cat :noun :written true}
            :iobj {:obj.case {:$ne :nom}
                   :obj.human true}
           :adjunct adjunct-in-a-place})

(def mangiare
  (add "mangiare" "to eat"
             {:cat :verb
              :subj {:animate true}
              :obj {:cat :noun
                    :edible true}
              :adjunct {:cat :prep
                        :obj.place true}
              :infl :infinitive}))


;; FIXME: hacks until italian morphology works better: mangiare
;; is a regular -are verb.
(add "mangi" "to eat"
	    {:root mangiare
	     :cat :verb :infl :present
	     :person :2nd :number :singular})
(add "mangia" "to eat"
     {:root mangiare
      :cat :verb :infl :present
      :person :3rd :number :singular})

(add "parlere" "to speak"
     {:subj {:human true}
      :cat :verb :infl :infinitive
      :iobj {:obj.human true}
      :adjunct adjunct-in-a-place})

(add "smettere" "to quit"
     {:cat :verb :infl :infinitive-omit
      :subj {:human true}
      :obj {:cat :noun}
      :adjunct adjunct-in-a-place})
;; someday: (e.g. "quits working at the bank")
;; :obj vp[:tense present-participle]
 
(add "pranzare" "to eat lunch"
           {:cat :verb
            :infl :infinitive
            :subj {:human true}
            :adjunct adjunct-in-a-place}) ;; e.g. "[eats lunch [in [ the cafe ]]]"
           
;; <andare root>
(def andare
  (add "andare" "to go"
             {:cat :verb :infl :supertype
              :subj {:animate true}}))

;; <andare adjunct variants> 
(add "andare" "to go"
           (merge andare
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "a"
                             :obj.andare-a true}}))
(add "andare" "to go"
           (merge andare
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "in"
                             :obj.andare-in true}}))
(add "andare" "to go"
           (merge andare
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "a"
                             :obj.andare-al true}}))
(add "andare" "to go"
           (merge andare
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "da"
                             :obj.human true}}))
;; </andare root variants>

;; <andare exceptions>
(add-infl "vado" (list firstp sing present)
	  {:root andare})
(add-infl "vai" (list secondp sing present)
	  {:root andare})
(add-infl "va" (list thirdp sing present)
	  {:root andare})
(add-infl "andiamo" (list firstp plural present)
	  {:root andare})
(add-infl "andate" (list secondp plural present)
	  {:root andare})
(add-infl "vanno" (list thirdp plural present)
	  {:root andare})
;; </andare exceptions>
;; </andare>

;; <venire>

(def venire
  (add "venire" "to come"
             {:cat :verb :infl :supertype
              :subj {:animate true}}))

;; <venire adjunct variants>
;; TODO : add and use (add-variant)
;; (which doesn't need the english repeated,
;; and uses just the defined symbol venire,
;; not the string "venire".
;; come *to* a place or by means of something ("vengo in treno")
(add "venire" "to come"
           (merge venire
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "in"
                             :obj.andare-in true}}))
;; come *to* a place.
(add "venire" "to come"
           (merge venire
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "a"
                             :obj.andare-a true}}))
;; come *from* a place.
(add "venire" "to come"
           (merge venire
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "da"
                             :obj.place true}}))
;; come *to* a person.
(add "venire" "to come"
           (merge venire
                  {:infl :infinitive
                   :adjunct {:cat :prep
                             :italian "da"
                             :obj.human true}}))

;; </venire adjunct variants>

 ;; <venire exceptions>
 (add-infl "vengo" (list firstp sing present
		       {:root venire}))
(add-infl "vieni" (list secondp present
		       {:root venire}))
(add-infl "viene" (list thirdp sing present
		       {:root venire}))
(add-infl "veniamo" (list firstp plural present
		       {:root venire}))
(add-infl "venite" (list secondp plural present
		       {:root venire}))
(add-infl "vengono" (list thirdp plural present
		       {:root venire}))
;; </venire exceptions>

;; </venire>

(def volare (add "volare" "to want"
                 {:cat :verb :infl :infinitive
                  :subj {:animate true}}
                 (list choose-vp-inf)))
                       
(add-infl "voglio" (list firstp sing present
			 {:root volare}))
(add-infl "vogli" (list secondp sing present
			{:root volare}))
(add-infl "voglie" (list thirdp sing present
			 {:root volare}))
(add-infl "vogliamo" (list firstp plural sing present
			 {:root volare}))
(add-infl "vogliete" (list secondp plural plural present
			 {:root volare}))
(add-infl "vogliono" (list thirdp plural plural present
			 {:root volare}))


(def potere (add "potere" "to be able"
                 {:cat :verb :infl :infinitive
                  :subj {:animate true}}
                 (list choose-vp-inf)))

(add-infl "posso" (list firstp sing present
			 {:root potere}))
(add-infl "possi" (list secondp sing present
			{:root potere}))
(add-infl "può" (list thirdp sing present
			 {:root potere}))
(add-infl "possiamo" (list firstp plural sing present
			 {:root potere}))
(add-infl "potete" (list secondp plural plural present
			 {:root potere}))
(add-infl "possono" (list thirdp plural plural present
			 {:root potere}))

(def dovere (add "dovere" "must"
                 {:cat :verb :infl :infinitive
                  :subj {:animate true}}
                 (list choose-vp-inf)))
(add-infl "devo" (list firstp sing present
                       {:root dovere}))
(add-infl "devi" (list firstp sing present
                       {:root dovere}))
(add-infl "deve" (list firstp sing present
                       {:root dovere}))
(add-infl "dobbiamo" (list firstp sing present
                       {:root dovere}))
(add-infl "dovete" (list firstp sing present
                       {:root dovere}))
(add-infl "devono" (list firstp sing present
                       {:root dovere}))


(def fare (add "fare" "to make"
                     {:cat :verb :infl :infinitive
                      :obj {:cat :noun
                            :artifact true}
                      :subj {:human true}
                      :iobj {:obj.animate true
                             :benefactive true}
                      :adjunct adjunct-in-a-place}))
                     
(add-infl "facio" (list firstp sing present
			{:root fare}))
(add-infl "fai" (list secondp sing present
		      {:root fare}))
(add-infl "fà" (list thirdp sing present
		      {:root fare}))
(add-infl "facciamo" (list firstp plural sing present
			   {:root fare}))
(add-infl "fate" (list secondp plural plural present
			 {:root fare}))
(add-infl "fanno" (list thirdp plural plural present
			 {:root fare}))

(def essere (add "essere" "to be"
                     {:cat :verb :infl :infinitive
                      :obj {:cat :noun}}))

(add-infl "sono" (list firstp sing present
                       {:root essere
                        :english "am"}))
(add-infl "sei" (list secondp sing present
                      {:root essere
                       :english "are"}))
(add-infl "è" (list thirdp sing present
                    {:root essere
                     :english "is"}))
(add-infl "siamo" (list firstp plural sing present
                        {:root essere
                         :english "are"}))
(add-infl "siete" (list secondp plural plural present
                        {:root essere
                         :english "are"}))
(add-infl "sono" (list thirdp plural plural present
                       {:root essere
                        :english "are"}))

;; pronouns
(add "io" "i" {
               :person :1st :number :singular :cat :noun
               } (list pronoun nominative))
(add "mi" "me" {:person :1st :number :singular :cat :noun :case :acc}
     (list pronoun))
(add "tu" "you" {:person :2nd :number :singular :cat :noun :case :nom}
           (list pronoun))
(add "te" "you" {:person :2nd :number :singular :cat :noun :case :acc}
           (list pronoun))
(add "lui" "he" {:person :3rd :number :singular :cat :noun :gender :masc} (list pronoun nominative))
(add "lei" "she" {:person :3rd :number :singular :cat :noun :gender :fem} (list pronoun nominative))
(add "noi" "we" {:person :1st :number :plural :cat :noun} (list pronoun nominative))
(add "voi" "you all" {:person :2nd :number :plural :cat :noun} (list pronoun))
(add "loro" "they" {:person :3rd :number :plural :cat :noun} (list pronoun nominative))

(add "io" "me" {
                :person :1st :number :singular :cat :noun} (list pronoun accusative))
(add "lui" "him" {:person :3rd :number :singular :cat :noun :gender :masc} (list pronoun accusative))
(add "lei" "her" {:person :3rd :number :singular :cat :noun :gender :fem} (list pronoun accusative))
(add "noi" "us" {:person :1st :number :plural :cat :noun} (list pronoun accusative))
(add "loro" "them" {:person :3rd :number :plural :cat :noun} (list pronoun accusative))

;; Proper nouns
(add "Italia" "Italy" 
           {}
           (list sing propernoun region))

(add "Spagna" "Spain" 
           {}
           (list sing propernoun region))

(add "Sicily" "Sicily" 
           {}
           (list sing propernoun region))

(add "Firenze" "Florence" 
           {}
           (list sing propernoun city))

(add "Napoli" "Naples" 
           {}
           (list sing propernoun city))

;; determiners
(add "il" "the" {:gender :masc :number :singular :cat :det
                 :def :def})
(add "un" "a" {:gender :masc :number :singular :cat :det
               :def :indef})
(add "i" "the" {:gender :masc :number :plural :cat :det
                :def :def})

(add "la" "the" {:gender :fem :number :singular :cat :det
                 :def :def})
(add "una" "a" {:gender :fem :number :singular :cat :det
                :def :indef})
(add "le" "the" {:gender :fem :number :plural :cat :det
                 :def :def})

(add "del" "some" {:gender :masc :number :plural :cat :det
                 :def :part})


;; nouns
(add-with-plural "uomo" "man"
  {:cat :noun
   :number :singular
   :gender :masc}
  (list noun human)
  "uomini" "men")

(add-with-plural "donna" "woman"
  {:cat :noun
   :number :singular
   :gender :fem}
  (list noun human)
  nil "women")

(add-with-plural "studente" "student"
  {:cat :noun
   :number :singular
   :gender :masc}
  (list noun human))

(add-with-plural "ragazzo" "guy"
  {:cat :noun
   :number :singular
   :gender :masc}
  (list noun human))
	     
(add-with-plural "ragazza" "girl"
  {:cat :noun
   :number :singular
   :gender :fem}
  (list noun human))

(add-with-plural "cane" "dog"
  {:cat :noun
   :number :singular
   :gender :masc
   :animate true}
  (list noun))

(add "pacco" "package"
     {:cat :noun
      :number :singular
      :gender :masc
      :artifact true}
     (list noun))

(add "pane" "bread"
     {:cat :noun
      :number :singular
      :gender :masc
      :artifact true
      :edible true
      :holdable true}
     (list noun))

(add "pasta" "pasta"
	    {:cat :noun
	     :number :singular
	     :gender :fem
         :makeable true
         :edible true
         :det {:def :def}
         :holdable true}
        (list mass-noun noun))

(add-with-plural "libro" "book"
     {:cat :noun
      :number :singular
      :gender :masc
      :artifact true
      :written true
      :holdable true
      :roomable true
      :tableable true
      :ruggable true
      :person :3rd}
     (list noun))

(add-with-plural "tavolo" "table"
     {:cat :noun
      :number :singular
      :gender :masc
      :artifact true
      :written true
      :holdable true ;; barely holdable (if you're strong or there's more than one of you) :)
      :furniture true
      :ruggable true
      :person :3rd}
     (list noun))


(add-with-plural "gamba" "leg"
  {:cat :noun
   :number :singular
   :gender :fem
   :person :3rd
   :body-part true}
  (list noun))

(add "giornale" "newspaper"
	    {:cat :noun
	     :number :singular
	     :gender :masc
         :person :3rd
         :artifact true
         :holdable true
         :written true}
        (list noun))

(add-with-plural "abito" "dress"
  {:gender :masc
   :artifact true
   :holdable :true
   }
  (list noun))

(add-with-plural "parola" "word"
  {:sayable true
   :writable true
   :gender :fem}
  (list noun)
  "parole" "words")

(add "centro" "downtown"
           {:andare-in true
            :cat :noun
            :det nil})

(add "ufficio" "the office" ;; TODO: better english translation might be "my office","your office", etc, or in some cases "work".
           {:andare-in true
            :cat :noun
            :place true
            :det nil})

;; variant that takes a determiner: ('il ufficio')
;; commented out: lexical variants do not work yet!
;(add "ufficio" "the office"
;     {:andare-in true
;      :cat :noun
;      :place true
;      :det true})

(add "casa" "home"
           {:andare-a true
            :cat :noun
            :english-at true
            :place true
            :det nil})

(add "letto" "bed"
     {:andare-a true
      :english-in true
      :place true
      :cat :noun
      :det nil})

(add-with-plural "letto" "bed"
     {:furniture true
      :gender :masc
      :number :singular}
     (list noun))

(add-with-plural "tavolino" "coffee table"
     {:furniture true
      :gender :masc
      :number :singular}
     (list noun))

(add-with-plural "finestra" "window"
     {:furniture true
      :gender :fem
      :number :singular}
     (list noun))

(add-with-plural "parete" "wall"
     {:furniture true
      :gender :fem
      :number :singular}
     (list noun)
     "pareti")

(add-with-plural "poltrona" "easy chair"
     {:furniture true
      :gender :fem
      :number :singular}
     (list noun))

(add-with-plural "sedia" "chair"
     {:furniture true
      :gender :fem
      :number :singular}
     (list noun))

(add "cinema" "the theatre"
     {:andare-al true
      :place true
      :english-in true
      :cat :noun
      :det nil})

(add "mare" "the beach"
     {:andare-al true
      :place true
      :english-at true
      :cat :noun
      :det nil})

(add "ristorante" "the restaurant"
     {:andare-al true
      :cat :noun
      :english-at true
      :english-in true
      :place true
      :det nil})

(add "salute" "health"
     {:cat :noun
      :det nil})

;; adjectives
(add "bianco" "white"
	    {:cat :adjective})
(add "nero" "black"
	    {:cat :adjective})
(add "forte" "strong"
	    {:cat :adjective})

(add "sinistra" "left"
	    {:cat :adjective})
(add "destra" "right"
	    {:cat :adjective})

;; sentences

(add "ha gli occhi azzuri" "he has blue eyes"
	    {:person "3rd" :number :singular :cat :verb})
(add "ha i cappelli non molte lunghi" "he has not very long hair"
	    {:person "3rd" :number :singular :cat :verb})
(add "ha il naso alla francese" "he has a french nose" 
	    {:person "3rd" :number :singular :cat :verb})
(add "non lo so" "i don't know"
	    {:cat :verb})
(add "come sono?" "how?"
	    {:cat :verb})
(add "cosa fa?" "what?"
	    {:cat :verb})

;; adjectives
(add "alto" "tall"
	    {:cat :adjective})
(add "basso" "short"
	    {:cat :adjective})
(add "giovano" "young"
	    {:cat :adjective})
(add "anziano" "old"
	    {:cat :adjective})
(add "margra" "lean"
	    {:cat :adjective})
(add "grasso" "fat"
	    {:cat :adjective})
(add "bello" "beautiful"
	    {:cat :adjective})
(add "bruto" "ugly"
	    {:cat :adjective})
(add "carino" "cute"
	    {:cat :adjective})
(add "lunghi" "long"
	    {:cat :adjective})
(add "corti" "short"
	    {:cat :adjective})
(add "calvo" "bald"
	    {:cat :adjective})
(add "bruno" "brown"
	    {:cat :adjective})
(add "bianchi" "white"
	    {:cat :adjective})
(add "di mezza eta" "middle-aged"
	    {:cat :adjective})
(add "qui" "here"
	   {:cat :adjective})

;; note whimsical :furniture true constraint on :obj.
(add "proprio accanto a" "right next to"
     {:cat :prep
      :landscape-prep true}) ;; e.g. "the post office is right next to the bank".

;; furniture prepositions.
;; note that the query language currently might not support nested constraints like [:obj [:furniture true]]

(add "a destra de" "to the right of"
     {:cat :prep
      :furniture-prep true
      :obj {:case {:$ne :nom}
            :furniture true}})

(add "a sinistra de" "to the left of"
     {:cat :prep
      :furniture-prep true
      :obj {:case {:$ne :nom}
            :furniture true}})

(add "accanto a" "next to"
     {:cat :prep
      :furniture-prep true
      :obj {:case {:$ne :nom}
            :furniture true}})

(add "dentro" "inside"
     {:cat :prep
      :furniture-prep true
      :subj {:holdable true}
      :obj {:case {:$ne :nom}
            :furniture true}})

(add "dietro" "behind"
     {:cat :prep
      :furniture-prep true
      :obj {:case {:$ne :nom}
            :furniture true}})

(add "davanti a" "in front of"
     {:cat :prep
      :furniture-prep true
      :obj {:furniture true}})

;; "le sedie sono intorno al tavolino
;;  (the chairs are around the the table)"
;; TODO: doesn't work yet: :obj should be post-condition on NP.
(add "intorno a" "around"
     {:cat :prep
      :furniture-prep false
      :subj {:number :plural
             :furniture true}
      :obj {:number :sing
            :furniture true}})

(add "sopra" "above"
     {:cat :prep
      :furniture-prep true
      :obj {:case {:$ne :nom}
            :furniture true}})

(add "sotto" "under"
     {:cat :prep
      :furniture-prep true
      :obj {:case {:$ne :nom}
            :furniture true}})

(add "su" "on"
     {:cat :prep
      :furniture-prep true
      :obj {:case {:$ne :nom}
            :furniture true}})

;; TODO: doesn't work yet: :obj should be post-condition on NP.
;(add "tra" "between"
;     {:cat :prep
;      :furniture-prep true
;      :obj {:plural true
;            :case {:$ne :nom}
;            :furniture true}})

(add "vicino a" "close to"
     {:cat :prep
      :furniture-prep true
      :obj {:furniture true}})
