(ns italianverbs.lexicon
  (:use [hiccup core page-helpers]
	[somnium.congomongo]
	[italianverbs.grammar])
  (:require [clojure.string :as string]
	    [clojure.contrib.str-utils2 :as str-utils]))

(mongo! :db "mydb")

(defn get-from-lexicon [italian]
  (fetch-one :lexicon :where {:italian italian}))

(defn italian [lexeme]
  (get (nth lexeme 1) :lexicon))

(defn synsem [lexeme]
  (nth lexeme 1))

(defn english [lexeme]
  (get (nth lexeme 1) :english))

(defn verb-row [italian]
  (html  
   [:tr 
   [:th italian] [:td (get (get-from-lexicon italian) :english)] 
   ]))

(defn clear-lexicon []
  (destroy! :lexicon {}))

(defn add-lexeme [italian english & [featuremap]]
  (let [featuremap
	(merge featuremap
	       (if english
		 (assoc {} :italian italian :english english)
		 (assoc {} :italian italian)))]
    (let [function-to-symbol featuremap]
      (insert! :lexicon function-to-symbol)
      featuremap)))

(defn add-lex2 [italian & [types result]]
  (if (first types)
    (add-lex2
     italian
     (rest types)
     (merge (first types) result))
    (add-lexeme italian nil result)))

(defn remove-to [english-verb-phrase]
  (let [regex #"to (.*)"]
    (str-utils/replace english-verb-phrase regex (fn [[_ rest]] (str rest)))))

(defn add-s-to-first-word [english-verb-phrase]
  ;; FIXME: look at (get english-verb-phrase :head)
  (let [regex #"^([^ ]*)([o])([ ]?)(.*)"
	with-e
	(str-utils/replace
	 english-verb-phrase
	 regex
	 (fn [[_ word vowel space rest]] (str word (if vowel (str vowel "e")) space rest)))]
    (let [regex #"^([^ ]*)([ ]?)(.*)"]
      (str-utils/replace
       with-e
       regex
       (fn [[_ word space rest]] (str word "s" space rest))))))

(defn conjugate-english-verb [verb-head subject]
  ;; conjugate verb based on subject and eventually verb's features (such as tense)
  (let [english (get verb-head :english)]
    (cond
     (and (not (= (get subject :cat) "noun"))
	  (not (= (get subject :cat) "pronoun")))
     {:cat :error
      :note  (str ":cat != :noun for " subject)}
     (= (get subject :person) "1st")
     (remove-to english)
     (= (get subject :person) "2nd")
     (remove-to english)
     (and
      (= (get subject :person) "3rd")
      (= (get subject :number) "singular")) 
     (add-s-to-first-word (remove-to english))
     true
     (remove-to english))))

(defn conjugate-italian-verb-regular [verb-head subject]
   (let [root-form (get verb-head :italian)
	 regex #"^([^ ]*)([aei])re([ ]?)(.*)"]
     (cond

      (and (= (get subject :person) "1st")
	   (= (get subject :number) "singular"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem "o" space rest)))

      (and (= (get subject :person) "1st")
	   (= (get subject :number) "plural"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem "i" "amo" space rest)))


      (and (= (get subject :person) "2nd")
	   (= (get subject :number) "singular"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem "i" space rest)))

      (and (= (get subject :person) "2nd")
	   (= (get subject :number) "plural"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem vowel "te" space rest)))

      
      (and (= (get subject :person) "3rd")
	   (= (get subject :number) "singular"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem "e" space rest)))

      (and (= (get subject :person) "3rd")
	   (= (get subject :number) "plural"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem vowel "no" space rest)))
      true
      (str "(conjugate-italian-verb-regular=>(can't conjugate this..)"
	   (tablize verb-head)
	   (tablize subject)))))
 
(defn plural-masc [italian]
 (let [regex #"^([^ ]*)o([ ]?)(.*)"]
   (str-utils/replace
    italian
    regex (fn [[_ stem space rest]] (str stem "i" space rest)))))

(defn plural-fem [italian]
 (let [regex #"^([^ ]*)a([ ]?)(.*)"]
   (str-utils/replace
    italian
    regex (fn [[_ stem space rest]] (str stem "e" space rest)))))

(defn conjugate-it [head]
  (cond (= (get head :cat) "noun")
	(cond (= (get head :gender) "masc")
	      (cond (= (get head :number) "plural")
		    (plural-masc (get head :italian))
		    true
		    (get head :italian))
	      (= (get head :gender) "fem")
	      (cond (= (get head :number) "plural")
		    (plural-fem (get head :italian))
		    true
		    (get head :italian))
	      true
	      "??(not masc or fem)")
	true
	(str "??(cat != noun)"
	     (get head :cat)
	     (= (get head :cat) "noun"))))

(defn conjugate-en [head arg]
  (str (get arg :english)
       " "
       (cond (= (get head :cat) "noun")
	     (cond (= (get head :number) "plural")
		   (str (get head :english) "s")
		   true
		   (get head :english))
	     true
	     (str "??(cat != noun)"
		  (get head :cat)
		  (= (get head :cat) "noun")))))

(defn get-head [sign]
  (if (get sign :head)
    (get sign :head)
    sign))

(defn conjugate-italian-verb [verb-phrase subject]
  ;; conjugate verb based on subject and eventually verb's features (such as tense)
  ;; takes two feature structures and returns a string.
  (let [italian (get verb-phrase :italian)
	italian-head (get (get verb-phrase :head) :italian)]
    (let [italian (if italian-head italian-head italian)]
      (let [irregular
	    (fetch-one :lexicon
		       :where {:cat :verb
			       :infl :present
			       :person (get subject :person)
			       :number (get subject :number)
			       :root.italian (get (get-head verb-phrase) :italian)
			       }
		       )]
	(if irregular
	  (str (get irregular :italian)
	       " "
	       (get (get verb-phrase :comp) :italian))
	  (str
	   (conjugate-italian-verb-regular
	    (get-head verb-phrase) subject) " "
	   (get (get verb-phrase :comp) :italian)))))))

(defn unify-np [head arg]
  (if (and
       (= (get head :gender)
	  (get arg :gender))
       (= (get head :number)
	  (get arg :number)))
    (assoc {}
      :head head)
    (assoc {}
      :cat :fail
      ;; FIXME: rewrite as (defn diagnosis [head arg])
      :note (str (get head :gender) " != " (get arg :gender)
		 " or "
		 (get head :number) " != " (get arg :number)))))

(defn noun-fn [head arg]  ;; e.g. "il libro"
  (merge
   (unify-np head arg)
   (assoc {}
    :english
    (conjugate-en head arg)
    :italian
    (string/join " "
		 (list (get arg :italian)
		       (conjugate-it head))))))

(defn verb-sv [head comp]  ;; e.g. "i [sleep]","he [writes a book]"
  (cond
   (or (= (get (get-head comp) :cat) "noun")
       (= (get (get-head comp) :cat) "pronoun"))
   {:fn "verb-sv"
    :english
    (string/join " "
		 (list 
		  (get comp :english)
		  (conjugate-english-verb (get-head head) comp)
		  (get (get head :comp) :english)))
    :italian
    (string/join " "
		 (list
		  (get comp :italian)
		  (conjugate-italian-verb head comp)))}
   (= (get (get-head comp) :cat) "prep")
   {:fn "verb-sv"
    :head head
    :comp comp
    :italian
    (str
     (get head :italian)
     " "
     (get comp :italian))
     :english
    (str
     (get head :english)
     " "
     (get comp :english))}
   true
   {:cat :error
    :note "verb does not know what to do with this argument."}))

(defn verb-vo [head arg]  ;; e.g. "[sees a house]","[writes a book]"
  (assoc {}
    :infl :infinitive
    :fn verb-sv
    :head head
    :comp arg
    :english
    (string/join " "
		 (list 
		  (get head :english)
		  (get arg :english)))
    :italian
    (string/join " "
		 (list 
		  (get head :italian)
		  (get arg :italian)))))

(defn trans2 []) ;; e.g. "give"

;; useful abbreviations
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

;; BEGIN LEXICON
(clear-lexicon)

(defn prep-fn [head arg]  ;; e.g. "[in Italia]","[a lavorare]"
  {:head head
   :comp arg
   :english
   (string/join " "
		(list 
		 (get head :english)
		 (get arg :english)))
   
   :italian
   (string/join " "
		(list 
		 (get head :italian)
		 (get arg :italian)))})

;; prepositions
(add-lexeme "in" "in"
	    {:cat :prep
	     :fn "prep-fn"})

;; verbs

(add-lexeme "dimenticare" "to forget"
	    {:cat :verb :infl :infinitive :fn "verb-vo"})

(def dire (add-lexeme "dire" "to say"
		      {:cat :verb :infl :infinitive :fn "verb-sv"}))
(add-lex2 "dico" (list firstp sing present
		       {:root dire}))
(add-lex2 "dici" (list secondp sing present
		       {:root dire}))
(add-lex2 "dice" (list thirdp sing present
		       {:root dire}))
(add-lex2 "diciamo" (list firstp plural present
		       {:root dire}))
(add-lex2 "dite" (list secondp plural present
		       {:root dire}))
(add-lex2 "dicono" (list thirdp plural present
		       {:root dire}))

(def venire (add-lexeme "venire" "to come"
			{:cat :verb :infl :infinitive :fn "verb-sv"}))
(add-lex2 "vengo" (list firstp sing present
		       {:root venire}))
(add-lex2 "vieni" (list secondp present
		       {:root venire}))
(add-lex2 "viene" (list thirdp sing present
		       {:root venire}))
(add-lex2 "veniamo" (list firstp plural present
		       {:root venire}))
(add-lex2 "venite" (list secondp plural present
		       {:root venire}))
(add-lex2 "vengono" (list thirdp plural present
		       {:root venire}))

(add-lexeme "scrivere" "to write"
	    {:cat :verb :infl :infinitive :fn "verb-vo"})
(add-lexeme "correggere" "to correct"
	    {:cat :verb :infl :infinitive :fn "verb-vo"})
(add-lexeme "leggere" "to read"
	    {:cat :verb :infl :infinitive :fn "verb-vo"})

(add-lexeme "mangiare" "to eat"
	    {:cat :verb :infl :infinitive :fn "verb-vo"})

;; FIXME: hacks until italian morphology works better: mangiare
;; is a regular -are verb.
(add-lexeme "mangi" "to eat"
	    {:italian-root "mangiare"
	     :cat :verb :infl :present
	     :person :2nd :number :singular})
(add-lexeme "mangia" "to eat"
	    {:italian-root "mangiare"
	     :cat :verb :infl :present
	     :person :3rd :number :singular})

(add-lexeme "parlere" "to speak"
	    {:cat :verb :infl :infinitive :fn "verb-vo"})
(add-lexeme "smettere" "to quit"
	    {:cat :verb :infl :infinitive :fn "verb-vo"})
(add-lexeme "pranzare" "to eat lunch"
	    {:cat :verb :infl :infinitive :fn "verb-sv"})

(def andare
  (add-lexeme "andare" "to go"
	      {:cat :verb :infl :infinitive :fn "verb-sv"}))
;; exceptions
(add-lex2 "vado" (list firstp sing present)
	  {:root andare})
(add-lex2 "vai" (list secondp sing present)
	  {:root andare})
(add-lex2 "va" (list thirdp sing present)
	  {:root andare})
(add-lex2 "andiamo" (list firstp plural present)
	  {:root andare})
(add-lex2 "andate" (list secondp plural present)
	  {:root andare})
(add-lex2 "vanno" (list thirdp plural present)
	  {:root andare})

(def volare (add-lexeme "volare" "to want"
			{:cat :verb :infl :infinitive :fn "verb-sv"}))
(add-lex2 "voglio" (list firstp sing present
			 {:root volare}))
(add-lex2 "vogli" (list secondp sing present
			{:root volare}))
(add-lex2 "voglie" (list thirdp sing present
			 {:root volare}))
(add-lex2 "vogliamo" (list firstp plural sing present
			 {:root volare}))
(add-lex2 "vogliete" (list secondp plural plural present
			 {:root volare}))
(add-lex2 "vogliono" (list thirdp plural plural present
			 {:root volare}))

(def fare (add-lexeme "fare" "to make"
			    {:cat :verb :infl :infinitive :fn "verb-sv"}))

(add-lex2 "facio" (list firstp sing present
			{:root fare}))
(add-lex2 "fai" (list secondp sing present
		      {:root fare}))
(add-lex2 "fà" (list thirdp sing present
		      {:root fare}))
(add-lex2 "facciamo" (list firstp plural sing present
			   {:root fare}))
(add-lex2 "fate" (list secondp plural plural present
			 {:root fare}))
(add-lex2 "fanno" (list thirdp plural plural present
			 {:root fare}))

;; pronouns

(add-lexeme "io" "i" {:person :1st :number :singular :cat :pronoun})
(add-lexeme "tu" "you" {:person :2nd :number :singular :cat :pronoun})
(add-lexeme "lui" "he" {:person :3rd :number :singular :cat :pronoun})
(add-lexeme "noi" "we" {:person :1st :number :plural :cat :pronoun})
(add-lexeme "voi" "you all" {:person :2nd :number :plural :cat :pronoun})
(add-lexeme "loro" "they" {:person :3rd :number :plural :cat :pronoun})

(add-lexeme "Italia" "Italy" {:cat :noun})

;; determiners
(add-lexeme "il" "the" {:gender :masc :number :singular :cat :det
			:def :def})
(add-lexeme "i" "the" {:gender :masc :number :plural :cat :det
		       :def :def})
(add-lexeme "gli" "the" {:gender :masc :number :plural :cat :det
			 :def :def})

(add-lexeme "la" "the" {:gender :fem :number :singular :cat :det
			:def :def})
(add-lexeme "le" "the" {:gender :fem :number :plural :cat :det
			:def :def})

;; nouns
(add-lexeme "uomo" "man"
	    {:cat :noun
	     :number :singular
	     :gender :masc
	     :fn "noun-fn"})
	     
(add-lexeme "donna" "woman"
	    {:cat :noun
	     :number :singular
	     :gender :fem
	     :fn "noun-fn"})

(add-lexeme "pane" "bread"
	    {:cat :noun
	     :number :singular
	     :gender :masc
	     :fn "noun-fn"})

(add-lexeme "pasta" "pasta"
	    {:cat :noun
	     :number :singular
	     :gender :fem
	     :fn "noun-fn"})

(add-lexeme "libro" "book"
	    {:cat :noun
	     :number :singular
	     :gender :masc
	     :writable true
	     :fn "noun-fn"})
(add-lexeme "abito" "dress"
	    {:cat :noun
	     :number :singular
	     :gender :masc
	     :writable true
	     :fn "noun-fn"})

;; adjectives
(add-lexeme "bianco" "white"
	    {:cat :adjective})
(add-lexeme "nero" "black"
	    {:cat :adjective})
(add-lexeme "forte" "strong"
	    {:cat :adjective})

(add-lexeme "sinistra" "left"
	    {:cat :adjective})
(add-lexeme "destra" "right"
	    {:cat :adjective})

;; sentences

(add-lexeme "ha gli occhi azzuri" "he has blue eyes"
	    {:person "3rd" :number :singular :cat :verb})
(add-lexeme "ha i cappelli non molte lunghi" "he has not very long hair"
	    {:person "3rd" :number :singular :cat :verb})
(add-lexeme "ha il naso alla francese" "he has a french nose" 
	    {:person "3rd" :number :singular :cat :verb})
(add-lexeme "non lo so" "i don't know"
	    {:cat :verb})
(add-lexeme "come sono?" "how?"
	    {:cat :verb})
(add-lexeme "cosa fa?" "what?"
	    {:cat :verb})

;; adjectives
(add-lexeme "alto" "tall"
	    {:cat :adjective})
(add-lexeme "basso" "short"
	    {:cat :adjective})
(add-lexeme "giovano" "young"
	    {:cat :adjective})
(add-lexeme "anziano" "old"
	    {:cat :adjective})
(add-lexeme "margra" "lean"
	    {:cat :adjective})
(add-lexeme "grasso" "fat"
	    {:cat :adjective})
(add-lexeme "bello" "beautiful"
	    {:cat :adjective})
(add-lexeme "bruto" "ugly"
	    {:cat :adjective})
(add-lexeme "carino" "cute"
	    {:cat :adjective})
(add-lexeme "lunghi" "long"
	    {:cat :adjective})
(add-lexeme "corti" "short"
	    {:cat :adjective})
(add-lexeme "calvo" "bald"
	    {:cat :adjective})
(add-lexeme "bruno" "brown"
	    {:cat :adjective})
(add-lexeme "bianchi" "white"
	    {:cat :adjective})
(add-lexeme "di mezza eta" "middle-aged"
	    {:cat :adjective})
(add-lexeme "qui" "here"
	    {:cat :adjective})

(defn verb-table [lexicon]
  (html [:table 
	(for [verb (sort (keys lexicon))]
	     (verb-row verb))]))

