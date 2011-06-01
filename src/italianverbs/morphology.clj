;; RESTARTING OF RING REQUIRED FOR CHANGES TO THIS FILE. (maybe not actually)
(ns italianverbs.morphology
  (:use [hiccup core page-helpers]
	[somnium.congomongo])
  (:require
   [clojure.string :as string]
   [italianverbs.html :as html]
   [clojure.contrib.string :as stringc]
   [clojure.contrib.str-utils2 :as str-utils]))

(defn get-head [sign]
  (if (get sign :head)
    (get-head (get sign :head))
    sign))

(defn remove-to [english-verb-phrase]
  (let [english (get english-verb-phrase :english)]
    (let [regex #"^to[ ]+(.*)"]
      (let [string
            (str-utils/replace english regex (fn [[_ rest]] (str rest)))]
        (merge
         {:remove-to string}
         english-verb-phrase)))))

(defn add-s-to-first-word [english-verb-phrase]
  (let [english-verb-string (get english-verb-phrase :english)]
    (let [regex #"^[ ]*([^ ]+)[ ]*(.*)"
          with-s
          (str-utils/replace
           english-verb-string
           regex
           (fn [[_ first-word rest]]
             (str first-word (if (re-find #"o$" first-word) "e") "s" " " rest)))]
      (merge
       {:add-s with-s}
       english-verb-phrase))))
    
(defn conjugate-english-verb [verb-head subject & [conjugate-verb-as]]
  ;; conjugate verb based on subject and eventually verb's features (such as tense)
  (let [vp verb-head
        verb-head (if (get verb-head :head)
                    (get verb-head :head)
                    verb-head)
        english (get verb-head :english)
        remove-to (remove-to verb-head)
        irregular ;; c.f.: (conjugate-italian-verb)
        (fetch-one :lexicon
                   :where {
                           :cat :verb
                           :infl (get vp :infl)
                           :person (get (get-head subject) :person)
                           :number (get (get-head subject) :number)
                           :root.english (get (get-head verb-head) :english)})]
    (cond
     (get irregular :english)
     (str (get irregular :english) " "
          (get (get vp :comp) :english))

     (= (get (get-head verb-head) :english) "must")
     (str
      (stringc/replace-re
       #"^must to"
       "must"
       (get verb-head :english))
      " "
      (get (get vp :comp) :english))

     (= (get (get-head verb-head) :english) "to be able")
     (str
      (stringc/replace-re
       #"^to be able( to)?"
       "can"
       (get verb-head :english))
      " "
      (get (get vp :comp) :english))

     (= (get (get-head subject) :person) "1st")
     (str
      (get remove-to :remove-to)
      " "
      (get (get vp :comp) :english))

     (= (get (get-head subject) :person) "2nd")
     (str
      (get remove-to :remove-to)
      " "
      (get (get vp :comp) :english))
      

     (and
      (or (= (get (get-head subject) :person) "3rd")
          (= (get (get-head subject) :person) :3rd))
      (or (= (get (get-head subject) :number) "singular")
          (= (get (get-head subject) :number) :singular))
      (or (= (get (get-head verb-head) :infl) :present)
          (= (get conjugate-verb-as :infl) :present)))
     (str (get
           (add-s-to-first-word
            (merge
             remove-to
             {:english (get remove-to :remove-to)}))
           :add-s)
          " "
          (get (get vp :comp) :english))

     true ;; 3rd plural
     (str (get remove-to :remove-to)
          " "
          (get (get vp :comp) :english)))))

(defn final-char-of [string]
  (str-utils/get string (- (.length string) 1)))

(defn if-isco [verb]
  (if (= (get verb :isco) true)
    "isc"
    ""))

(defn conjugate-italian-verb-regular [verb-head subject-head]
  (let [root-form (get verb-head :italian)
        regex #"^([^ ]*)([aei])re[ ]*$"]
    (cond

     (and (or (= (get subject-head :person) "1st")
              (= (get subject-head :person) :1st))
          (or (= (get subject-head :number) "singular")
              (= (get subject-head :number) :singular)))
     (str-utils/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem
                                                        (if-isco verb-head)
                                                        "o"
                                                        space)))

     (and (or (= (get subject-head :person) "1st")
              (= (get subject-head :person) :1st))
          (or (= (get subject-head :number) "plural")
              (= (get subject-head :number) :plural)))
     (str-utils/replace root-form regex
                        (fn [[_ stem i space]] (str stem
                                                    (if-isco verb-head)
                                                    (if (not (= (final-char-of stem) (final-char-of "i"))) "i" "")
                                                    "amo" space)))

     (and (or (= (get subject-head :person) "2nd")
              (= (get subject-head :person) :2nd))
          (or (= (get subject-head :number) :singular)
              (= (get subject-head :number) "singular")))
     (str-utils/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem
                                                        (if-isco verb-head)
                                                        (if (not (= (final-char-of stem) (final-char-of "i"))) "i" "")
                                                        space)))

     (and (or (= (get subject-head :person) "2nd")
              (= (get subject-head :person) :2nd))
          (or (= (get subject-head :number) "plural")
              (= (get subject-head :number) :plural)))
     (str-utils/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem
                                                        (if-isco verb-head)
                                                        vowel "te" space)))

     (and (or (= (get subject-head :person) "3rd")
              (= (get subject-head :person) :3rd))
          (or (= (get subject-head :number) "singular")
              (= (get subject-head :number) :singular)))
     ;; TODO: this works for -ire verbs like aprire->aprie but not
     ;; -ire verbs like finire->finisco.
     (str-utils/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem
                                                        (if-isco verb-head)
                                                        (cond
                                                         (= vowel "a") "a"
                                                         true "e")
                                                        space)))

     (and (or (= (get subject-head :person) "3rd")
              (= (get subject-head :person) :3rd))
          (or (= (get subject-head :number) "plural")
              (= (get subject-head :number) :plural)))
     (str-utils/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem
                                                        (if-isco verb-head)
                                                        (cond
                                                         (= vowel "a") "a"
                                                         true "o")
                                                        "no" space)))
     true
     (str "<tt><i>error: :person or :number value was not matched</i>. (<b>conjugate-italian-verb-regular</b> " (get verb-head :italian) ",(phrase with head:'" (get subject-head :italian) "'))</i></tt>"))))

(defn get-root-head [sign]
  (cond
   (get sign :head)
   (get-root-head (get sign :head))
   true
   sign))

;; TODO: figure out how to interpolate variables into regexps.
(defn except-first-words [first-words words]
  (let [regex #"^[^ ]+[ ]?(.*)"]
    (str-utils/replace words regex (fn [[_ rest]] rest))))

(defn plural-masc [italian]
  (stringc/join " "
                (cons (stringc/replace-re #"[o]$" "i" (first (stringc/split #"\s+"
                                                                          italian)))
                      (rest (stringc/split #"\s+" italian)))))

(defn plural-fem [italian]
  (stringc/join " "
                (cons (stringc/replace-re #"[oa]$" "e" (first (stringc/split #"\s+"
                                                                          italian)))
                      (rest (stringc/split #"\s+" italian)))))

(defn single-fem [italian]
  (stringc/join " "
                (cons (stringc/replace-re #"[o]$" "a" (first (stringc/split #"\s+"
                                                                            italian)))
                      (rest (stringc/split #"\s+" italian)))))

(defn conjugate-passato-prossimo [verb-phrase subject]
  (cond

   (and (= (get verb-phrase :aux) "essere")
        (or (= (get subject :gender) :fem)
            (= (get subject :gender) "fem"))
        (or (= (get subject :number) :singular)
            (= (get subject :number) "singular")))
   (single-fem (get verb-phrase :italian))

   (and (= (get verb-phrase :aux) "essere")
        (or (= (get subject :gender) :masc)
            (= (get subject :gender) "masc")
            (not (get subject :gender)))
        (or (= (get subject :number) :plural)
            (= (get subject :number) "plural")))
   (plural-masc (get verb-phrase :italian))

   (and (= (get verb-phrase :aux) "essere")
        (or (= (get subject :gender) :fem)
            (= (get subject :gender) "fem"))
        (or (= (get subject :number) :plural)
            (= (get subject :number) "plural")))
   (plural-fem (get verb-phrase :italian))
   true (get verb-phrase :italian)))

(defn conjugate-italian-verb [verb-phrase subject]
  ;; conjugate verb based on subject and eventually verb's features (such as tense)
  ;; takes two feature structures and returns a string.
  (cond
   (or (= (get verb-phrase :infl) :passato-prossimo)
       (= (get verb-phrase :infl) "passato-prossimo"))
   (conjugate-passato-prossimo verb-phrase subject)
   true
   (let [italian (get verb-phrase :italian)
         italian-head (get (get-head verb-phrase) :italian)
         ;; all we need is the head, which has the relevant grammatical information, not the whole subject
         subject (get-head subject)] 
     (let [italian (if italian-head italian italian)]
       (let [irregular
             (fetch-one :lexicon
                        :where {:cat :verb
                                :infl :present
                                :person (get subject :person)
                               :number (get subject :number)
                                :root.italian (get (get-root-head verb-phrase) :italian)
                                }
                        )
             except-first
             (except-first-words
              (get-head verb-phrase)
              (get verb-phrase :italian))]
         (if irregular
           (string/join " "
                        (list
                         (get irregular :italian)
                         except-first))
           (string/join (list
                         " "
                         (conjugate-italian-verb-regular
                          (get-head verb-phrase) subject)
                         except-first))))))))

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

(defn italian-article [det noun]
  "do italian det/noun morphology e.g. [def :def] + studente => lo studente" 
  ;; TODO: return a feature structure holding the current return value in :italian.
  (let [det-italian (get det :italian)
        det-noun (get noun :italian)]
    (cond
     (and (re-find #"^[aeiou]" (get noun :italian))
          ;; TODO: figure out why we need to check for both string ("def") and symbol (:def)
          ;; probably has to do with mongo to clojure mapping.
          (or (= (get det :def) "def")
              (= (get det :def) :def))
          (or (= (get noun :number) "singular")
              (= (get noun :number) :singular))
          ;; for numbers, "l'una" but "le otto", only match "una" here.
          ;; not sure about other numbers that start with "una", if any.
          (or (not (= (get noun :numerical) true))
              (and (= (get noun :numerical) true)
                   (= (get noun :italian) "una"))))
     (str "l'" (get noun :italian))

     (and (re-find #"^st" (get noun :italian))
          (= (get det :def) "def")
          (= (get noun :number) "singular")
          (= (get noun :gender) "masc"))
     (str "lo " (get noun :italian))

     (and (re-find #"^(st|[aeiou])" (get noun :italian))
          (= (get det :def) "def")
          (= (get noun :number) "plural")
          (= (get noun :gender) "masc"))
     (str "gli " (get noun :italian))

     (and (re-find #"^[aeiou]" (get noun :italian))
          (= (get det :def) "indef")
          (= (get noun :number) "singular")
          (= (get noun :gender) "masc"))
     (str "un'" (get noun :italian))

     (and (re-find #"^st" (get noun :italian))
          (= (get det :def) "indef")
          (= (get noun :number) "singular")
          (= (get noun :gender) "masc"))
     (str "uno " (get noun :italian))

     (and (= (get det :def) "part")
          (= (get noun :gender) "fem"))
     (str "delle " (get noun :italian))

     (and (= (get det :def) "part")
          (= (get noun :gender) "masc")
          (re-find #"^(st|[aeiou])" (get noun :italian)))
     (str "degli " (get noun :italian))

     (and (= (get det :def) "part")
          (= (get noun :gender) "masc"))
     (str "dei " (get noun :italian))
     
     true (str det-italian " " det-noun))))


(defn replace-from-list [regexp-list target]
  "Apply the first regexp pair (from=>to) from regexp-list to target;
   if this regexp changes target, return changed string,
   otherwise, try next regexp."
  (if (> (count regexp-list) 0)
    (let [regexp-pair (first regexp-list)
          regexp-from (first regexp-pair)
          regexp-to (second regexp-pair)
          result (stringc/replace-re regexp-from regexp-to target)]
      (if (= result target)
        (replace-from-list (rest regexp-list) target)
        result))
    target))

(defn conjugate-italian-prep [prep np]
  (let [concat (str (get prep :italian)
                    " "
                    (get np :italian))]
    (replace-from-list
     (list
      (list #"\ba il " "al ")
      (list #"\ba lo " "allo ")
      (list #"\ba la " "alla ")
      (list #"\ba l'" "all'")
      (list #"\ba i " "ai ")
      (list #"\ba gli " "agli ")
      (list #"\ba le " "alle ")
      
      (list #"\bda il " "dal ")
      (list #"\bda lo " "dallo ")
      (list #"\bda la " "dalla ")
      (list #"\bda l'" "dall'")
      (list #"\bda i " "dai ")
      (list #"\bda gli " "dagli ")
      (list #"\bda le " "dalle ")

      (list #"\bde il " "del ")
      (list #"\bde lo " "dello ")
      (list #"\bde la " "della ")
      (list #"\bde l'" "dell'")
      (list #"\bde i " "dei ")
      (list #"\bde gli " "degli ")
      (list #"\bde le " "delle ")
      
      (list #"\bdi il " "del ")
      (list #"\bdi lo " "dello ")
      (list #"\bdi la " "della ")
      (list #"\bdi l'" "dell'")
      (list #"\bdi i " "dei ")
      (list #"\bdi gli " "degli ")
      (list #"\bdi le " "delle ")
      
      (list #"\bin il " "nel ")
      (list #"\bin lo " "nello ")
      (list #"\bin la " "nella ")
      (list #"\bin l'" "nell'")
      (list #"\bin i " "nei ")
      (list #"\bin gli " "negli ")
      (list #"\bin le " "nelle ")

      (list #"\bsu il " "sul ")
      (list #"\bsu lo " "sullo ")
      (list #"\bsu la " "sulla ")
      (list #"\bsu l'" "sull'")
      (list #"\bsu i " "sui ")
      (list #"\bsu gli " "sugli ")
      (list #"\bsu le " "sulle ")

      
      )
     concat)))
