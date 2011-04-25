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
    
(defn conjugate-english-verb [verb-head subject]
  ;; conjugate verb based on subject and eventually verb's features (such as tense)
  (let [english (get verb-head :english)
        remove-to (remove-to verb-head)]
    (cond
     (= (get (get-head verb-head) :english) "must")
     (stringc/replace-re
      #"^must to"
      "must"
      (get verb-head :english))
     (= (get (get-head verb-head) :english) "to be able")
     (stringc/replace-re
      #"^to be able to"
      "can"
      (get verb-head :english))
     (= (get (get-head subject) :person) "1st")
     (get remove-to :remove-to)
     (= (get (get-head subject) :person) "2nd")
     (get remove-to :remove-to)
     (and
      (= (get (get-head subject) :person) "3rd")
      (= (get (get-head subject) :number) "singular"))
     (get
      (add-s-to-first-word
       (merge
        remove-to
        {:english (get remove-to :remove-to)}))
      :add-s)
     true ;; 3rd plural
     (get remove-to :remove-to))))

(defn conjugate-italian-verb-regular [verb-head subject-head]
  (let [root-form (get verb-head :italian)
        regex #"^([^ ]*)([aei])re[ ]*$"]
    (cond
     (and (= (get subject-head :person) "1st")
          (= (get subject-head :number) "singular"))
     (str-utils/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem "o" space)))
     (and (= (get subject-head :person) "1st")
          (= (get subject-head :number) "plural"))
     (str-utils/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem "i" "amo" space)))
     (and (= (get subject-head :person) "2nd")
          (= (get subject-head :number) "singular"))
     (str-utils/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem "i" space)))
     (and (= (get subject-head :person) "2nd")
          (= (get subject-head :number) "plural"))
     (str-utils/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem vowel "te" space)))
     (and (= (get subject-head :person) "3rd")
          (= (get subject-head :number) "singular"))
     (str-utils/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem "e" space)))
     (and (= (get subject-head :person) "3rd")
          (= (get subject-head :number) "plural"))
     (str-utils/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem
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

(defn conjugate-italian-verb [verb-phrase subject]
  ;; conjugate verb based on subject and eventually verb's features (such as tense)
  ;; takes two feature structures and returns a string.
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
                       (list (get irregular :italian)
                             except-first))
          (string/join " " 
                       (list
                        (conjugate-italian-verb-regular
                         (get-head verb-phrase) subject)
                        except-first)))))))

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

(defn italian-article [det noun]
  "do italian det/noun morphology e.g. [def :def] + studente => lo studente" 
  (let [det-italian (get det :italian)
        det-noun (get noun :italian)]
    (cond
     (and (re-find #"^[aeiou]" (get noun :italian))
          (= (get det :def) "def")
          (= (get noun :number) "singular"))
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
      (list #"^di il " "del ")
      (list #"^di lo " "dello ")
      (list #"^di la " "della ")
      (list #"^di l'" "dell'")
      (list #"^di i " "dei ")
      (list #"^di gli " "degli ")
      (list #"^di le " "delle ")

      (list #"^a il " "al ")
      (list #"^a lo " "allo ")
      (list #"^a la " "alla ")
      (list #"^a l'" "all'")
      (list #"^a i " "ai ")
      (list #"^a gli " "agli ")
      (list #"^a le " "alle ")
      
      (list #"^da il " "dal ")
      (list #"^da lo" "dallo ")
      (list #"^da la" "dalla ")
      (list #"^da l'" "dall'")
      (list #"^da i " "dai ")
      (list #"^da gli " "dagli ")
      (list #"^da le" "dalle ")

      (list #"^in il " "nel ")
      (list #"^in lo" "nello ")
      (list #"^in la" "nella ")
      (list #"^in l'" "nell'")
      (list #"^in i " "nei ")
      (list #"^in gli " "negli ")
      (list #"^in le" "nelle ")

      (list #"^su il " "sul ")
      (list #"^su lo" "sullo ")
      (list #"^su la" "sulla ")
      (list #"^su l'" "sull'")
      (list #"^su i " "sui ")
      (list #"^su gli " "sugli ")
      (list #"^su le" "sulle ")

      
      )
     concat)))
