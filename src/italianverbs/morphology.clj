;; RESTARTING OF RING REQUIRED FOR CHANGES TO THIS FILE. (maybe not actually)
(ns italianverbs.morphology
  (:require
   [italianverbs.fs :as fs]
   [clojure.tools.logging :as log]
   [clojure.string :as string]))

(defn conjugate-it [arg]
  "conjugate an italian expression."
;  (pprint (str "conjugating: " arg))
  (cond (nil? arg) ""
        (= (type arg) java.lang.String)
        arg
        (and (map? arg)
             (contains? (set (keys arg)) :1)
             (contains? (set (keys arg)) :2))
        (let [result1 (conjugate-it (:1 arg))
              result2 (conjugate-it (:2 arg))]
          (if (and (= (type result1) java.lang.String)
                   (= (type result2) java.lang.String))
            (string/join " " (list result1 result2))
            {:1 result1
             :2 result2}))

        (and (map? arg)
             (contains? (set (keys arg)) :agr)
             (contains? (set (keys arg)) :infinitive)
             ;; TODO: check (= :present) rather than (not (= :past)).
             (not (= :past (fs/get-in arg '(:infl) :notfound)))
             (not (= java.lang.String (type (fs/get-in arg '(:infinitive))))))
        ;; irregular present-tense (e.g. "fare")
        (let [root (fs/get-in arg '(:infinitive))
              person (fs/get-in arg '(:agr :person))
              number (fs/get-in arg '(:agr :number))
              present (fs/get-in arg '(:infinitive :irregular :present))]
          (cond
           (and (= person :1st)
                (= number :sing))
           (fs/get-in present '(:1sing))
           (and (= person :2nd)
                (= number :sing))
           (fs/get-in present '(:2sing))
           (and (= person :3rd)
                (= number :sing))
           (fs/get-in present '(:3sing))
           (and (= person :1st)
                (= number :plur))
           (fs/get-in present '(:1plur))
           (and (= person :2nd)
                (= number :plur))
           (fs/get-in present '(:2plur))
           (and (= person :3rd)
                (= number :plur))
          (fs/get-in present '(:3plur))
           :else arg))  ;(str "[unknown conjugation:root=" root ";person=" person ";number=" number "]:" root)))

        (= (type arg) clojure.lang.Keyword)
        (str "cannot conjugate: " arg)

        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :gender)) :fem)
             (= (fs/get-in arg '(:agr :number)) :sing))
        (fs/get-in arg '(:root))

        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :gender)) :masc)
             (= (fs/get-in arg '(:agr :number)) :sing))
        (fs/get-in arg '(:root))
        
        ;; feminine noun pluralization
        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :gender)) :fem)
             (= (fs/get-in arg '(:agr :number)) :plur))
        (string/replace (fs/get-in arg '(:root))
                        #"a$" "e")

        ;; masculine noun pluralization
        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :gender)) :masc)
             (= (fs/get-in arg '(:agr :number)) :plur))
        (string/replace (fs/get-in arg '(:root))
                        #"[eo]$" "i") ;; dottore => dottori; medico => medici


        ;; has a :root and :agr, therefore noun, and
        ;; number not specified: use root form by default.
        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr))
        (str (fs/get-in arg '(:root)))

        ;; irregular passato prossimo.
        (and (map? arg)
             (contains? arg :infl)
             (= (fs/get-in arg '(:infl)) :past)
             (not (= (fs/get-in arg '(:infinitive :irregular :passato) :notfound)
                     :notfound)))
        (str (fs/get-in arg '(:infinitive :irregular :passato)))

        (and (map? arg)
             (contains? arg :infl)
             (= (fs/get-in arg '(:infl)) :past))

        ;; regular past inflection
        (let [root (fs/get-in arg '(:infinitive))
              root (if root root "(no-root-found)")
              are-type (re-find #"are$" root)
              ere-type (re-find #"ere$" root)
              ire-type (re-find #"ire$" root)
              stem (string/replace root #"[iae]re$" "")
              last-stem-char-is-i (re-find #"i$" stem)]
          (cond (or are-type ere-type)
                (str stem "ato")
                (or are-type ire-type)
                (str stem "ito")
                true
                (str "(regpast:TODO)")))

        
        :else
        ;; assume present tense verb with map with keys (:root and :agr).
        (let [root (fs/get-in arg '(:infinitive))
              root (if (nil? root) "(nil)" root)
              root (if (not (= (type root) java.lang.String))
                      (fs/get-in arg '(:infinitive :infinitive))
                      root)
              person (fs/get-in arg '(:agr :person))
              number (fs/get-in arg '(:agr :number))
              stem (string/replace root #"[iae]re$" "")
              are-type (re-find #"are$" root)
              ere-type (re-find #"ere$" root)
              ire-type (re-find #"ire$" root)
              last-stem-char-is-i (re-find #"i$" stem)]
          (cond

           (and (= person :1st) (= number :sing))
           (str stem "o")

           (and (= person :2nd) (= number :sing)
                last-stem-char-is-i)
           (str stem)

           (and (= person :2nd) (= number :sing))
           (str stem "i")

           (and (= person :3rd) (= number :sing) (or ire-type ere-type))
           (str stem "e")

           (and (= person :3rd) (= number :sing) are-type)
           (str stem "a") 

           (and (= person :1st) (= number :plur)
                last-stem-char-is-i)
           (str stem "amo")

           (and (= person :1st) (= number :plur))
           (str stem "iamo")

           (and (= person :2nd) (= number :plur) are-type)
           (str stem "ate")

           (and (= person :2nd) (= number :plur) ere-type)
           (str stem "ete")

           (and (= person :2nd) (= number :plur) ire-type)
           (str stem "ite")

           (and (= person :3rd) (= number :plur))
           (str stem "ano")
           :else
           arg))))

(defn conjugate-en [arg]
  (cond (nil? arg) ""
        (= (type arg) java.lang.String)
        arg
        (and (map? arg)
             (contains? (set (keys arg)) :1)
             (contains? (set (keys arg)) :2))
        (let [result1 (conjugate-en (:1 arg))
              result2 (conjugate-en (:2 arg))]
          (if (and (= (type result1) java.lang.String)
                   (= (type result2) java.lang.String))
            (string/join " " (list result1 result2))
            {:1 result1
             :2 result2}))

        (and (map? arg)
             (contains? (set (keys arg)) :agr)
             (contains? (set (keys arg)) :infinitive)
             (not (= :notfound (fs/get-in arg '(:infinitive :irregular :present) :notfound)))
             ;; TODO: check (= :present) rather than (not (= :past)).
             (not (= :past (fs/get-in arg '(:infl) :notfound)))
             (not (= java.lang.String (type (fs/get-in arg '(:infinitive))))))
        ;; irregular present-tense (e.g. "fare")
        (let [root (fs/get-in arg '(:infinitive))
              person (fs/get-in arg '(:agr :person))
              number (fs/get-in arg '(:agr :number))
              present (fs/get-in arg '(:infinitive :irregular :present))]
          (cond
           (and (= person :1st)
                (= number :sing))
           (fs/get-in present '(:1sing))
           (and (= person :2nd)
                (= number :sing))
           (fs/get-in present '(:2sing))
           (and (= person :3rd)
                (= number :sing))
           (fs/get-in present '(:3sing))
           (and (= person :1st)
                (= number :plur))
           (fs/get-in present '(:1plur))
           (and (= person :2nd)
                (= number :plur))
           (fs/get-in present '(:2plur))
           (and (= person :3rd)
                (= number :plur))
          (fs/get-in present '(:3plur))
           :else arg))  ;(str "[unknown conjugation:root=" root ";person=" person ";number=" number "]:" root)))

        (= (type arg) clojure.lang.Keyword)
        (str "cannot conjugate: " arg)

        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :number)) :sing))
        (str (fs/get-in arg '(:root)))

        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :number)) :plur))
        (str (fs/get-in arg '(:root)) "s")

        ;; number not specified: use root form by default.
        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr))
        (str (fs/get-in arg '(:root)))

        ;; irregular past.
        (and (map? arg)
             (contains? arg :infl)
             (= (fs/get-in arg '(:infl)) :past)
             (not (= (fs/get-in arg '(:infinitive :irregular :past) :notfound)
                     :notfound)))
        (str (fs/get-in arg '(:infinitive :irregular :past)))

        (and (map? arg)
             (contains? arg :infl)
             (= (fs/get-in arg '(:infl)) :past))
        (let [root (fs/get-in arg '(:infinitive))
              root (if (nil? root) "(nilroot)" root)
              root (if (not (= (type root) java.lang.String))
                     (fs/get-in arg '(:infinitive :infinitive))
                     root)
              stem (string/replace root #"^to " "")
              stem-minus-one (nth (re-find #"(.*).$" stem) 1)
              penultimate-stem-char (nth (re-find #"(.).$" stem) 1)
              last-stem-char (re-find #".$" stem)
              last-stem-char-is-e (re-find #"e$" stem)]
          (cond last-stem-char-is-e  ;; e.g. "write" => "written"
                (str stem-minus-one penultimate-stem-char "en")
                true
                (str stem "en")))
        
        :else
        ;; assume a map with keys (:infinitive and :agr).
        (let [root (fs/get-in arg '(:infinitive))
              root (if (nil? root) "(nilroot)" root)
              root (if (not (= (type root) java.lang.String))
                      (fs/get-in arg '(:infinitive :infinitive))
                      root)
              person (fs/get-in arg '(:agr :person))
              number (fs/get-in arg '(:agr :number))
              stem (string/replace root #"^to " "")
              last-stem-char-is-e (re-find #"e$" stem)]
          (cond

           (and (= person :1st) (= number :sing))
           (str stem "")

           (and (= person :2nd) (= number :sing))
           (str stem "")

           (and (= person :3rd) (= number :sing))
           (str stem "s")

           (and (= person :1st) (= number :plur))
           (str stem "")

           (and (= person :2nd) (= number :plur))
           (str stem "")

           (and (= person :3rd) (= number :plur))
           (str stem "")
           :else arg))))

(defn get-italian [a b]
  (let [conjugated-a (conjugate-it a)
        conjugated-b (if (not (nil? b)) (conjugate-it b) "..")]
    (if (and
         (string? conjugated-a)
         (string? conjugated-b))
      (string/trim
       (cond

        (and (= conjugated-a "il")
             (re-find #"^s[t]" conjugated-b))
        (str "lo " conjugated-b)

        (and (= conjugated-a "un")
             (re-find #"^s[t]" conjugated-b))
        (str "uno " conjugated-b)

        (and (= conjugated-a "i")
             (re-find #"^[aeiou]" conjugated-b))
        (str "gli " conjugated-b)
        (and (= conjugated-a "i")
             (re-find #"^s[t]" conjugated-b))
        (str "gli " conjugated-b)

        (and (= conjugated-a "il")
             (re-find #"^[aeiou]" conjugated-b))
        (str "l'" conjugated-b)

        (and (= conjugated-a "la")
             (re-find #"^[aeiou]" conjugated-b))
        (str "l'" conjugated-b)


        ;; partitivi
        (and (= conjugated-a "di il")
             (re-find #"^s[t]" conjugated-b))
        (str "dello " conjugated-b)

        (and (= conjugated-a "di i")
             (re-find #"^s[t]" conjugated-b))
        (str "degli " conjugated-b)

        (and (= conjugated-a "di la")
             (re-find #"^[aeiou]" conjugated-b))
        (str "del'" conjugated-b)
        
        (= conjugated-a "di il")
        (str "del " conjugated-b)
        (= conjugated-a "di la")
        (str "della " conjugated-b)
        
        (= conjugated-a "di i")
        (str "dei " conjugated-b)
        (= conjugated-a "di le")
        (str "delle " conjugated-b)


        ;; prepositions
        (and (= conjugated-a "a")
             (re-find #"^il " conjugated-b))
        (str "al " (string/replace conjugated-b #"^il " ""))
        
        true
        (string/trim (str conjugated-a " " conjugated-b))))
      {:1 conjugated-a
       :2 conjugated-b})))

(defn get-english [a b]
  (let [conjugated-a (conjugate-en a)
        conjugated-b (if (not (nil? b)) (conjugate-en b) "..")]
    (if (and
         (= (type conjugated-a) java.lang.String)
         (= (type conjugated-b) java.lang.String))
      (string/trim (str conjugated-a " " conjugated-b))
      {:1 conjugated-a
       :2 conjugated-b})))

(defn remove-to [english-verb-phrase]
  (let [english (get english-verb-phrase :english)]
    (let [regex #"^to[ ]+(.*)"]
      (let [string
            (string/replace english regex (fn [[_ rest]] (str rest)))]
        (merge
         {:remove-to string}
         english-verb-phrase)))))

(defn add-s-to-first-word [english-verb-phrase]
  (let [english-verb-string (get english-verb-phrase :english)]
    (let [regex #"^[ ]*([^ ]+)[ ]*(.*)"
          with-s
          (replace
           english-verb-string
           regex
           (fn [[_ first-word rest]]
             (str first-word (if (re-find #"o$" first-word) "e") "s" " " rest)))]
      (merge
       {:add-s with-s}
       english-verb-phrase))))

(defn final-char-of [string]
  (get string (- (.length string) 1)))

(defn next-to-final-char-of [string]
  (if (> (.length string) 1)
    (get string (- (.length string) 2))))

(defn if-isco [verb]
  (if (= (get verb :isco) true)
    "isc"
    ""))

(defn conjugate-italian-verb-regular [verb-head subject-head]
  (let [root-form (fs/get-in-r verb-head '(:italian))
        regex #"^([^ ]*)([aei])re[ ]*$"]
    (log/info (str "conjugate-italian-verb-regular: " verb-head "," subject-head))
    (println (str "conjugate-italian-verb-regular: " verb-head "," subject-head))
    (cond

     (and (or (= (fs/get-in-r subject-head '(:person)) "1st")
              (= (fs/get-in-r subject-head '(:person)) :1st))
          (or (= (fs/get-in-r subject-head '(:number)) "singular")
              (= (fs/get-in-r subject-head '(:number)) :singular)))
     (string/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem
                                                        (if-isco verb-head)
                                                        "o"
                                                        space)))

     (and (or (= (fs/get-in-r subject-head '(:person)) "1st")
              (= (fs/get-in-r subject-head '(:person)) :1st))
          (or (= (fs/get-in-r subject-head '(:number)) "plural")
              (= (fs/get-in-r subject-head '(:number)) :plural)))
     (string/replace root-form regex
                        (fn [[_ stem i space]] (str stem
                                                    (if-isco verb-head)
                                                    
                                                    (if (and
                                                         (not (= (next-to-final-char-of stem) (final-char-of "s")))
                                                         (= (final-char-of stem) (final-char-of "c")))
                                                      "h" "")

                                                    (if (not (= (final-char-of stem) (final-char-of "i")))
                                                      "i" "")


                                                    
                                                    "amo" space)))

     (and (or (= (fs/get-in-r subject-head '(:person)) "2nd")
              (= (fs/get-in-r subject-head '(:person)) :2nd))
          (or (= (fs/get-in-r subject-head '(:number)) :singular)
              (= (fs/get-in-r subject-head '(:number)) "singular")))
     (string/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem
                                                        (if-isco verb-head)

                                                        (if (and
                                                             (not (= (next-to-final-char-of stem) (final-char-of "s")))
                                                             (= (final-char-of stem) (final-char-of "c")))
                                                          "h" "")
                                                        
                                                        (if (not (= (final-char-of stem) (final-char-of "i"))) "i" "")
                                                        space)))

     (and (or (= (fs/get-in-r subject-head '(:person)) "2nd")
              (= (fs/get-in-r subject-head '(:person)) :2nd))
          (or (= (fs/get-in-r subject-head '(:number)) "plural")
              (= (fs/get-in-r subject-head '(:number)) :plural)))
     (string/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem
                                                        (if-isco verb-head)
                                                        vowel "te" space)))

     (and (or (= (fs/get-in-r subject-head '(:person)) "3rd")
              (= (fs/get-in-r subject-head '(:person)) :3rd))
          (or (= (fs/get-in-r subject-head '(:number)) "singular")
              (= (fs/get-in-r subject-head '(:number)) :singular)))
     ;; TODO: this works for -ire verbs like aprire->aprie but not
     ;; -ire verbs like finire->finisco.
     (string/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem
                                                        (if-isco verb-head)
                                                        (cond
                                                         (= vowel "a") "a"
                                                         true "e")
                                                        space)))

     (and (or (= (fs/get-in-r subject-head '(:person)) "3rd")
              (= (fs/get-in-r subject-head '(:person)) :3rd))
          (or (= (fs/get-in-r subject-head '(:number)) "plural")
              (= (fs/get-in-r subject-head '(:number)) :plural)))
     (string/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem
                                                        (if-isco verb-head)
                                                        (cond
                                                         (= vowel "a") "a"
                                                         true "o")
                                                        "no" space)))
     true
     (str "conjugate-italian-verb-regular error: :person (" (get subject-head :person) ") or :number (" (get subject-head :number) ") "
          " value was not matched. (verb-head=" (get verb-head :italian) "),(subject-head=" (get subject-head :italian) ")"))))

;; TODO: figure out how to interpolate variables into regexps.
(defn except-first-words [first-words words]
  (let [regex #"^[^ ]+[ ]?(.*)"]
    (string/replace words regex (fn [[_ rest]] rest))))

(defn plural-masc [italian]
  (string/join " "
               (cons (string/replace (first (string/split italian #"\s+"))
                                     #"[eo]$" (fn [x] "i"))
                     (rest (string/split italian #"\s+")))))

(defn plural-fem [italian]
  (string/join " "
               (cons (string/replace #"[oa]$" "e" (first (string/split #"\s+"
                                                                       italian)))
                     (rest (string/split italian #"\s+")))))

(defn single-fem [italian]
  (string/join " "
                (cons (string/replace #"[o]$" "a" (first (string/split #"\s+"
                                                                    italian)))
                      (rest (string/split italian #"\s+")))))

(defn plural-en [english]
  (if (re-find #"[hsx]$" english) (str english "es")
      (str english "s")))

(defn conjugate-passato-prossimo [verb-phrase subject]
  (cond

   (and (= (fs/get-r verb-phrase :aux) "essere")
        (or (= (fs/get-r subject :gender) :fem)
            (= (fs/get-r subject :gender) "fem"))
        (or (= (fs/get-r subject :number) :singular)
            (= (fs/get-r subject :number) "singular")))
   (single-fem (fs/get-r verb-phrase :italian))

   (and (= (fs/get-r verb-phrase :aux) "essere")
        (or (= (fs/get-r subject :gender) :masc)
            (= (fs/get-r subject :gender) "masc")
            (not (fs/get-r subject :gender)))
        (or (= (fs/get-r subject :number) :plural)
            (= (fs/get-r subject :number) "plural")))
   (plural-masc (fs/get-r verb-phrase :italian))

   (and (= (fs/get-r verb-phrase :aux) "essere")
        (or (= (fs/get-r subject :gender) :fem)
            (= (fs/get-r subject :gender) "fem"))
        (or (= (fs/get-r subject :number) :plural)
            (= (fs/get-r subject :number) "plural")))
   (plural-fem (fs/get-r verb-phrase :italian))
   true (fs/get-r verb-phrase :italian)))

(defn conjugate-italian-verb [verb-phrase subject]
  ;; conjugate verb based on subject and eventually verb's features (such as tense)
  ;; takes two feature structures and returns a string.
  (cond
   (or (= (get verb-phrase :infl) :passato-prossimo)
       (= (get verb-phrase :infl) "passato-prossimo"))
   (conjugate-passato-prossimo verb-phrase subject)
   true
   (let [italian (get verb-phrase :italian)
         italian-head (get (fs/get-head verb-phrase) :italian)
         ;; all we need is the head, which has the relevant grammatical information, not the whole subject
         subject (fs/get-head subject)] 
     (let [italian (if italian-head italian italian)]
       (let [except-first
             (except-first-words
              (fs/get-head verb-phrase)
              (get verb-phrase :italian))]
         (string/join (list
                       " "
                       (conjugate-italian-verb-regular
                        (fs/get-head verb-phrase) subject)
                       except-first)))))))

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
          result (string/replace regexp-from regexp-to target)]
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

(defn stem-per-futuro [infinitive]
  "_infinitive_ should be a string (italian verb infinitive form)"
  (string/replace infinitive #"^(.*)([aei])(re)$" (fn [[_ prefix vowel suffix]] (str prefix (if (= vowel "a") "e" vowel) "r"))))

(defn stem-per-passato-prossimo [infinitive]
  "_infinitive_ should be a string (italian verb infinitive form)"
  (string/replace infinitive #"^(.*)([aei])(re)$" (fn [[_ prefix vowel suffix]] (str prefix))))

(defn passato-prossimo [infinitive]
  (str (stem-per-passato-prossimo infinitive) "ato"))

;cyclic
;(defn conjugate [map]
;  (let [irregular (search/search-one map)]
;    (if irregular irregular
;        ;; else, regular.
;        (fs/merge map {:italian (passato-prossimo (:italian map))
;                       :english (str (:remove-to (remove-to map)) "en")}))))

(defn conjugate-future-italian [infinitive subject & [ stem ] ]
  (let [stem (if stem
               stem 
               (stem-per-futuro (get infinitive :italian)))]
    (cond
     (= (get subject :person)
        :1st)
     (cond (= (get subject :number)
              :singular)
           (str stem "ò")
           (= (get subject :number)
              :plural)
           (str stem "emo")
           true "??")
     (= (get subject :person)
        :2nd)
     (cond (= (get subject :number)
              :singular)
           (str stem "ai")
           (= (get subject :number)
              :plural)
           (str stem "ete")
           true "??")
     (= (get subject :person)
        :3rd)
     (cond (= (get subject :number)
              :singular)
           (str stem "à")
           (= (get subject :number)
              :plural)
           (str stem "anno")
           true "??")
     true
     "??")))

