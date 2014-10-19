(ns italianverbs.morphology
  (:refer-clojure :exclude [get-in str])
  (:use [italianverbs.unify :only (ref? get-in fail?)])
   (:require
    [clojure.core :as core]
    [clojure.tools.logging :as log]
    [clojure.string :as string]))

(defn remove-parens [str]
  (string/replace str #"\(.*\)" ""))

(defn str [ & args ]
  (string/trim (apply core/str args)))

(defn strip [str]
  "remove heading and trailing whitespace"
  (string/replace (string/replace str #"^\s+" "") #"\s+$" ""))

(defn phrase-is-finished? [phrase]
  (cond
   (string? phrase) true
   (map? phrase)
   (or (phrase-is-finished? (get-in phrase '(:italian)))
       (string? (get-in phrase '(:infinitive)))
       (and (phrase-is-finished? (get-in phrase '(:a)))
            (phrase-is-finished? (get-in phrase '(:b)))))
   :else false))

(declare get-english)
(declare plural-en)

(defn get-english-1 [word]
  (log/debug (str "get-english-1: " word))
  (cond
   (ref? word)
   (get-english-1 @word)

   (= word :top)
   ".."

   (and (map? word)
        (nil? (:a word))
        (nil? (:b word))
        (nil? (:english word))
        (nil? (:infinitive word)))
   ".."

   ;; "to do [past]" + "well" => "did well"
   (and (= (get-in word '(:cat)) :verb)
        (= (get-in word '(:infl)) :past)
        (string? (get-in word '(:a :irregular :past))))
   (str (get-in word '(:a :irregular :past)) " "
        (get-english-1 (get-in word '(:b))))

   ;; :note is used for little annotations that are significant in italian but not in english
   ;; e.g. gender signs (♂,♀) on nouns like "professore" and "professoressa".
   (and (string? (get-in word '(:english)))
        (string? (get-in word '(:note))))
   (str (strip (get-english-1 (dissoc word :note))) " " (strip (get-in word '(:note))))

   (= (get-in word '(:a)) :top)
   (str
    ".." " " (get-english-1 (get-in word '(:b))))

   ;; show elipsis (..) if :b is not specified.
   (and
    (= (get-in word '(:b)) :top)
    (string? (get-english-1 (get-in word '(:a)))))
   (str
    (get-english-1 (get-in word '(:a)))
    " " "..")

   ;; show elipsis (..) if :a is not specified.
   (and
    (= (get-in word '(:b)) :top)
    (string? (get-in word '(:a :english))))
   (str
    (get-english-1 (get-in word '(:a :english)))
    " " "..")

   (string? word)
   (strip word)

   ;; (could have) + (to go) => "could have gone"
   (and
    (get-in word '(:a))
    (get-in word '(:b))
    (string? (get-in word '(:a :irregular :past)))
    (= (get-in word '(:a :irregular :past)) "could have")
    (string? (get-in word '(:b :irregular :past-participle)))
    (= (get-in word '(:a :infl)) :past))
   (string/join " " (list (get-in word '(:a :irregular :past))
                          (get-in word '(:b :irregular :past-participle))))

   ;; (could have) + (to sleep) => "could have slept"
   (and
    (get-in word '(:a))
    (get-in word '(:b))
    (string? (get-in word '(:a :irregular :past)))
    (= (get-in word '(:a :irregular :past)) "could have")
    (string? (get-in word '(:b :irregular :past)))
    (= (get-in word '(:a :infl)) :past))
   (string/join " " (list (get-in word '(:a :irregular :past))
                          (get-in word '(:b :irregular :past))))

   ;; (could have) + (do X) => "could have done X"
   (and
    (get-in word '(:a))
    (get-in word '(:b))
    (string? (get-in word '(:a :irregular :past)))
    (= (get-in word '(:a :irregular :past)) "could have")
    (string? (get-in word '(:b :a :irregular :past-participle)))
    (= (get-in word '(:a :infl)) :past))
   ;; recursive call after inflecting '(:b :a) to past.
   (get-english {:a (get-in word '(:a))
                 :b {:a (get-in word '(:b :a :irregular :past-participle))
                     :b (get-in word '(:b :b))}})

   ;; (could have) + (make X) => "could have made X"
   (and
    (get-in word '(:a))
    (get-in word '(:b))
    (string? (get-in word '(:a :irregular :past)))
    (= (get-in word '(:a :irregular :past)) "could have")
    (string? (get-in word '(:b :a :irregular :past)))
    (= (get-in word '(:a :infl)) :past))
   ;; recursive call after inflecting '(:b :a) to past.
   (get-english {:a (get-in word '(:a))
                 :b {:a (get-in word '(:b :a :irregular :past))
                     :b (get-in word '(:b :b))}})

   (and
    (get-in word '(:a))
    (get-in word '(:b))
    (string? (get-in word '(:a)))
    (string? (get-in word '(:b))))
   (string/join " "
         (list (get-in word '(:a))
               (get-in word '(:b))))

   (and
    (get-in word '(:a))
    (get-in word '(:b)))
   (get-english (get-in word '(:a))
                (get-in word '(:b)))

   (and (= :infinitive (get-in word '(:infl)))
        (string? (get-in word '(:infinitive))))
   (get-in word '(:infinitive))

   (= true (get-in word '(:hidden)))
;;   "Ø"
   ""
   (and
    (= true (get-in word '(:a :hidden)))
    (= true (get-in word '(:b :hidden))))
;;   "Ø"
   ""
   (= true (get-in word '(:a :hidden)))
   (get-english-1 (get-in word '(:b)))

   (= true (get-in word '(:b :hidden)))
   (get-english-1 (get-in word '(:a)))

   (and (= (get-in word '(:infl)) :conditional)
        (get-in word '(:infinitive))
        (not (nil? (get-in word '(:agr :number))))
        (not (nil? (get-in word '(:agr :person)))))
   (let [infinitive (get-in word '(:infinitive))
         stem (string/replace infinitive #"^to " "")]
     (str "would " stem))

   (and (= (get-in word '(:infl)) :futuro)
        (get-in word '(:infinitive))
        (not (nil? (get-in word '(:agr :number))))
        (not (nil? (get-in word '(:agr :person)))))
   (let [infinitive (get-in word '(:infinitive))
         stem (string/replace infinitive #"^to " "")]
     (str "will " stem))

   (and (= (get-in word '(:infl)) :imperfetto)
        (= :plur (get-in word '(:agr :number)))
        (get-in word '(:irregular :imperfect :plur)))
   (str (get-in word '(:irregular :imperfect :plur)))

   (and (= (get-in word '(:infl)) :imperfetto)
        (= :sing (get-in word '(:agr :number)))
        (= :2nd (get-in word '(:agr :person)))
        (get-in word '(:irregular :imperfect :2sing)))
   (str (get-in word '(:irregular :imperfect :2sing)))

   ;; if neither of the two above does not match:
   (and (= (get-in word '(:infl)) :imperfetto)
        (map? (get-in word '(:irregular :imperfect))))
   (str (get-in word '(:irregular :imperfect :default)))

   (and (= (get-in word '(:infl)) :imperfetto)
        (get-in word '(:irregular :imperfect)))
   (str (get-in word '(:irregular :imperfect)))


   (and (= (get-in word '(:infl)) :imperfetto)
        (get-in word '(:infinitive)))
   (let [infinitive (get-in word '(:infinitive))
         stem (string/replace infinitive #"^to " "")
         to-final (re-find #" to$" stem) ;; occurs in e.g. "have to": in imperfect becomes "was having to"
         stem (string/replace stem #" to$" "")
         stem-minus-one (nth (re-find #"(.*).$" stem) 1)
         penultimate-stem-char (nth (re-find #"(.).$" stem) 1)
         penultimate-stem-char-is-vowel (or (= penultimate-stem-char "a")
                                            (= penultimate-stem-char "e")
                                            (= penultimate-stem-char "i")
                                            (= penultimate-stem-char "o")
                                            (= penultimate-stem-char "u"))
         last-stem-char (re-find #".$" stem)
         last-stem-char-is-e (re-find #"e$" stem)]
     ;; remove final "e", if any, before adding "e": e.g. "write" => "writing"
     (let [stem (if (and last-stem-char-is-e
                         (> (.length stem) 2) ;; don't apply this to "be".
                         (not penultimate-stem-char-is-vowel))
                  stem-minus-one
                  stem)]
       (cond

        ;; TODO: add support for per-agreement (by number or person) irregular imperfetto;
        ;; for now, only support for a single imperfetto irregular form for all agreements.
        ;; (might not be needed for english)

        ;; 2) use irregular that is the same for all number and person if there is one.
        (string? (get-in word '(:irregular :imperfetto)))
        (get-in word '(:irregular :imperfetto))

        (and (= :sing (get-in word '(:agr :number)))
             (or (= :1st (get-in word '(:agr :person)))
                 (= :3rd (get-in word '(:agr :person))))
             (string? (get-in word '(:irregular :imperfetto-suffix))))
        (str "was " (get-in word '(:irregular :imperfetto-suffix)))

        (string? (get-in word '(:irregular :imperfetto-suffix)))
        (str "were " (get-in word '(:irregular :imperfetto-suffix)))

        (and (= :sing (get-in word '(:agr :number)))
             (or (= :1st (get-in word '(:agr :person)))
                 (= :3rd (get-in word '(:agr :person)))))
        (str "was " stem "ing" (if to-final to-final ""))

        true
        (str "were " stem "ing" (if to-final to-final "")))))

   ;; irregular past (1): a single inflection for all persons/numbers.
   (and (= :past (get-in word '(:infl)))
        (string? (get-in word '(:irregular :past))))
   (get-in word '(:irregular :past))

   (and (= :past (get-in word '(:infl)))
        (= :top (get-in word '(:agr :number)))
        (string? (get-in word '(:irregular :past :2sing))))
   ;; use the 2nd singular form if there's not enough inflection info to go on.
   (str "[" (get-in word '(:irregular :past :2sing)) "]")

   (= :top (get-in word '(:infl)))
   (get-in word '(:infinitive))

   ;; irregular past (2): a different inflection for each persons/numbers.
   (and (= :past (get-in word '(:infl)))
        (map? (get-in word '(:irregular :past))))
   (let [number (get-in word '(:agr :number))
         person (get-in word '(:agr :person))]
     (cond (and (= person :1st) (= number :sing))
           (get-in word '(:irregular :past :1sing))
           (and (= person :2nd) (= number :sing))
           (get-in word '(:irregular :past :2sing))
           (and (= person :3rd) (= number :sing))
           (get-in word '(:irregular :past :3sing))
           (and (= person :1st) (= number :plur))
           (get-in word '(:irregular :past :1plur))
           (and (= person :2nd) (= number :plur))
           (get-in word '(:irregular :past :2plur))
           (and (= person :3rd) (= number :plur))
           (get-in word '(:irregular :past :3plur))
           true word)) ;; not enough agreement specified to conjugate.

   ;; regular past
   (and (= :past (get-in word '(:infl)))
        (string? (get-in word '(:infinitive))))
   (let [infinitive (get-in word '(:infinitive))
         stem (string/replace infinitive #"^to " "")
         stem-minus-one (nth (re-find #"(.*).$" stem) 1)
         penultimate-stem-char (nth (re-find #"(.).$" stem) 1)
         last-stem-char (re-find #".$" stem)
         last-stem-char-is-e (re-find #"e$" stem)]
     (cond last-stem-char-is-e
           (str stem-minus-one penultimate-stem-char "ed")
           true
           (str stem "ed")))

   (and
    (= :present (get-in word '(:infl)))
    (string? (get-in word '(:infinitive))))
   (let [root (get-in word '(:infinitive))
         ;; TODO: throw exception rather than encoding error "nilrootz" as part
         ;; of the english string.
         root (if (nil? root) "(nilrootz)" root)
         root (if (not (= (type root) java.lang.String))
                (get-in word '(:infinitive :infinitive))
                root)
         person (get-in word '(:agr :person))
         number (get-in word '(:agr :number))
         stem (string/replace root #"^to " "")
         last-stem-char-is-e (re-find #"e$" stem)
         last-stem-char-is-vowel (re-find #"[aeiou]$" stem)]
     (log/debug "+else")
     (log/debug (str "(english):word: " word))
     (cond

      (and (= person :1st) (= number :sing)
           (string? (get-in word '(:irregular :present :1sing))))
      (get-in word '(:irregular :present :1sing))
      (and (= person :2nd) (= number :sing)
           (string? (get-in word '(:irregular :present :2sing))))
      (get-in word '(:irregular :present :2sing))
      (and (= person :3rd) (= number :sing)
           (string? (get-in word '(:irregular :present :3sing))))
      (get-in word '(:irregular :present :3sing))

      (and (= person :1st) (= number :plur)
           (string? (get-in word '(:irregular :present :1plur))))
      (get-in word '(:irregular :present :1plur))
      (and (= person :2nd) (= number :plur)
           (string? (get-in word '(:irregular :present :2plur))))
      (get-in word '(:irregular :present :2plur))
      (and (= person :3rd) (= number :plur)
           (string? (get-in word '(:irregular :present :3plur))))
      (get-in word '(:irregular :present :3plur))

      (and (= person :1st) (= number :sing))
      (str stem "")

      (and (= person :2nd) (= number :sing))
      (str stem "")

      (and (= person :3rd) (= number :sing)
           (= last-stem-char-is-vowel "o"))
      (str stem "es")

      (and (= person :3rd) (= number :sing))
      (str stem "s")

      (and (= person :1st) (= number :plur))
      (str stem "")

      (and (= person :2nd) (= number :plur))
      (str stem "")

      (and (= person :3rd) (= number :plur))
      (str stem "")

      (string? (get-in word '(:infinitive)))
      (get-in word '(:infinitive))

      (string? (get-in word '(:english)))
      (get-in word '(:english))

      :else (str root )))

   (and
    (get-in word '(:irregular :plur))
    (= (get-in word '(:agr :number)) :plur)
    (= (get-in word '(:cat) :noun)))
   (get-in word '(:irregular :plur))


   ;; TODO: remove support for deprecated :root - use :irregular instead (as immediately above).
   (and
    (get-in word '(:root :irregular :plur))
    (= (get-in word '(:agr :number)) :plur)
    (= (get-in word '(:cat) :noun)))
   (get-in word '(:root :irregular :plur))

   ;; TODO: remove support for deprecated :root - use :irregular instead.
   (and
    (= (get-in word '(:agr :number)) :sing)
    (= (get-in word '(:cat) :noun))
    (string? (get-in word '(:root))))
   (get-in word '(:root))

   (and
    (= (get-in word '(:agr :number)) :sing)
    (= (get-in word '(:cat) :noun))
    (string? (get-in word '(:english))))
   (strip (str (get-in word '(:english)) " "
               (if (get-in word '(:note))
                 (get-in word '(:note))
                 "")))

   ;; TODO: remove support for deprecated :root - use :irregular instead.
   (and
    (= (get-in word '(:agr :number)) :sing)
    (= (get-in word '(:cat) :noun))
    (string? (get-in word '(:root :english))))
   (get-in word '(:root :english))


   (and
    (= (get-in word '(:agr :number)) :sing)
    (= (get-in word '(:cat) :noun))
    (string? (get-in word '(:english :english))))
   (str
    (strip (get-in word '(:english :english))) " "
    (if (get-in word '(:note))
      (strip (get-in word '(:note)))))

   ;; TODO: remove support for deprecated :root - use :irregular instead.
   (and (= (get-in word '(:agr :number)) :plur)
        (= (get-in word '(:cat)) :noun)
        (string? (get-in word '(:root))))
   (str (get-in word '(:root)) "s")

   (and (= (get-in word '(:agr :number)) :plur)
        (= (get-in word '(:cat)) :noun)
        (string? (get-in word '(:english))))
   (str (plural-en (get-in word '(:english)))
        (if (get-in word '(:note))
          (str (get-in word '(:note)))))

   (and (= (get-in word '(:agr :number)) :plur)
        (= (get-in word '(:cat)) :noun)
        (string? (get-in word '(:english :english))))
   (str (plural-en (get-in word '(:english :english)))
        (if (get-in word '(:english :note))
          (str (get-in word '(:english :note)))))


   (and (= (get-in word '(:cat)) :adjective)
        (string? (get-in word '(:english))))
   (get-in word '(:english))

   (string? (get-in word '(:english)))
   (get-in word '(:english))

   ;; TODO: not sure if this code is alive or not: is there ever
   ;; a case of a sign with '(:english :english :english)?
   (and (string? (get-in word '(:english :english)))
        (= (.size (keys word)) 1))
   (get-in word '(:english :english))

   (string? (get-in word '(:infinitive)))
   (get-in word '(:infinitive))

   :else
   word))

(defn get-english [a & [ b ] ]
  (let [a (if (nil? a) "" a)
        b (if (nil? b) "" b)
        re-a (get-english-1 a)
        re-b (get-english-1 b)]
    (log/debug (str "get-english a: " a " => " re-a))
    (log/debug (str "get-english b: " b " => " re-b))
    (log/debug (str "a is modal?: " (= true (get-in a '(:modal)))))
    (cond

     (and (string? re-a)
          (map? re-b)
          (not (nil? (get-in re-b '(:a))))
          (not (nil? (get-in re-b '(:b)))))
     {:a re-a
      :b re-b}

     (and (map? a)
          (map? re-a))
          {:a re-a
           :b re-b}

     (and (map? a)
          (= (get-in a '(:modal)) true)
          (string? re-b))
     (get-english-1 {:a re-a
                     :b (string/replace re-b #"^to " "")})

     (and (string? re-a)
          (string? re-b))
     (strip (str re-a " " re-b))

     ;; new-style n' -> adj noun
     (and
      (= (get-in a '(:cat)) :adjective)
      (= (get-in b '(:cat)) :noun)
      (= (get-in a '(:agr :number)) :top))
     {:a a
      :b b}

     ;; old-style n' -> adj noun
     ;; TODO: remove this.
     (and
      (= (get-in a '(:cat)) :noun)
      (= (get-in b '(:cat)) :adjective))
     ;; If a is a noun, and b is a adj, reverse a and b in string,
     ;;  so that italian word order is reversed to english word order.
     {:a b
      :b a}

     (and (string? re-a) (string? re-b)
          (= re-a "a")
          (re-find #"^[aeiou]" re-b))
     (str "an " re-b)

     (and (string? re-a) (string? re-b))
     (str re-a " " re-b)

     (and (string? re-a) (string? (get-in re-b '(:english))))
     (str re-a " " (get-in re-b '(:english)))

     :else
     {:a (if (nil? a) :top a)
      :b (if (nil? b) :top b)})))

(defn remove-to [english-verb-phrase]
  (let [english (get english-verb-phrase :english)]
    (let [regex #"^to[ ]+(.*)"]
      (let [string
            (string/replace english regex (fn [[_ rest]] (str rest)))]
        (merge
         {:remove-to string}
         english-verb-phrase)))))

;; TODO: not used; remove after getting useful stuff out.
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

(defn if-isco [verb]
  (if (= (get verb :isco) true)
    "isc"
    ""))

(defn plural-en [english]
  (if (re-find #"[t][y]$" english) ;; city => cities
    (string/replace english #"[y]$" "ies")
    (if (re-find #"[hsx]$" english) ;; brush => brushes
      (str english "es")
      ;; default case.
      (str english "s"))))

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

(defn stem-per-passato-prossimo [infinitive]
  "_infinitive_ should be a string (italian verb infinitive form)"
  (string/replace infinitive #"^(.*)([aei])(re)$" (fn [[_ prefix vowel suffix]] (str prefix))))

(defn passato-prossimo [infinitive]
  (str (stem-per-passato-prossimo infinitive) "ato"))

(defn capitalize [s]
  "Capitalize first char and leave the rest of the characters alone (compare with string/capitalize which lower-cases all chars after first."
  (if (nil? s) ""
      (let [s (.toString s)]
        (if (< (count s) 2)
          (.toUpperCase s)
          (str (.toUpperCase (subs s 0 1))
               (subs s 1))))))

(declare fo-ps-en)
(declare fo-ps-it)

(defn fo-ps [expr]
  "show the phrase-structure of a phrase structure tree, e.g [hh21 'mangiare (to eat)' [cc10 'il (the)' 'pane(bread)']]"
  ;; [:first = {:head,:comp}] will not yet be found in expr, so this head-first? will always be false.
  (let [head-first? (= :head (get-in expr [:first]))]
    (cond

     (and
      (or (set? expr)
          (seq? expr)
          (vector? expr))
      (empty? expr))
     (str "")


     (and
      (or (set? expr)
          (seq? expr)
          (vector? expr))
      (not (empty? expr)))

     ;; expr is a sequence of some kind. Assume each element is a phrase structure tree and show each.
     (map (fn [each]
            (fo-ps each))
          expr)


     (and (map? expr)
          (:italiano expr)
          (:english expr))
     (str "it:" (fo-ps-it (:italiano expr)) ";"
          "en:" (fo-ps-en (:english expr)))

     (and (map? expr)
          (:rule expr)
          (= (get-in expr '(:italian :a))
             (get-in expr '(:comp :italian))))
     ;; complement first
     (str "[" (:rule expr) " "
          (fo-ps (get-in expr '(:comp)))
          " "
          (fo-ps (get-in expr '(:head)))
          "]")

     (and (map? expr)
          (:rule expr))
     ;; head first ('else' case of above.)
     (str "[" (:rule expr) " "
          (fo-ps (get-in expr '(:head)))
          " "
          (fo-ps (get-in expr '(:comp)))
          "]")


     (and (map? expr)
          (:comment expr)
          (= (get-in expr '(:italian :a))
             (get-in expr '(:comp :italian))))
     ;; complement first
     (str "[" (:comment expr) " "
          (fo-ps (get-in expr '(:comp)))
          " "
          (fo-ps (get-in expr '(:head)))
          "]")

     (and (map? expr)
          (:comment expr))
     ;; head first ('else' case of above.)
     (str "[" (:comment expr) " "
          (fo-ps (get-in expr '(:head)))
          " "
          (fo-ps (get-in expr '(:comp)))
          "]")

     (and
      (map? expr)
      (or (:italian expr)
          (:english expr)))
     (str (get-italian-1 (get-in expr '(:italian)))
          " ("
          (get-english-1 (get-in expr '(:english)))
          ")")

     true
     expr)))

(defn fo-ps-en [expr]
  "show the phrase-structure of a phrase structure tree, e.g [hh21 'mangiare (to eat)' [cc10 'il (the)' 'pane(bread)']]"
  ;; [:first = {:head,:comp}] will not yet be found in expr, so this head-first? will always be false.
  (let [head-first? (= :head (get-in expr [:first]))]
    (cond

     (and
      (or (set? expr)
          (seq? expr)
          (vector? expr))
      (empty? expr))
     (str "")

     (and
      (or (set? expr)
          (seq? expr)
          (vector? expr))
      (not (empty? expr)))

     ;; expr is a sequence of some kind. Assume each element is a phrase structure tree and show each.
     (map (fn [each]
            (fo-ps-en each))
          expr)


     (and (map? expr)
          (:italiano expr)
          (:english expr))
     (str "it:" (fo-ps-en (:italiano expr)) ";"
          "en:" (fo-ps-en (:english expr)))

     (and (map? expr)
          (:rule expr)
          (= (get-in expr '(:english :a))
             (get-in expr '(:comp :english))))
     ;; complement first
     (str "[" (:rule expr) " "
          (fo-ps-en (get-in expr '(:comp)))
          " "
          (fo-ps-en (get-in expr '(:head)))
          "]")

     (and (map? expr)
          (:rule expr))
     ;; head first ('else' case of above.)
     (str "[" (:rule expr) " "
          (fo-ps-en (get-in expr '(:head)))
          " "
          (fo-ps-en (get-in expr '(:comp)))
          "]")


     (and (map? expr)
          (:comment expr)
          (= (get-in expr '(:english :a))
             (get-in expr '(:comp :english))))
     ;; complement first
     (str "[" (:comment expr) " "
          (fo-ps-en (get-in expr '(:comp)))
          " "
          (fo-ps-en (get-in expr '(:head)))
          "]")

     (and (map? expr)
          (:comment expr))
     ;; head first ('else' case of above.)
     (str "[" (:comment expr) " "
          (fo-ps-en (get-in expr '(:head)))
          " "
          (fo-ps-en (get-in expr '(:comp)))
          "]")

     (and
      (map? expr)
      (:english expr))
     (str (get-english-1 (get-in expr '(:english))))

     true
     expr)))

;; TODO: fo-ps-it and fo-ps-en duplicate lots of stuff in (fo-ps).
(defn fo-ps-it [expr]
  "show the phrase-structure of a phrase structure tree, e.g [hh21 'mangiare (to eat)' [cc10 'il (the)' 'pane(bread)']]"
  ;; [:first = {:head,:comp}] will not yet be found in expr, so this head-first? will always be false.
  (let [head-first? (= :head (get-in expr [:first]))]
    (cond

     (and
      (or (set? expr)
          (seq? expr)
          (vector? expr))
      (empty? expr))
     (str "")


     (and
      (or (set? expr)
          (seq? expr)
          (vector? expr))
      (not (empty? expr)))

     ;; expr is a sequence of some kind. Assume each element is a phrase structure tree and show each.
     (map (fn [each]
            (fo-ps-it each))
          expr)


     (and (map? expr)
          (:italiano expr)
          (:english expr))
     (str "it:" (fo-ps-it (:italiano expr)) ";"
          "en:" (fo-ps-en (:english expr)))

     (and (map? expr)
          (:rule expr)
          (= (get-in expr '(:italian :a))
             (get-in expr '(:comp :italian))))
     ;; complement first
     (str "[" (:rule expr) " "
          (fo-ps-it (get-in expr '(:comp)))
          " "
          (fo-ps-it (get-in expr '(:head)))
          "]")

     (and (map? expr)
          (:rule expr))
     ;; head first ('else' case of above.)
     (str "[" (:rule expr) " "
          (fo-ps-it (get-in expr '(:head)))
          " "
          (fo-ps-it (get-in expr '(:comp)))
          "]")


     (and (map? expr)
          (:comment expr)
          (= (get-in expr '(:italian :a))
             (get-in expr '(:comp :italian))))
     ;; complement first
     (str "[" (:comment expr) " "
          (fo-ps-it (get-in expr '(:comp)))
          " "
          (fo-ps-it (get-in expr '(:head)))
          "]")

     (and (map? expr)
          (:comment expr))
     ;; head first ('else' case of above.)
     (str "[" (:comment expr) " "
          (fo-ps-it (get-in expr '(:head)))
          " "
          (fo-ps-it (get-in expr '(:comp)))
          "]")

     (and
      (map? expr)
      (:italian expr))
     (str (get-italian-1 (get-in expr '(:italian))))

     true
     expr)))

(defn formattare-1 [expr]
  (log/trace (str "doing f-1 on: " expr))
  (cond
   (= :fail expr)
   expr

   :else
   (let [failed-warning (if (fail? expr) (str "FAILED: " expr " ") "")
         italian
         (capitalize
          (get-italian-1 (get-in expr '(:italian))))
         english
         (capitalize
          (get-english-1 (get-in expr '(:english))))]
     (string/trim
      (str failed-warning italian " (" english ").")))))

;;; e.g.:
;;; (formattare (over (over s-present (over (over np lexicon) (lookup {:synsem {:human true}}))) (over (over vp-present lexicon) (over (over np lexicon) lexicon))))
;; TODO: fix english formatting in:
;;italianverbs.benchmark> (fo (take 1 (forest/hlcl cache grammar)))
;; (("Stato Roma ({:a {:infl #<Ref@1f574a31: :past>, :case #<Ref@773c3b34: :nom>, :agr #<Ref@3619649b: {:gender #<Ref@7f628bdb: :masc>, :number #<Ref@1ecc1833: :sing>}>, :irregular {:past {:2sing \"were\", :1sing \"was\", :participle \"been\", :3sing \"was\", :3plur \"were\", :2plur \"were\", :1plur \"were\"}, :present {:2sing \"are\", :1sing \"am\", :3sing \"is\", :3plur \"are\", :2plur \"are\", :1plur \"are\"}}, :infinitive \"to be\"}, :b \"Rome\"})."))

(declare fo)
(declare finalize)
;; TODO:more clojure-ish way of handling argument types:
;; if it's a map and its keys are such-and-such, then..
(defn formattare [expressions]
  "format a bunch of expressions (feature-structures) showing just the italian (and english in parentheses)."
  (cond

   (and (map? expressions)
        (= (.size (keys expressions)) 2)
        (= (set (keys expressions))
           #{:italiano :english}))
   (str "en:" (fo (:italiano expressions))
        " it:" (fo (:english expressions)))

   (keyword? expressions)
   expressions
   (symbol? expressions)
   expressions
   (fn? expressions)
   expressions
   true
    (if (and (= (type expressions) clojure.lang.LazySeq)
             (not (empty? expressions)))
      (cons
       (formattare (first expressions))
       (lazy-seq
        (formattare (rest expressions))))
      (if (map? expressions)
        ;; wrap this single expression in a list and re-call.
        (list (formattare-1 expressions))
        (cond (nil? expressions) nil
              (fail? expressions)
              ":fail"
              (empty? expressions) nil
              true
            (lazy-seq
             (cons
              (formattare-1 (first expressions))
              (formattare (rest expressions)))))))))

;; useful abbreviation:
(defn fo [expressions]
  (formattare expressions))

(defn fof [expressions]
  (let [retval (lazy-seq (list (formattare expressions)))
        total-time (time (first retval))]
    (log/debug (str "tense info: " (get-in expressions '(:comp :synsem :sem :tense))))
    (log/debug (str "FINISHED FORMATTING: " (first retval))) ;; acts as a delimiter for this sentence's generation for debugging purposes.
    retval))

(defn finalize [expr]
  (cond

   ;; This is the case where we've generated totally separate english and italian phrase structure trees,
   ;; in which case we need to extract the english and italian strings from their respective trees.
   (and (map? expr)
        (= (.size (keys expr)) 2)
        (= (set (keys expr))
           #{:italiano :english}))
   (let [english
         (get-english-1 (get-in expr '(:english :english)))
         italian
         ;; TODO: much inconsistent. make the path: [:italiano :italiano]  rather than [:italiano :italian]
         (get-italian-1 (get-in expr '(:italiano :italian)))]
     (log/debug (str "input expr: " (fo expr)))
     (log/debug (str "finalized english: " english))
     (log/debug (str "finalized italian: " italian))
     {:italian italian
      :english english
      :english-tree (get-in expr [:english])
      :italian-tree (get-in expr [:italiano])})

   (if (= :fail expr)
     :fail)

   (map? expr)
   (let [english
         (get-english-1 (get-in expr '(:english)))
         italian
         (get-italian-1 (get-in expr '(:italian)))]
     (log/debug (str "input expr: " (fo expr)))
     (log/debug (str "finalized english: " english))
     (log/debug (str "finalized italian: " italian))
     (merge expr
            {:italian italian
             :english english}))

   (seq? expr)
   (map (fn [x]
          (finalize x))
        expr)

    (if (= :fail expr)
      :fail
      (let [english
            (get-english-1 (get-in expr '(:english)))
        italian
            (get-italian-1 (get-in expr '(:italian)))]
        (log/debug (str "input expr: " (fo expr)))
        (log/debug (str "finalized english: " english))
        (log/debug (str "finalized italian: " italian))
        (merge expr
               {:italian italian
                :english english})))))

(defn normalize-whitespace [string]
  (string/trim
   (string/replace string #"[ ]+" " ")))
