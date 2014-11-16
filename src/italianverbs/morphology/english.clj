(ns italianverbs.morphology.english
  (:refer-clojure :exclude [get-in merge resolve replace reverse]))

(require '[clojure.core :as core])
(require '[clojure.string :refer :all])
(require '[clojure.tools.logging :as log])
(require '[italianverbs.unify :refer :all])

(declare get-string)
(declare plural-en)

(defn get-string-1 [word]
  (log/debug (str "get-string-1: " word))
  (cond
   (ref? word)
   (get-string-1 @word)

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
        (string? (get-in word '(:a :past))))
   (str (get-in word '(:a :past)) " "
        (get-string-1 (get-in word '(:b))))

   ;; :note is used for little annotations that are significant in italian but not in english
   ;; e.g. gender signs (♂,♀) on nouns like "professore" and "professoressa".
   (and (string? (get-in word '(:english)))
        (string? (get-in word '(:note))))
   (str (trim (get-string-1 (dissoc word :note))) " " (trim (get-in word '(:note))))

   (= (get-in word '(:a)) :top)
   (str
    ".." " " (get-string-1 (get-in word '(:b))))

   ;; show elipsis (..) if :b is not specified.
   (and
    (= (get-in word '(:b)) :top)
    (string? (get-string-1 (get-in word '(:a)))))
   (str
    (get-string-1 (get-in word '(:a)))
    " " "..")

   ;; show elipsis (..) if :a is not specified.
   (and
    (= (get-in word '(:b)) :top)
    (string? (get-in word '(:a :english))))
   (str
    (get-string-1 (get-in word '(:a :english)))
    " " "..")

   (string? word)
   (trim word)

   ;; (could have) + (to go) => "could have gone"
   (and
    (get-in word '(:a))
    (get-in word '(:b))
    (string? (get-in word '(:a :past)))
    (= (get-in word '(:a :past)) "could have")
    (string? (get-in word '(:b :past-participle)))
    (= (get-in word '(:a :infl)) :past))
   (join " " (list (get-in word '(:a :past))
                          (get-in word '(:b :past-participle))))

   ;; (could have) + (to sleep) => "could have slept"
   (and
    (get-in word '(:a))
    (get-in word '(:b))
    (string? (get-in word '(:a :past)))
    (= (get-in word '(:a :past)) "could have")
    (string? (get-in word '(:b :past)))
    (= (get-in word '(:a :infl)) :past))
   (join " " (list (get-in word '(:a :past))
                          (get-in word '(:b :past))))

   ;; (could have) + (do X) => "could have done X"
   (and
    (get-in word '(:a))
    (get-in word '(:b))
    (string? (get-in word '(:a :past)))
    (= (get-in word '(:a :past)) "could have")
    (string? (get-in word '(:b :a :past-participle)))
    (= (get-in word '(:a :infl)) :past))
   ;; recursive call after inflecting '(:b :a) to past.
   (get-string {:a (get-in word '(:a))
                 :b {:a (get-in word '(:b :a :past-participle))
                     :b (get-in word '(:b :b))}})

   ;; (could have) + (make X) => "could have made X"
   (and
    (get-in word '(:a))
    (get-in word '(:b))
    (string? (get-in word '(:a :irregular :past)))
    (= (get-in word '(:a :past)) "could have")
    (string? (get-in word '(:b :a :past)))
    (= (get-in word '(:a :infl)) :past))
   ;; recursive call after inflecting '(:b :a) to past.
   (get-string {:a (get-in word '(:a))
                 :b {:a (get-in word '(:b :a :past))
                     :b (get-in word '(:b :b))}})

   (and
    (get-in word '(:a))
    (get-in word '(:b))
    (string? (get-in word '(:a)))
    (string? (get-in word '(:b))))
   (join " "
         (list (get-in word '(:a))
               (get-in word '(:b))))

   (and
    (get-in word '(:a))
    (get-in word '(:b)))
   (get-string (get-in word '(:a))
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
   (get-string-1 (get-in word '(:b)))

   (= true (get-in word '(:b :hidden)))
   (get-string-1 (get-in word '(:a)))

   (and (= (get-in word '(:infl)) :conditional)
        (get-in word '(:infinitive))
        (not (nil? (get-in word '(:agr :number))))
        (not (nil? (get-in word '(:agr :person)))))
   (let [infinitive (get-in word '(:infinitive))
         stem (replace infinitive #"^to " "")]
     (str "would " stem))

   (and (= (get-in word '(:infl)) :futuro)
        (get-in word '(:infinitive))
        (not (nil? (get-in word '(:agr :number))))
        (not (nil? (get-in word '(:agr :person)))))
   (let [infinitive (get-in word '(:infinitive))
         stem (replace infinitive #"^to " "")]
     (str "will " stem))

   (and (= (get-in word '(:infl)) :imperfetto)
        (= :plur (get-in word '(:agr :number)))
        (get-in word '(:imperfect :plur)))
   (str (get-in word '(:imperfect :plur)))

   (and (= (get-in word '(:infl)) :imperfetto)
        (= :sing (get-in word '(:agr :number)))
        (= :2nd (get-in word '(:agr :person)))
        (get-in word '(:imperfect :2sing)))
   (str (get-in word '(:imperfect :2sing)))

   ;; if neither of the two above does not match:
   (and (= (get-in word '(:infl)) :imperfetto)
        (map? (get-in word '(:imperfect))))
   (str (get-in word '(:imperfect :default)))

   (and (= (get-in word '(:infl)) :imperfetto)
        (get-in word '(:imperfect)))
   (str (get-in word '(:imperfect)))


   (and (= (get-in word '(:infl)) :imperfetto)
        (get-in word '(:infinitive)))
   (let [infinitive (get-in word '(:infinitive))
         stem (replace infinitive #"^to " "")
         to-final (re-find #" to$" stem) ;; occurs in e.g. "have to": in imperfect becomes "was having to"
         stem (replace stem #" to$" "")
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
        (string? (get-in word '(:imperfetto)))
        (get-in word '(:imperfetto))

        (and (= :sing (get-in word '(:agr :number)))
             (or (= :1st (get-in word '(:agr :person)))
                 (= :3rd (get-in word '(:agr :person))))
             (string? (get-in word '(:imperfetto-suffix))))
        (str "was " (get-in word '(:imperfetto-suffix)))

        (string? (get-in word '(:imperfetto-suffix)))
        (str "were " (get-in word '(:imperfetto-suffix)))

        (and (= :sing (get-in word '(:agr :number)))
             (or (= :1st (get-in word '(:agr :person)))
                 (= :3rd (get-in word '(:agr :person)))))
        (str "was " stem "ing" (if to-final to-final ""))

        true
        (str "were " stem "ing" (if to-final to-final "")))))

   ;; irregular past (1): a single inflection for all persons/numbers.
   (and (= :past (get-in word '(:infl)))
        (string? (get-in word '(:past))))
   (get-in word '(:irregular :past))

   (and (= :past (get-in word '(:infl)))
        (= :top (get-in word '(:agr :number)))
        (string? (get-in word '(:past :2sing))))
   ;; use the 2nd singular form if there's not enough inflection info to go on.
   (str "[" (get-in word '(:past :2sing)) "]")

   (= :top (get-in word '(:infl)))
   (get-in word '(:infinitive))

   ;; irregular past (2): a different inflection for each persons/numbers.
   (and (= :past (get-in word '(:infl)))
        (map? (get-in word '(:past))))
   (let [number (get-in word '(:agr :number))
         person (get-in word '(:agr :person))]
     (cond (and (= person :1st) (= number :sing))
           (get-in word '(:past :1sing))
           (and (= person :2nd) (= number :sing))
           (get-in word '(:past :2sing))
           (and (= person :3rd) (= number :sing))
           (get-in word '(:past :3sing))
           (and (= person :1st) (= number :plur))
           (get-in word '(:past :1plur))
           (and (= person :2nd) (= number :plur))
           (get-in word '(:past :2plur))
           (and (= person :3rd) (= number :plur))
           (get-in word '(:past :3plur))
           true word)) ;; not enough agreement specified to conjugate.

   ;; regular past
   (and (= :past (get-in word '(:infl)))
        (string? (get-in word '(:infinitive))))
   (let [infinitive (get-in word '(:infinitive))
         stem (replace infinitive #"^to " "")
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
         stem (replace root #"^to " "")
         last-stem-char-is-e (re-find #"e$" stem)
         last-stem-char-is-vowel (re-find #"[aeiou]$" stem)]
     (log/debug "+else")
     (log/debug (str "(english):word: " word))
     (cond

      (and (= person :1st) (= number :sing)
           (string? (get-in word '(:present :1sing))))
      (get-in word '(:present :1sing))
      (and (= person :2nd) (= number :sing)
           (string? (get-in word '(:present :2sing))))
      (get-in word '(:present :2sing))
      (and (= person :3rd) (= number :sing)
           (string? (get-in word '(:present :3sing))))
      (get-in word '(:present :3sing))

      (and (= person :1st) (= number :plur)
           (string? (get-in word '(:present :1plur))))
      (get-in word '(:present :1plur))
      (and (= person :2nd) (= number :plur)
           (string? (get-in word '(:present :2plur))))
      (get-in word '(:present :2plur))
      (and (= person :3rd) (= number :plur)
           (string? (get-in word '(:present :3plur))))
      (get-in word '(:present :3plur))

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
    (get-in word '(:plur))
    (= (get-in word '(:agr :number)) :plur)
    (= (get-in word '(:cat) :noun)))
   (get-in word '(:plur))


   ;; TODO: remove support for deprecated :root - use :irregular instead (as immediately above).
   (and
    (get-in word '(:root :plur))
    (= (get-in word '(:agr :number)) :plur)
    (= (get-in word '(:cat) :noun)))
   (get-in word '(:root :plur))

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
   (trim (str (get-in word '(:english)) " "
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
    (trim (get-in word '(:english :english))) " "
    (if (get-in word '(:note))
      (trim (get-in word '(:note)))))

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

(defn get-string [a & [ b ] ]
  (let [a (if (nil? a) "" a)
        b (if (nil? b) "" b)
        re-a (get-string-1 a)
        re-b (get-string-1 b)]
    (log/debug (str "get-string a: " a " => " re-a))
    (log/debug (str "get-string b: " b " => " re-b))
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
     (get-string-1 {:a re-a
                     :b (replace re-b #"^to " "")})

     (and (string? re-a)
          (string? re-b))
     (trim (str re-a " " re-b))

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
            (replace english regex (fn [[_ rest]] (str rest)))]
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
    (replace english #"[y]$" "ies")
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


(declare fo-ps-en)

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
     (str (get-string-1 (get-in expr '(:english))))

     true
     expr)))

