(ns italianverbs.morphology
  (:require
   ;; TODO: "fs/" is historical; use "unify/" instead.
   [italianverbs.unify :as fs]
   [clojure.tools.logging :as log]
   [clojure.string :as string]))

(defn strip [str]
  "remove heading and trailing whitespace"
  (string/replace (string/replace str #"^\s+" "") #"\s+$" ""))

(defn phrase-is-finished? [phrase]
  (cond
   (string? phrase) true
   (map? phrase)
   (or (phrase-is-finished? (fs/get-in phrase '(:italian)))
       (string? (fs/get-in phrase '(:infinitive)))
       (and (phrase-is-finished? (fs/get-in phrase '(:a)))
            (phrase-is-finished? (fs/get-in phrase '(:b)))))
   :else false))

(defn suffix-of [word]
  "compute the final character given a lexical entry and agreement info in :agr."
  (let [suffix (cond

                (and (= (fs/get-in word '(:agr :gender)) :fem)
                     (= (fs/get-in word '(:agr :number)) :sing)
                     (= (fs/get-in word '(:essere)) true))
                "a"

                (and (= (fs/get-in word '(:agr :gender)) :fem)
                     (= (fs/get-in word '(:agr :number)) :plur)
                     (= (fs/get-in word '(:essere)) true))
                "e"

                (and (= (fs/get-in word '(:agr :number)) :plur)
                     (= (fs/get-in word '(:essere)) true))
                "i"

                true
                "o"

                )]
    suffix))

(declare get-italian)

(defn stem-per-futuro [infinitive]
  "_infinitive_ should be a string (italian verb infinitive form)"
  (cond
   (re-find #"iare$" infinitive)
   (string/replace infinitive #"iare$" "er")
   (re-find #"are$" infinitive)
   (string/replace infinitive #"are$" "er")
   (re-find #"ere$" infinitive)
   (string/replace infinitive #"ere$" "er")
   (re-find #"ire$" infinitive)
   (string/replace infinitive #"ire$" "ir")

   true
   infinitive))

(defn stem-per-imperfetto [infinitive]
  "_infinitive_ should be a string (italian verb infinitive form)"
  (cond
   (re-find #"re$" infinitive)
   (string/replace infinitive #"re$" "")
   true
   infinitive))

(defn analyze-italian-1 [word]
  (let [person (fs/get-in word '(:agr :person))
        number (fs/get-in word '(:agr :number))]
    {:person person
     :number number
   :infinitive?    (and (= :infinitive (fs/get-in word '(:infl)))
                        (string? (fs/get-in word '(:infinitive))))

   :irregular-futuro?    (and
                          (= (fs/get-in word '(:infl)) :futuro)
                          (map? (fs/get-in word '(:irregular :futuro))))

   :regular-futuro?    (and (= (fs/get-in word '(:infl)) :futuro)
                            (fs/get-in word '(:infinitive)))

   :regular-imperfetto?    (and (= (fs/get-in word '(:infl)) :imperfetto)
                                (fs/get-in word '(:infinitive)))


   :irregular-past?    (and
                        (= :past (fs/get-in word '(:infl)))
                        (string? (fs/get-in word '(:irregular :past))))

   ;;nei: not enough information to conjugate.
   :past-irregular-essere-type-nei
   (and (= :past (fs/get-in word '(:infl)))
        (fs/get-in word '(:irregular :passato))
        (fs/get-in word '(:essere) true)
        (or (= :notfound (fs/get-in word '(:agr :number) :notfound))
            (= :top (fs/get-in word '(:agr :number)))))


   ;;nei: not enough information to conjugate.
   :past-esseri-but-nei?
   (and (= :past (fs/get-in word '(:infl)))
        (fs/get-in word '(:essere) true)
        (or (= :notfound (fs/get-in word '(:agr :number) :notfound))
            (= :top (fs/get-in word '(:agr :number)))))
   :irregular-passato?
   (and (= :past (fs/get-in word '(:infl)))
        (fs/get-in word '(:irregular :passato)))

   :regular-passato
   (= :past (fs/get-in word '(:infl)))

   :present
     (= (fs/get-in word '(:infl)) :present)


}))

(defn get-italian-1 [word]
  (let [analysis (analyze-italian-1 word)
        person (fs/get-in word '(:agr :person))
        number (fs/get-in word '(:agr :number))]

    (cond

     ;; handle lexical exceptions (plural feminine adjectives):
     (and
      (= (fs/get-in word '(:agr :number)) :plur)
      (= (fs/get-in word '(:agr :gender)) :fem)
      (= (fs/get-in word '(:cat)) :adjective)
      (string? (fs/get-in word '(:irregular :fem :plur))))
     (fs/get-in word '(:irregular :fem :plur))

     ;; handle lexical exceptions (plural masculine adjectives):
     (and
      (= (fs/get-in word '(:agr :number)) :plur)
      (= (fs/get-in word '(:agr :gender)) :masc)
      (= (fs/get-in word '(:cat)) :adjective)
      (string? (fs/get-in word '(:irregular :masc :plur))))
     (fs/get-in word '(:irregular :masc :plur))

     (and
      (or (= (fs/get-in word '(:agr :gender)) :masc)
          (= (fs/get-in word '(:agr :gender)) :top))
      (= (fs/get-in word '(:agr :number)) :plur)
      (= (fs/get-in word '(:cat)) :adjective))
     (string/replace (fs/get-in word '(:italian))
                     #"[eo]$" "i") ;; nero => neri

     (and
      (= (fs/get-in word '(:agr :gender)) :fem)
      (= (fs/get-in word '(:agr :number)) :plur)
      (= (fs/get-in word '(:cat)) :adjective))
     (string/replace (fs/get-in word '(:italian))
                     #"[eo]$" "e") ;; nero => nere

     ;; handle lexical exceptions (plural nouns):
     (and
      (= (fs/get-in word '(:agr :number)) :plur)
      (= (fs/get-in word '(:cat)) :noun)
      (string? (fs/get-in word '(:irregular :plur))))
     (fs/get-in word '(:irregular :plur))

     ;; regular masculine nouns
     (and
      (= (fs/get-in word '(:agr :gender)) :masc)
      (= (fs/get-in word '(:agr :number)) :plur)
      (= (fs/get-in word '(:cat) :noun))
      (fs/get-in word '(:italian)))
     (string/replace (fs/get-in word '(:italian))
                     #"[eo]$" "i") ;; dottore => dottori; medico => medici

     ;; regular feminine nouns
     (and
      (= (fs/get-in word '(:agr :gender)) :fem)
      (= (fs/get-in word '(:agr :number)) :plur)
      (= (fs/get-in word '(:cat) :noun))
      (fs/get-in word '(:italian)))
     (string/replace (fs/get-in word '(:italian))
                     #"[a]$" "e") ;; donna => donne

     ;; TODO: move this down to other adjectives.
     ;; this was moved up here to avoid
     ;; another rule from matching it.
     (and
      (= (fs/get-in word '(:agr :gender)) :fem)
      (= (fs/get-in word '(:agr :number)) :plur)
      (= (fs/get-in word '(:cat)) :adjective))
     (string/replace (fs/get-in word '(:italian))
                     #"[eo]$" "e") ;; nero => nere

     (and
      (= (fs/get-in word '(:agr :gender)) :fem)
      (= (fs/get-in word '(:agr :number)) :sing)
      (= (fs/get-in word '(:cat)) :adjective))
     (string/replace (fs/get-in word '(:italian))
                     #"[eo]$" "a") ;; nero => nera

     (and
      (string? (fs/get-in word '(:italian)))
      (= :top (fs/get-in word '(:agr :sing) :top)))
     (str (fs/get-in word '(:italian)))

     (= (fs/get-in word '(:a)) :top)
     (str
      ".." " " (get-italian-1 (fs/get-in word '(:b))))

     (and
      (= (fs/get-in word '(:b)) :top)
      (string? (get-italian-1 (fs/get-in word '(:a)))))
     (str
      (get-italian-1 (fs/get-in word '(:a)))
      " " "..")


     (and
      (= (fs/get-in word '(:b)) :top)
      (string? (fs/get-in word '(:a :italian))))
     (str
      (get-italian-1 (fs/get-in word '(:a :italian)))
      " " "..")

     (and (= :infinitive (fs/get-in word '(:infl)))
          (string? (fs/get-in word '(:infinitive))))
     (fs/get-in word '(:infinitive))

     (and
      (= (fs/get-in word '(:infl)) :futuro)
      (map? (fs/get-in word '(:irregular :futuro))))
     (let [infinitive (fs/get-in word '(:infinitive))
           person (fs/get-in word '(:agr :person))
           number (fs/get-in word '(:agr :number))]
       (cond
        (and (= person :1st) (= number :sing))
        (fs/get-in word '(:irregular :futuro :1sing))
        (and (= person :2nd) (= number :sing))
        (fs/get-in word '(:irregular :futuro :2sing))
        (and (= person :3rd) (= number :sing))
        (fs/get-in word '(:irregular :futuro :3sing))
        (and (= person :1st) (= number :plur))
        (fs/get-in word '(:irregular :futuro :1plur))
        (and (= person :2nd) (= number :plur))
        (fs/get-in word '(:irregular :futuro :2plur))
        (and (= person :3rd) (= number :plur))
        (fs/get-in word '(:irregular :futuro :3plur))
        true
        word))

     ;; regular futuro tense
     (and (= (fs/get-in word '(:infl)) :futuro)
          (fs/get-in word '(:infinitive)))
     (let [infinitive (fs/get-in word '(:infinitive))
           person (fs/get-in word '(:agr :person))
           number (fs/get-in word '(:agr :number))
           stem (stem-per-futuro infinitive)]
       (cond
        (and (= person :1st) (= number :sing))
        (str stem "ò")

        (and (= person :2nd) (= number :sing))
        (str stem "ai")

        (and (= person :3rd) (= number :sing))
        (str stem "à")

        (and (= person :1st) (= number :plur))
        (str stem "emo")

        (and (= person :2nd) (= number :plur))
        (str stem "ete")

        (and (= person :3rd) (= number :plur))
        (str stem "anno")

        :else
        word))

     ;; irregular imperfetto sense:
     ;; 1) use irregular based on number and person.
     (and
      (= (fs/get-in word '(:infl)) :imperfetto)
      (= :sing (fs/get-in word '(:agr :number)))
      (= :1st (fs/get-in word '(:agr :person)))
      (string? (fs/get-in word '(:irregular :imperfetto :1sing))))
     (fs/get-in word '(:irregular :imperfetto :1sing))

     (and
      (= (fs/get-in word '(:infl)) :imperfetto)
      (= :sing (fs/get-in word '(:agr :number)))
      (= :2nd (fs/get-in word '(:agr :person)))
      (string? (fs/get-in word '(:irregular :imperfetto :2sing))))
     (fs/get-in word '(:irregular :imperfetto :2sing))

     (and
      (= (fs/get-in word '(:infl)) :imperfetto)
      (= :sing (fs/get-in word '(:agr :number)))
      (= :3rd (fs/get-in word '(:agr :person)))
      (string? (fs/get-in word '(:irregular :imperfetto :3sing))))
     (fs/get-in word '(:irregular :imperfetto :3sing))

     (and
      (= (fs/get-in word '(:infl)) :imperfetto)
      (= :plur (fs/get-in word '(:agr :number)))
      (= :1st (fs/get-in word '(:agr :person)))
      (string? (fs/get-in word '(:irregular :imperfetto :1plur))))
     (fs/get-in word '(:irregular :imperfetto :1plur))
     (and
      (= (fs/get-in word '(:infl)) :imperfetto)
      (= :plur (fs/get-in word '(:agr :number)))
      (= :2nd (fs/get-in word '(:agr :person)))
      (string? (fs/get-in word '(:irregular :imperfetto :2plur))))
     (fs/get-in word '(:irregular :imperfetto :2plur))
     (and
      (= (fs/get-in word '(:infl)) :imperfetto)
      (= :plur (fs/get-in word '(:agr :number)))
      (= :3rd (fs/get-in word '(:agr :person)))
      (string? (fs/get-in word '(:irregular :imperfetto :3plur))))
     (fs/get-in word '(:irregular :imperfetto :3plur))

     ;; regular imperfetto sense
     (and (= (fs/get-in word '(:infl)) :imperfetto)
          (fs/get-in word '(:infinitive)))
     (let [infinitive (fs/get-in word '(:infinitive))
           person (fs/get-in word '(:agr :person))
           number (fs/get-in word '(:agr :number))
           stem (stem-per-imperfetto infinitive)]
       (cond
        (and (= person :1st) (= number :sing))
        (str stem "vo")

        (and (= person :2nd) (= number :sing))
        (str stem "vi")

        (and (= person :3rd) (= number :sing))
        (str stem "va")

        (and (= person :1st) (= number :plur))
        (str stem "vamo")

        (and (= person :2nd) (= number :plur))
        (str stem "vate")

        (and (= person :3rd) (= number :plur))
        (str stem "vano")

        (string? infinitive)
        (str infinitive )

        :else
        (merge word
               {:error 1})))

     ;; TODO: remove this: :past is only for english (get-english-1), not italian.
     ;; italian uses :passato.
     (and
      (= :past (fs/get-in word '(:infl)))
      (string? (fs/get-in word '(:irregular :past))))
     (fs/get-in word '(:irregular :past))

     ;; TODO: do not use brackets: if there's an error about there being
     ;; not enough information, throw an exception explicitly.
     ;; return the irregular form in square brackets, indicating that there's
     ;; not enough information to conjugate the verb.
     (and (= :past (fs/get-in word '(:infl)))
          (fs/get-in word '(:irregular :passato))
          (fs/get-in word '(:essere) true)
          (or (= :notfound (fs/get-in word '(:agr :number) :notfound))
              (= :top (fs/get-in word '(:agr :number)))))
     (str "[" (fs/get-in word '(:irregular :passato)) "]")

     ;; TODO: do not use brackets: if there's an error about there being
     ;; regular passato prossimo and essere-verb => NEI (not enough information): defer conjugation and keep as a map.
     (and (= :past (fs/get-in word '(:infl)))
          (= (fs/get-in word '(:essere)) true)
          (or (= :notfound (fs/get-in word '(:agr :number) :notfound))
              (= :top (fs/get-in word '(:agr :number)))))
     (str "[" (fs/get-in word '(:infinitive)) " (past)]")

     ;; conjugate irregular passato
     (and (= :past (fs/get-in word '(:infl)))
          (fs/get-in word '(:irregular :passato)))
     (let [irregular-passato (fs/get-in word '(:irregular :passato))
           butlast (nth (re-find #"(.*).$" irregular-passato) 1)]
       (str butlast (suffix-of word)))

     ;; conjugate regular passato
     (and (= :past (fs/get-in word '(:infl)))
          (string? (fs/get-in word '(:infinitive))))
     (let [infinitive (fs/get-in word '(:infinitive))
           are-type (try (re-find #"are$" infinitive)
                         (catch Exception e
                           (throw (Exception. (str "Can't regex-find on non-string: " infinitive)))))
           ere-type (re-find #"ere$" infinitive)
           ire-type (re-find #"ire$" infinitive)
           stem (string/replace infinitive #"[iae]re$" "")
           last-stem-char-is-i (re-find #"i$" stem)

           ;; for passato prossimo, the last char depends on gender and number, if an essere-verb.
           suffix (suffix-of word)

           ]

       (cond

        (or are-type ere-type)
        (str stem "at" suffix) ;; "ato" or "ati"

        (or are-type ire-type)
        (str stem "it" suffix) ;; "ito" or "iti"

        true
        (str "(regpast:TODO):" stem)))

     (and (= (fs/get-in word '(:infl)) :present)
          (= person :1st) (= number :sing)
          (string? (fs/get-in word '(:irregular :present :1sing))))
     (fs/get-in word '(:irregular :present :1sing))

     (and (= (fs/get-in word '(:infl)) :present)
          (= person :2nd) (= number :sing)
          (string? (fs/get-in word '(:irregular :present :2sing))))
     (fs/get-in word '(:irregular :present :2sing))

     (and (= (fs/get-in word '(:infl)) :present)
          (= person :3rd) (= number :sing)
          (string? (fs/get-in word '(:irregular :present :3sing))))
     (fs/get-in word '(:irregular :present :3sing))

     (and (= (fs/get-in word '(:infl)) :present)
          (= person :1st) (= number :plur)
          (string? (fs/get-in word '(:irregular :present :1plur))))
     (fs/get-in word '(:irregular :present :1plur))

     (and (= (fs/get-in word '(:infl)) :present)
          (= person :2nd) (= number :plur)
          (string? (fs/get-in word '(:irregular :present :2plur))))
     (fs/get-in word '(:irregular :present :2plur))

     (and (= (fs/get-in word '(:infl)) :present)
          (= person :3rd) (= number :plur)
          (string? (fs/get-in word '(:irregular :present :3plur))))
     (fs/get-in word '(:irregular :present :3plur))

     (and
      (= (fs/get-in word '(:infl)) :present)
      (string? (fs/get-in word '(:infinitive))))
     (let [infinitive (fs/get-in word '(:infinitive))
           are-type (try (re-find #"are$" infinitive)
                         (catch Exception e
                           (throw (Exception. (str "Can't regex-find on non-string: " infinitive " from word: " word)))))
           ere-type (re-find #"ere$" infinitive)
           ire-type (re-find #"ire$" infinitive)
           stem (string/replace infinitive #"[iae]re$" "")
           last-stem-char-is-i (re-find #"i$" stem)
           person (fs/get-in word '(:agr :person))
           number (fs/get-in word '(:agr :number))]
       (cond

        (and (= person :1st) (= number :sing)
             (string? (fs/get-in word '(:irregular :present :1sing))))
        (fs/get-in word '(:irregular :present :1sing))
        (and (= person :2nd) (= number :sing)
             (string? (fs/get-in word '(:irregular :present :2sing))))
        (fs/get-in word '(:irregular :present :2sing))
        (and (= person :3rd) (= number :sing)
             (string? (fs/get-in word '(:irregular :present :3sing))))
        (fs/get-in word '(:irregular :present :3sing))

        (and (= person :1st) (= number :plur)
             (string? (fs/get-in word '(:irregular :present :1plur))))
        (fs/get-in word '(:irregular :present :1plur))
        (and (= person :2nd) (= number :plur)
             (string? (fs/get-in word '(:irregular :present :2plur))))
        (fs/get-in word '(:irregular :present :2plur))
        (and (= person :3rd) (= number :plur)
             (string? (fs/get-in word '(:irregular :present :3plur))))
        (fs/get-in word '(:irregular :present :3plur))

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
        (str infinitive )))


     (= (fs/get-in word '(:infl)) :top)
     (str (fs/get-in word '(:infinitive)) )

     (and
      (fs/get-in word '(:a))
      (fs/get-in word '(:b)))
     (get-italian
      (fs/get-in word '(:a))
      (fs/get-in word '(:b)))

     ;; TODO: remove support for deprecated :root.
     (and
      (= (fs/get-in word '(:agr :gender)) :masc)
      (= (fs/get-in word '(:agr :number)) :sing)
      (= (fs/get-in word '(:cat)) :noun)
      (fs/get-in word '(:root)))
     (fs/get-in word '(:root))

     (and
      (= (fs/get-in word '(:agr :gender)) :fem)
      (= (fs/get-in word '(:agr :number)) :sing)
      (= (fs/get-in word '(:cat)) :noun))
     (fs/get-in word '(:italian))

     ;; deprecated: remove support for :root.
     (and
      (= (fs/get-in word '(:agr :gender)) :masc)
      (= (fs/get-in word '(:agr :number)) :plur)
      (= (fs/get-in word '(:cat) :noun))
      (fs/get-in word '(:root)))
     (string/replace (fs/get-in word '(:root))
                     #"[eo]$" "i") ;; dottore => dottori; medico => medici

   ;; deprecated: TODO: remove this
   (and
    (= (fs/get-in word '(:agr :gender)) :fem)
    (= (fs/get-in word '(:agr :number)) :plur)
    (= (fs/get-in word '(:cat)) :noun)
    (fs/get-in word '(:root)))
   (string/replace (fs/get-in word '(:root))
                   #"[a]$" "e") ;; donna => donne


   ;; deprecated: TODO: remove support for :root.
   (and
    (= (fs/get-in word '(:agr :gender)) :fem)
    (= (fs/get-in word '(:agr :number)) :sing)
    (= (fs/get-in word '(:cat)) :noun)
    (string? (fs/get-in word '(:root))))
   (fs/get-in word '(:root))

   (and
    (= (fs/get-in word '(:agr :gender)) :masc)
    (= (fs/get-in word '(:agr :number)) :sing)
    (= (fs/get-in word '(:cat) :adjective)))
   (fs/get-in word '(:italian)) ;; nero

   (and
    (= (fs/get-in word '(:agr :gender)) :masc)
    (= (fs/get-in word '(:agr :number)) :plur)
    (= (fs/get-in word '(:cat)) :adjective)
    ;; handle lexical exceptions.
    (string? (fs/get-in word '(:irregular :masc :plur))))
   (fs/get-in word '(:irregular :masc :plur))


   (and
    (= (fs/get-in word '(:agr :gender)) :fem)
    (= (fs/get-in word '(:agr :number)) :plur)
    (= (fs/get-in word '(:cat)) :adjective)
    ;; handle lexical exceptions.
    (string? (fs/get-in word '(:irregular :fem :plur))))
   (fs/get-in word '(:irregular :fem :plur))

   (string? (fs/get-in word '(:infinitive)))
   (fs/get-in word '(:infinitive))

   ;; TODO: throw exception rather than returning _word_, which is a map or something else unprintable.
   ;; in other words, if we've gotten this far, it's a bug.
   :else
   word))
  )

(defn get-italian [a & [ b ]]
  (let [a (if (nil? a) "" a)
        b (if (nil? b) "" b)
        a (get-italian-1 a)
        b (get-italian-1 b)]
    (cond

     (and (string? a)
          (= a "di")
          (string? b)
          (re-find #"^il (mio|tio|suo|nostro|vostro|loro)\b" b))
     (str a " " (string/replace b #"^il " ""))

     (and (string? a)
          (= a "di")
          (string? b)
          (re-find #"^la (mia|tia|sua|nostra|vostra|loro)\b" b))
     (str a " " (string/replace b #"^la " ""))

     (and (string? a)
          (= a "di")
          (string? b)
          (re-find #"^i (miei|tuoi|suoi|nostri|vostri|loro)\b" b))
     (str a " " (string/replace b #"^i " ""))

     (and (string? a)
          (= a "di")
          (string? b)
          (re-find #"^le (mie|tue|sue|nostre|vostre|loro)\b" b))
     (str a " " (string/replace b #"^le " ""))

     (and (= a "di i")
          (string? b)
          (re-find #"^[aeiou]" b))
     (str "degli " b)

     (and (= a "di i")
          (string? b)
          (re-find #"^s[t]" b))
     (str "degli " b)

     (and (= a "di i")
          (string? b))
     (str "dei " b)

     (and (= a "di il")
          (string? b))
     (get-italian "del" b)  ;; allows this to feed next rule:

     (and (= a "del")
          (string? b)
          (re-find #"^[aeiou]" b))
     (str "dell'" b)

     (and (= a "di la")
          (string? b))
     (get-italian "della" b) ;; allows this to feed next rule:

     (and (= a "della")
          (string? b)
          (re-find #"^[aeiou]" b))
     (str "dell'" b)

     (and (= a "di le")
          (string? b))
     (str "delle " b)

     (and (= a "i")
          (string? b)
          (re-find #"^[aeiou]" b))
     (str "gli " b)

     (and (= a "i")
          (string? b)
          (re-find #"^s[t]" b))
     (str "gli " b)

     (and (= a "un")
          (string? b)
          (re-find #"^s[t]" b))
     (str "uno " b)

     (and (= a "una")
          (string? b)
          (re-find #"^[aeiou]" b))
     (str "un'" b)

     (and (= a "il")
          (string? b)
          (re-find #"^[aeiou]" b))
     (str "l'" b)

     (and (= a "il")
          (string? b)
          (re-find #"^s[t]" b))
     (str "lo " b)

     (and (= a "la")
          (string? b)
          (re-find #"^[aeiou]" b))
     (str "l'" b)

     (and (= a "quell[ao]")
          (string? b)
          (re-find #"^[aeiou]" b))
     (str "quell'" b)

     (and (= a "quest[aeio]")
          (string? b)
          (re-find #"^[aeiou]" b))
     (str "quest'" b)

     ;; prepositional phrases
     (and (= a "a")
          (string? b)
          (re-find #"^il " b))
     (str "al " (string/replace b #"^il " ""))

     (and (= a "a")
          (string? b)
          (re-find #"^le " b))
     (str "alle " (string/replace b #"^le " ""))

     (and (= a "a")
          (string? b)
          (re-find #"^la " b))
     (str "alla " (string/replace b #"^la " ""))

     (and (string? a) (string? b))
     (str a " " b)

     (and (string? a) (string? (fs/get-in b '(:italian))))
     (str a " " (fs/get-in b '(:italian)))

     (and (string? (fs/get-in a '(:italian)))
          (string? b))
     (str (fs/get-in a '(:italian)) " " b)


     (and (= :verb (fs/get-in a '(:cat)))
          (= :noun (fs/get-in b '(:cat)))
          (= :acc (fs/get-in b '(:case))))
     ;; flip order in this case:
     ;; i.e. "vedo ti" => "ti vedo".
     {:a (if (nil? b) :top b)
      :b (if (nil? a) :top a)}

     true
     {:a (if (nil? a) :top a)
      :b (if (nil? b) :top b)})))

(declare get-english)
(declare plural-en)

(defn get-english-1 [word]
  (log/debug (str "get-english-1: " word))
  (cond

   ;; :note is used for little annotations that are significant in italian but not in english
   ;; e.g. gender signs (♂,♀) on nouns like "professore" and "professoressa".
   (and (string? (fs/get-in word '(:english)))
        (string? (fs/get-in word '(:note))))
   (str (get-english-1 (dissoc word :note)) " " (fs/get-in word '(:note)))

   (= (fs/get-in word '(:a)) :top)
   (str
    ".." " " (get-english-1 (fs/get-in word '(:b))))

   ;; show elipsis (..) if :b is not specified.
   (and
    (= (fs/get-in word '(:b)) :top)
    (string? (get-english-1 (fs/get-in word '(:a)))))
   (str
    (get-english-1 (fs/get-in word '(:a)))
    " " "..")

   ;; show elipsis (..) if :a is not specified.
   (and
    (= (fs/get-in word '(:b)) :top)
    (string? (fs/get-in word '(:a :english))))
   (str
    (get-english-1 (fs/get-in word '(:a :english)))
    " " "..")

   (string? word)
   (strip word)

   ;; (could have) + (to go) => "could have gone"
   (and
    (fs/get-in word '(:a))
    (fs/get-in word '(:b))
    (string? (fs/get-in word '(:a :irregular :past)))
    (= (fs/get-in word '(:irregular :past)) "could have")
    (string? (fs/get-in word '(:b :irregular :past-participle)))
    (= (fs/get-in word '(:a :infl)) :past))
   (string/join " " (list (fs/get-in word '(:a :irregular :past))
                          (fs/get-in word '(:b :irregular :past-participle))))

   ;; (could have) + (to sleep) => "could have slept"
   (and
    (fs/get-in word '(:a))
    (fs/get-in word '(:b))
    (string? (fs/get-in word '(:a :irregular :past)))
    (string? (fs/get-in word '(:b :irregular :past)))
    (= (fs/get-in word '(:a :infl)) :past))
   (string/join " " (list (fs/get-in word '(:a :irregular :past))
                          (fs/get-in word '(:b :irregular :past))))

   ;; (could have) + (do X) => "could have done X"
   (and
    (fs/get-in word '(:a))
    (fs/get-in word '(:b))
    (string? (fs/get-in word '(:a :irregular :past)))
    (= (fs/get-in word '(:a :irregular :past)) "could have")
    (string? (fs/get-in word '(:b :a :irregular :past-participle)))
    (= (fs/get-in word '(:a :infl)) :past))
   ;; recursive call after inflecting '(:b :a) to past.
   (get-english {:a (fs/get-in word '(:a))
                 :b {:a (fs/get-in word '(:b :a :irregular :past-participle))
                     :b (fs/get-in word '(:b :b))}})

   ;; (could have) + (make X) => "could have made X"
   (and
    (fs/get-in word '(:a))
    (fs/get-in word '(:b))
    (string? (fs/get-in word '(:a :irregular :past)))
    (= (fs/get-in word '(:a :irregular :past)) "could have")
    (string? (fs/get-in word '(:b :a :irregular :past)))
    (= (fs/get-in word '(:a :infl)) :past))
   ;; recursive call after inflecting '(:b :a) to past.
   (get-english {:a (fs/get-in word '(:a))
                 :b {:a (fs/get-in word '(:b :a :irregular :past))
                     :b (fs/get-in word '(:b :b))}})

   (and
    (fs/get-in word '(:a))
    (fs/get-in word '(:b))
    (string? (fs/get-in word '(:a)))
    (string? (fs/get-in word '(:b))))
   (string/join " "
         (list (fs/get-in word '(:a))
               (fs/get-in word '(:b))))

   (and
    (fs/get-in word '(:a))
    (fs/get-in word '(:b)))
   (get-english (fs/get-in word '(:a))
                (fs/get-in word '(:b)))

   (and (= :infinitive (fs/get-in word '(:infl)))
        (string? (fs/get-in word '(:infinitive))))
   (fs/get-in word '(:infinitive))

   (= true (fs/get-in word '(:hidden)))
;;   "Ø"
   ""
   (and
    (= true (fs/get-in word '(:a :hidden)))
    (= true (fs/get-in word '(:b :hidden))))
;;   "Ø"
   ""
   (= true (fs/get-in word '(:a :hidden)))
   (get-english-1 (fs/get-in word '(:b)))

   (= true (fs/get-in word '(:b :hidden)))
   (get-english-1 (fs/get-in word '(:a)))

   (and (= (fs/get-in word '(:infl)) :futuro)
        (fs/get-in word '(:infinitive))
        (not (nil? (fs/get-in word '(:agr :number))))
        (not (nil? (fs/get-in word '(:agr :person)))))
   (let [infinitive (fs/get-in word '(:infinitive))
         stem (string/replace infinitive #"^to " "")]
     (str "will " stem))

   (and (= (fs/get-in word '(:infl)) :imperfetto)
        (fs/get-in word '(:infinitive)))
   (let [infinitive (fs/get-in word '(:infinitive))
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
        (string? (fs/get-in word '(:irregular :imperfetto)))
        (fs/get-in word '(:irregular :imperfetto))

        (and (= :sing (fs/get-in word '(:agr :number)))
             (or (= :1st (fs/get-in word '(:agr :person)))
                 (= :3rd (fs/get-in word '(:agr :person))))
             (string? (fs/get-in word '(:irregular :imperfetto-suffix))))
        (str "was " (fs/get-in word '(:irregular :imperfetto-suffix)))

        (string? (fs/get-in word '(:irregular :imperfetto-suffix)))
        (str "were " (fs/get-in word '(:irregular :imperfetto-suffix)))

        (and (= :sing (fs/get-in word '(:agr :number)))
             (or (= :1st (fs/get-in word '(:agr :person)))
                 (= :3rd (fs/get-in word '(:agr :person)))))
        (str "was " stem "ing" (if to-final to-final ""))

        true
        (str "were " stem "ing" (if to-final to-final "")))))

   ;; irregular past (1): a single inflection for all persons/numbers.
   (and (= :past (fs/get-in word '(:infl)))
        (string? (fs/get-in word '(:irregular :past))))
   (fs/get-in word '(:irregular :past))

   (and (= :past (fs/get-in word '(:infl)))
        (= :top (fs/get-in word '(:agr :number)))
        (string? (fs/get-in word '(:irregular :past :2sing))))
   ;; use the 2nd singular form if there's not enough inflection info to go on.
   (str "[" (fs/get-in word '(:irregular :past :2sing)) "]")

   (= :top (fs/get-in word '(:infl)))
   (str (fs/get-in word '(:infinitive)) )

   ;; irregular past (2): a different inflection for each persons/numbers.
   (and (= :past (fs/get-in word '(:infl)))
        (map? (fs/get-in word '(:irregular :past))))
   (let [number (fs/get-in word '(:agr :number))
         person (fs/get-in word '(:agr :person))]
     (cond (and (= person :1st) (= number :sing))
           (fs/get-in word '(:irregular :past :1sing))
           (and (= person :2nd) (= number :sing))
           (fs/get-in word '(:irregular :past :2sing))
           (and (= person :3rd) (= number :sing))
           (fs/get-in word '(:irregular :past :3sing))
           (and (= person :1st) (= number :plur))
           (fs/get-in word '(:irregular :past :1plur))
           (and (= person :2nd) (= number :plur))
           (fs/get-in word '(:irregular :past :2plur))
           (and (= person :3rd) (= number :plur))
           (fs/get-in word '(:irregular :past :3plur))
           true word)) ;; not enough agreement specified to conjugate.

   ;; regular past
   (and (= :past (fs/get-in word '(:infl)))
        (string? (fs/get-in word '(:infinitive))))
   (let [infinitive (fs/get-in word '(:infinitive))
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
    (= :present (fs/get-in word '(:infl)))
    (string? (fs/get-in word '(:infinitive))))
   (let [root (fs/get-in word '(:infinitive))
         ;; TODO: throw exception rather than encoding error "nilrootz" as part
         ;; of the english string.
         root (if (nil? root) "(nilrootz)" root)
         root (if (not (= (type root) java.lang.String))
                (fs/get-in word '(:infinitive :infinitive))
                root)
         person (fs/get-in word '(:agr :person))
         number (fs/get-in word '(:agr :number))
         stem (string/replace root #"^to " "")
         last-stem-char-is-e (re-find #"e$" stem)
         last-stem-char-is-vowel (re-find #"[aeiou]$" stem)]
     (log/debug "+else")
     (log/debug (str "(english):word: " word))
     (cond

      (and (= person :1st) (= number :sing)
           (string? (fs/get-in word '(:irregular :present :1sing))))
      (fs/get-in word '(:irregular :present :1sing))
      (and (= person :2nd) (= number :sing)
           (string? (fs/get-in word '(:irregular :present :2sing))))
      (fs/get-in word '(:irregular :present :2sing))
      (and (= person :3rd) (= number :sing)
           (string? (fs/get-in word '(:irregular :present :3sing))))
      (fs/get-in word '(:irregular :present :3sing))

      (and (= person :1st) (= number :plur)
           (string? (fs/get-in word '(:irregular :present :1plur))))
      (fs/get-in word '(:irregular :present :1plur))
      (and (= person :2nd) (= number :plur)
           (string? (fs/get-in word '(:irregular :present :2plur))))
      (fs/get-in word '(:irregular :present :2plur))
      (and (= person :3rd) (= number :plur)
           (string? (fs/get-in word '(:irregular :present :3plur))))
      (fs/get-in word '(:irregular :present :3plur))

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

      (string? (fs/get-in word '(:infinitive)))
      (fs/get-in word '(:infinitive))

      :else (str root )))

   (and
    (fs/get-in word '(:irregular :plur))
    (= (fs/get-in word '(:agr :number)) :plur)
    (= (fs/get-in word '(:cat) :noun)))
   (fs/get-in word '(:irregular :plur))

   ;; TODO: remove support for deprecated :root - use :irregular instead (as immediately above).
   (and
    (fs/get-in word '(:root :irregular :plur))
    (= (fs/get-in word '(:agr :number)) :plur)
    (= (fs/get-in word '(:cat) :noun)))
   (fs/get-in word '(:root :irregular :plur))

   ;; TODO: remove support for deprecated :root - use :irregular instead.
   (and
    (= (fs/get-in word '(:agr :number)) :sing)
    (= (fs/get-in word '(:cat) :noun))
    (string? (fs/get-in word '(:root))))
   (fs/get-in word '(:root))

   (and
    (= (fs/get-in word '(:agr :number)) :sing)
    (= (fs/get-in word '(:cat) :noun))
    (string? (fs/get-in word '(:english))))
   (str (fs/get-in word '(:english))
        (if (fs/get-in word '(:note))
          (fs/get-in word '(:note))))

   ;; TODO: remove support for deprecated :root - use :irregular instead.
   (and
    (= (fs/get-in word '(:agr :number)) :sing)
    (= (fs/get-in word '(:cat) :noun))
    (string? (fs/get-in word '(:root :english))))
   (fs/get-in word '(:root :english))

   (and
    (= (fs/get-in word '(:agr :number)) :sing)
    (= (fs/get-in word '(:cat) :noun))
    (string? (fs/get-in word '(:english :english))))
   (str
    (fs/get-in word '(:english :english))
    (if (fs/get-in word '(:note))
      (fs/get-in word '(:note))))

   ;; TODO: remove support for deprecated :root - use :irregular instead.
   (and (= (fs/get-in word '(:agr :number)) :plur)
        (= (fs/get-in word '(:cat)) :noun)
        (string? (fs/get-in word '(:root))))
   (str (fs/get-in word '(:root)) "s")

   (and (= (fs/get-in word '(:agr :number)) :plur)
        (= (fs/get-in word '(:cat)) :noun)
        (string? (fs/get-in word '(:english))))
   (str (plural-en (fs/get-in word '(:english)))
        (if (fs/get-in word '(:note))
          (str (fs/get-in word '(:note)))))

   (and (= (fs/get-in word '(:agr :number)) :plur)
        (= (fs/get-in word '(:cat)) :noun)
        (string? (fs/get-in word '(:english :english))))
   (str (plural-en (fs/get-in word '(:english :english)))
        (if (fs/get-in word '(:english :note))
          (str (fs/get-in word '(:english :note)))))


   (and (= (fs/get-in word '(:cat)) :adjective)
        (string? (fs/get-in word '(:english))))
   (fs/get-in word '(:english))

   (string? (fs/get-in word '(:english)))
   (fs/get-in word '(:english))

   ;; TODO: not sure if this code is alive or not: is there ever
   ;; a case of a sign with '(:english :english :english)?
   (and (string? (fs/get-in word '(:english :english)))
        (= (.size (keys word)) 1))
   (fs/get-in word '(:english :english))

   (string? (fs/get-in word '(:infinitive)))
   (fs/get-in word '(:infinitive))

   :else
   word))

(defn get-english [a & [ b ] ]
  (let [a (if (nil? a) "" a)
        b (if (nil? b) "" b)
        re-a (get-english-1 a)
        re-b (get-english-1 b)]
    (log/debug (str "get-english a: " a " => " re-a))
    (log/debug (str "get-english b: " b " => " re-b))
    (log/debug (str "a is modal?: " (= true (fs/get-in a '(:modal)))))
    (cond

     (and (string? re-a)
          (map? re-b)
          (not (nil? (fs/get-in re-b '(:a))))
          (not (nil? (fs/get-in re-b '(:b)))))
     {:a re-a
      :b re-b}

     (and (map? a)
          (map? re-a))
          {:a re-a
           :b re-b}

     (and (map? a)
          (= (fs/get-in a '(:modal)) true)
          (string? re-b))
     (get-english-1 {:a re-a
                     :b (string/replace re-b #"^to " "")})

     (and (string? re-a)
          (string? re-b))
     (strip (str re-a " " re-b))

     ;; new-style n' -> adj noun
     (and
      (= (fs/get-in a '(:cat)) :adjective)
      (= (fs/get-in b '(:cat)) :noun)
      (= (fs/get-in a '(:agr :number)) :top))
     {:a a
      :b b}

     ;; old-style n' -> adj noun
     ;; TODO: remove this.
     (and
      (= (fs/get-in a '(:cat)) :noun)
      (= (fs/get-in b '(:cat)) :adjective))
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

     (and (string? re-a) (string? (fs/get-in re-b '(:english))))
     (str re-a " " (fs/get-in re-b '(:english)))

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

