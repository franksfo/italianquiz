(ns italianverbs.morphology
  (:refer-clojure :exclude [get-in])
  (:use [italianverbs.unify :only (ref? get-in fail?)])
   (:require
    [clojure.tools.logging :as log]
    [clojure.string :as string]))

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

(defn suffix-of [word]
  "compute the final character given a lexical entry and agreement info in :agr."
  (let [suffix (cond

                (and (= (get-in word '(:agr :gender)) :fem)
                     (= (get-in word '(:agr :number)) :sing)
                     (= (get-in word '(:essere)) true))
                "a"

                (and (= (get-in word '(:agr :gender)) :fem)
                     (= (get-in word '(:agr :number)) :plur)
                     (= (get-in word '(:essere)) true))
                "e"

                (and (= (get-in word '(:agr :number)) :plur)
                     (= (get-in word '(:essere)) true))
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
  (let [person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    {:person person
     :number number
   :infinitive?    (and (= :infinitive (get-in word '(:infl)))
                        (string? (get-in word '(:infinitive))))

   :irregular-futuro?    (and
                          (= (get-in word '(:infl)) :futuro)
                          (map? (get-in word '(:irregular :futuro))))

   :regular-futuro?    (and (= (get-in word '(:infl)) :futuro)
                            (get-in word '(:infinitive)))

   :regular-imperfetto?    (and (= (get-in word '(:infl)) :imperfetto)
                                (get-in word '(:infinitive)))

   :irregular-past?    (and
                        (= :past (get-in word '(:infl)))
                        (string? (get-in word '(:irregular :past))))

   ;;nei: not enough information to conjugate.
   :past-irregular-essere-type-nei
   (and (= :past (get-in word '(:infl)))
        (get-in word '(:irregular :passato))
        (get-in word '(:essere) true)
        (or (= :notfound (get-in word '(:agr :number) :notfound))
            (= :top (get-in word '(:agr :number)))))


   ;;nei: not enough information to conjugate.
   :past-esseri-but-nei?
   (and (= :past (get-in word '(:infl)))
        (get-in word '(:essere) true)
        (or (= :notfound (get-in word '(:agr :number) :notfound))
            (= :top (get-in word '(:agr :number)))))
   :irregular-passato?
   (and (= :past (get-in word '(:infl)))
        (get-in word '(:irregular :passato)))

   :regular-passato
   (= :past (get-in word '(:infl)))

   :present
     (= (get-in word '(:infl)) :present)


}))

(defn get-italian-1 [word]
  (let [analysis (analyze-italian-1 word)
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))
        info (log/debug "get-italian-1: input word: " word)
        ]

    (if (and false get-in word '(:a))
      (do (log/info (str "a? " (get-in word '(:a))))
          (log/info (str "b? " (get-in word '(:b))))
          (log/info (str "analysis: " analysis))))

    (log/debug (str "word's a is a string? " (get-in word '(:a)) " => " (string? (get-in word '(:a)))))
    (log/debug (str "word's b is a map? " (get-in word '(:b)) " => " (map? (get-in word '(:b)))))

    (log/debug (str "word's a italian is a string? " (get-in word '(:a :italian)) " => " (string? (get-in word '(:a :italian)))))


    ;; throw exception if contradictory facts are found:
;    (if (= (get-in word '(:a :initial) false))
;      (throw (Exception. (str ":a's initial is false: (:a should always be initial=true)."))))

    (cond

     (= word :top) ".."

     (ref? word)
     (get-italian-1 @word)

     ;; TODO: this is a special case that should be handled below instead
     ;; of forcing every input to go through this check.
     (= word {:initial false})
     ".."
     (= word {:initial true})
     ".."

     (and (string? (get-in word '(:a)))
          (string? (get-in word '(:b))))
     (get-italian (get-in word '(:a))
                  (get-in word '(:b)))

     (and (string? (get-in word '(:a)))
          (map? (get-in word '(:b))))
     (get-italian (get-in word '(:a))
                  (get-in word '(:b)))

     (and (string? (get-in word '(:a :italian)))
          (map? (get-in word '(:b))))
     (do
       (get-italian (get-in word '(:a :italian))
                    (get-in word '(:b))))


     (and (map? (get-in word '(:a)))
          (map? (get-in word '(:b))))
     (get-italian
      (get-in word '(:a))
      (get-in word '(:b)))

     ;; TODO: this rule is pre-empting all of the following rules
     ;; that look in :a and :b. Either remove those following rules
     ;; if they are redundant and not needed, or move this general rule
     ;; below the following rules.
     (and (not (= :none (get-in word '(:a) :none)))
          (not (= :none (get-in word '(:b) :none))))
     (get-italian (get-in word '(:a))
                  (get-in word '(:b)))

     (and
      (string? (get-in word '(:a :italian)))
      (string? (get-in word '(:b :infinitive)))
      (or (= :none (get-in word '(:b :agr :number) :none))
          (= :top (get-in word '(:b :agr :number) :none)))
      )
     (str (strip (get-in word '(:a :italian)))
          " "
          (strip (get-in word '(:b :infinitive))))

     (and
      (string? (get-in word '(:a)))
      (string? (get-in word '(:b :infinitive)))
      (or (= :none (get-in word '(:b :agr :number) :none))
          (= :top (get-in word '(:b :agr :number) :none)))
      )
     (str (strip (get-in word '(:a)))
          " "
          (strip (get-in word '(:b :infinitive))))


     (and
      (string? (get-in word '(:a :infinitive)))
      (get-in word '(:a :infinitive))
      (or (= :none (get-in word '(:b :agr :number) :none))
          (= :top (get-in word '(:b :agr :number) :none)))
      (= (get-in word '(:a :infl)) :top))
     (strip (str (get-in word '(:a :infinitive))
                 " " (get-italian-1 (get-in word '(:b)))))

     ;; handle lexical exceptions (plural feminine adjectives):
     (and
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:cat)) :adjective)
      (string? (get-in word '(:irregular :fem :plur))))
     (get-in word '(:irregular :fem :plur))

     ;; handle lexical exceptions (plural feminine adjectives):
     (and
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:cat)) :adjective)
      (string? (get-in word '(:irregular :fem :plur))))
     (get-in word '(:irregular :fem :plur))

     ;; handle lexical exceptions (plural masculine adjectives):
     (and
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:agr :gender)) :masc)
      (= (get-in word '(:cat)) :adjective)
      (string? (get-in word '(:irregular :masc :plur))))
     (get-in word '(:irregular :masc :plur))

     (and
      (or (= (get-in word '(:agr :gender)) :masc)
          (= (get-in word '(:agr :gender)) :top))
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat)) :adjective))
     (string/replace (get-in word '(:italian))
                     #"[eo]$" "i") ;; nero => neri

     (and
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat)) :adjective))
     (string/replace (get-in word '(:italian))
                     #"[eo]$" "e") ;; nero => nere

     ;; handle lexical exceptions (plural nouns):
     (and
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat)) :noun)
      (string? (get-in word '(:irregular :plur))))
     (get-in word '(:irregular :plur))

     ;; regular masculine nouns
     (and
      (= (get-in word '(:agr :gender)) :masc)
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat) :noun))
      (get-in word '(:italian)))
     (string/replace (get-in word '(:italian))
                     #"[eo]$" "i") ;; dottore => dottori; medico => medici

     ;; regular feminine nouns
     (and
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat) :noun))
      (get-in word '(:italian)))
     (string/replace (get-in word '(:italian))
                     #"[a]$" "e") ;; donna => donne

     ;; TODO: move this down to other adjectives.
     ;; this was moved up here to avoid
     ;; another rule from matching it.
     (and
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat)) :adjective))
     (string/replace (get-in word '(:italian))
                     #"[eo]$" "e") ;; nero => nere

     (and
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:agr :number)) :sing)
      (= (get-in word '(:cat)) :adjective))
     (string/replace (get-in word '(:italian))
                     #"[eo]$" "a") ;; nero => nera
     (and
      (string? (get-in word '(:italian)))
      (= :top (get-in word '(:agr :sing) :top)))
     (str (get-in word '(:italian)))

     (= (get-in word '(:a)) :top)
     (str
      ".." " " (get-italian-1 (get-in word '(:b))))

     (and
      (= (get-in word '(:b)) :top)
      (string? (get-italian-1 (get-in word '(:a)))))
     (str
      (get-italian-1 (get-in word '(:a)))
      " " "..")

     (and
      (= (get-in word '(:b)) :top)
      (string? (get-in word '(:a :italian))))
     (str
      (get-italian-1 (get-in word '(:a :italian)))
      " " "..")

     (and (= :infinitive (get-in word '(:infl)))
          (string? (get-in word '(:infinitive))))
     (get-in word '(:infinitive))

     (and
      (= (get-in word '(:infl)) :futuro)
      (map? (get-in word '(:irregular :futuro))))
     (let [infinitive (get-in word '(:infinitive))
           person (get-in word '(:agr :person))
           number (get-in word '(:agr :number))]
       (cond
        (and (= person :1st) (= number :sing))
        (get-in word '(:irregular :futuro :1sing))
        (and (= person :2nd) (= number :sing))
        (get-in word '(:irregular :futuro :2sing))
        (and (= person :3rd) (= number :sing))
        (get-in word '(:irregular :futuro :3sing))
        (and (= person :1st) (= number :plur))
        (get-in word '(:irregular :futuro :1plur))
        (and (= person :2nd) (= number :plur))
        (get-in word '(:irregular :futuro :2plur))
        (and (= person :3rd) (= number :plur))
        (get-in word '(:irregular :futuro :3plur))


        (and (= (get-in word '(:infl)) :futuro)
             (string? (get-in word '(:infinitive))))
        (str (get-in word '(:infinitive)) " (futuro)")

        true ;; failthrough: should usually not get here:
        ;; TODO: describe when it might be ok, i.e. why log/warn not log/error.
        (do (log/warn (str "get-italian-1 could not match: " word))
        word)))

     ;; regular inflection of futuro.
     (and (= (get-in word '(:infl)) :futuro)
          (get-in word '(:infinitive)))
     (let [infinitive (get-in word '(:infinitive))
           person (get-in word '(:agr :person))
           number (get-in word '(:agr :number))
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
        (get-in word '(:infinitive))))


     ;; irregular imperfetto sense:
     ;; 1) use irregular based on number and person.
     (and
      (= (get-in word '(:infl)) :imperfetto)
      (= :sing (get-in word '(:agr :number)))
      (= :1st (get-in word '(:agr :person)))
      (string? (get-in word '(:irregular :imperfetto :1sing))))
     (get-in word '(:irregular :imperfetto :1sing))

     (and
      (= (get-in word '(:infl)) :imperfetto)
      (= :sing (get-in word '(:agr :number)))
      (= :2nd (get-in word '(:agr :person)))
      (string? (get-in word '(:irregular :imperfetto :2sing))))
     (get-in word '(:irregular :imperfetto :2sing))

     (and
      (= (get-in word '(:infl)) :imperfetto)
      (= :sing (get-in word '(:agr :number)))
      (= :3rd (get-in word '(:agr :person)))
      (string? (get-in word '(:irregular :imperfetto :3sing))))
     (get-in word '(:irregular :imperfetto :3sing))

     (and
      (= (get-in word '(:infl)) :imperfetto)
      (= :plur (get-in word '(:agr :number)))
      (= :1st (get-in word '(:agr :person)))
      (string? (get-in word '(:irregular :imperfetto :1plur))))
     (get-in word '(:irregular :imperfetto :1plur))
     (and
      (= (get-in word '(:infl)) :imperfetto)
      (= :plur (get-in word '(:agr :number)))
      (= :2nd (get-in word '(:agr :person)))
      (string? (get-in word '(:irregular :imperfetto :2plur))))
     (get-in word '(:irregular :imperfetto :2plur))
     (and
      (= (get-in word '(:infl)) :imperfetto)
      (= :plur (get-in word '(:agr :number)))
      (= :3rd (get-in word '(:agr :person)))
      (string? (get-in word '(:irregular :imperfetto :3plur))))
     (get-in word '(:irregular :imperfetto :3plur))


     ;; regular imperfetto sense
     (and (= (get-in word '(:infl)) :imperfetto)
          (get-in word '(:infinitive)))
     (let [infinitive (get-in word '(:infinitive))
           person (get-in word '(:agr :person))
           number (get-in word '(:agr :number))
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
      (= :past (get-in word '(:infl)))
      (string? (get-in word '(:irregular :past))))
     (get-in word '(:irregular :past))

     (and
      (get-in word '(:a))
      (get-in word '(:b))
      true) (str
             (strip (get-italian-1 (get-in word '(:a)))) " "
             (strip (get-italian-1 (get-in word '(:b)))))

     ;; "fare [past]" + "bene" => "fatto bene"
     (and (= (get-in word '(:cat)) :verb)
          (= (get-in word '(:infl)) :past)
          (string? (get-in word '(:a :irregular :passato))))
     (str (get-in word '(:a :irregular :passato)) " "
          (get-italian-1 (get-in word '(:b))))

     ;; TODO: do not use brackets: if there's an error about there being
     ;; not enough information, throw an exception explicitly.
     ;; return the irregular form in square brackets, indicating that there's
     ;; not enough information to conjugate the verb.
     (and (= :past (get-in word '(:infl)))
          (get-in word '(:irregular :passato))
          (get-in word '(:essere) true)
          (or (= :notfound (get-in word '(:agr :number) :notfound))
              (= :top (get-in word '(:agr :number)))))
     (str "[" (get-in word '(:irregular :passato)) "]")

     ;; TODO: do not use brackets: if there's an error about there being
     ;; regular passato prossimo and essere-verb => NEI (not enough information): defer conjugation and keep as a map.
     (and (= :past (get-in word '(:infl)))
          (= (get-in word '(:essere)) true)
          (or (= :notfound (get-in word '(:agr :number) :notfound))
              (= :top (get-in word '(:agr :number)))))
     (str "[" (get-in word '(:infinitive)) " (past)]")

     ;; conjugate irregular passato
     (and (= :past (get-in word '(:infl)))
          (get-in word '(:irregular :passato)))
     (let [irregular-passato (get-in word '(:irregular :passato))
           butlast (nth (re-find #"(.*).$" irregular-passato) 1)]
       (str butlast (suffix-of word)))

     ;; conjugate regular passato
     (and (= :past (get-in word '(:infl)))
          (string? (get-in word '(:infinitive))))
     (let [infinitive (get-in word '(:infinitive))
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

     (and (= (get-in word '(:infl)) :present)
          (= person :1st) (= number :sing)
          (string? (get-in word '(:irregular :present :1sing))))
     (get-in word '(:irregular :present :1sing))

     (and (= (get-in word '(:infl)) :present)
          (= person :2nd) (= number :sing)
          (string? (get-in word '(:irregular :present :2sing))))
     (get-in word '(:irregular :present :2sing))

     (and (= (get-in word '(:infl)) :present)
          (= person :3rd) (= number :sing)
          (string? (get-in word '(:irregular :present :3sing))))
     (get-in word '(:irregular :present :3sing))

     (and (= (get-in word '(:infl)) :present)
          (= person :1st) (= number :plur)
          (string? (get-in word '(:irregular :present :1plur))))
     (get-in word '(:irregular :present :1plur))

     (and (= (get-in word '(:infl)) :present)
          (= person :2nd) (= number :plur)
          (string? (get-in word '(:irregular :present :2plur))))
     (get-in word '(:irregular :present :2plur))

     (and (= (get-in word '(:infl)) :present)
          (= person :3rd) (= number :plur)
          (string? (get-in word '(:irregular :present :3plur))))
     (get-in word '(:irregular :present :3plur))

     (and
      (= (get-in word '(:infl)) :present)
      (string? (get-in word '(:infinitive))))
     (let [infinitive (get-in word '(:infinitive))
           are-type (try (re-find #"are$" infinitive)
                         (catch Exception e
                           (throw (Exception. (str "Can't regex-find on non-string: " infinitive " from word: " word)))))
           ere-type (re-find #"ere$" infinitive)
           ire-type (re-find #"ire$" infinitive)
           stem (string/replace infinitive #"[iae]re$" "")
           last-stem-char-is-i (re-find #"i$" stem)
           person (get-in word '(:agr :person))
           number (get-in word '(:agr :number))]
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

     (= (get-in word '(:infl)) :top)
     (str (get-in word '(:infinitive)) )

     (and
      (get-in word '(:a))
      (get-in word '(:b)))
     (get-italian
      (get-in word '(:a))
      (get-in word '(:b)))

     ;; TODO: remove support for deprecated :root.
     (and
      (= (get-in word '(:agr :gender)) :masc)
      (= (get-in word '(:agr :number)) :sing)
      (= (get-in word '(:cat)) :noun)
      (get-in word '(:root)))
     (get-in word '(:root))

     (and
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:agr :number)) :sing)
      (= (get-in word '(:cat)) :noun))
     (get-in word '(:italian))

     ;; deprecated: remove support for :root.
     (and
      (= (get-in word '(:agr :gender)) :masc)
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat) :noun))
      (get-in word '(:root)))
     (string/replace (get-in word '(:root))
                     #"[eo]$" "i") ;; dottore => dottori; medico => medici

   ;; deprecated: TODO: remove this
   (and
    (= (get-in word '(:agr :gender)) :fem)
    (= (get-in word '(:agr :number)) :plur)
    (= (get-in word '(:cat)) :noun)
    (get-in word '(:root)))
   (string/replace (get-in word '(:root))
                   #"[a]$" "e") ;; donna => donne


   ;; deprecated: TODO: remove support for :root.
   (and
    (= (get-in word '(:agr :gender)) :fem)
    (= (get-in word '(:agr :number)) :sing)
    (= (get-in word '(:cat)) :noun)
    (string? (get-in word '(:root))))
   (get-in word '(:root))

   (and
    (= (get-in word '(:agr :gender)) :masc)
    (= (get-in word '(:agr :number)) :sing)
    (= (get-in word '(:cat) :adjective)))
   (get-in word '(:italian)) ;; nero

   (and
    (= (get-in word '(:agr :gender)) :masc)
    (= (get-in word '(:agr :number)) :plur)
    (= (get-in word '(:cat)) :adjective)
    ;; handle lexical exceptions.
    (string? (get-in word '(:irregular :masc :plur))))
   (get-in word '(:irregular :masc :plur))


   (and
    (= (get-in word '(:agr :gender)) :fem)
    (= (get-in word '(:agr :number)) :plur)
    (= (get-in word '(:cat)) :adjective)
    ;; handle lexical exceptions.
    (string? (get-in word '(:irregular :fem :plur))))
   (get-in word '(:irregular :fem :plur))

   (string? (get-in word '(:infinitive)))
   (get-in word '(:infinitive))

   (or
    (not (= :none (get-in word '(:a) :none)))
    (not (= :none (get-in word '(:b) :none))))
   (get-italian (get-in word '(:a))
                (get-in word '(:b)))

   (or
    (= (get-in word '(:agr :case)) {:not :acc})
    (= (get-in word '(:agr)) :top))
   ".."

   ;; TODO: throw exception rather than returning _word_, which is a map or something else unprintable.
   ;; in other words, if we've gotten this far, it's a bug.
   :else
   word)
  ))

(defn get-italian [a & [ b ]]
  (let [a (if (nil? a) "" a)
        b (if (nil? b) "" b)
        a (get-italian-1 a)
        b (get-italian-1 b)
        info-a (log/debug (str "get-italian: a: " a))
        info-b (if b (log/debug (str "get-italian: b: " b)))

        it-b (log/debug "it-a is string? " (string? (get-in a '(:italian))))
        it-b (log/debug "it-b is string? " (string? (get-in b '(:italian))))

        cat-a (log/debug (str "cat a:" (get-in a '(:cat))))
        cat-b (log/debug (str "cat b:" (get-in b '(:cat))))

        ]
    (cond

     (and (= a "i")
          (string? (get-in b '(:italian)))
          (re-find #"^[aeiou]" (get-in b '(:italian))))
     (str "gli " b)

     ;; TODO: cleanup & remove.
     (and false ;; going to throw out this logic: will use :initial and rule schemata instead.
          (= :verb (get-in a '(:cat)))
          (= :noun (get-in b '(:cat)))
          (= :acc (get-in b '(:case))))
     ;; flip order in this case:
     ;; i.e. "vedo ti" => "ti vedo".
     {:a (if (nil? b) :top b)
      :b (if (nil? a) :top a)}

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

     (and (= (get-in a '(:italian)) "di i")
          (string? b))
     (str "dei " b)

     (and (= (get-in a '(:italian)) "di i")
          (string? (get-in b '(:italian))))
     (str "dei " (get-italian-1 (get-in b '(:italian))))

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
          (string? (get-in b '(:italian)))
          (re-find #"^[aeiou]" (get-in b '(:italian))))
     (str "gli " b)

     (and (= a "i")
          (string? b)
          (re-find #"^s[t]" b))
     (str "gli " b)

     ;; 1),2),3) handle e.g. "io lo ho visto" => "io l'ho visto"
     ;; 1)
     (and (= a "mi")
          (string? b)
          (re-find #"^[aeiouh]" b))
     (str "m'" b)
     ;; 2)
     (and (= a "ti")
          (string? b)
          (re-find #"^[aeiouh]" b))
     (str "t'" b)
     ;; 3)
     (and (string? a)
          (re-find #"^l[ao]$" a)
          (string? b)
          (re-find #"^[aeiouh]" b))
     (str "l'" b)

     ;; 4) handle e.g. "aiutari + ti" => "aiutarti"
     (and (string? a)
          (or (re-find #"are$" a)
              (re-find #"ere$" a)
              (re-find #"ire$" a))
          (or (= b "ci")
              (= b "mi")
              (= b "la")
              (= b "le")
              (= b "li")
              (= b "lo")
              (= b "ti")
              (= b "vi")))
     (str (string/replace a #"[e]$" "")
          b)

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
          (re-find #"^s[ct]" b))
     (str "lo " b)

     (and (= a "la")
          (string? b)
          (re-find #"^[aeiou]" b))
     (str "l'" b)

     (and (= a "quell[ao]")
          (string? b)
          (re-find #"^[aeiou]" b))
     (str "quell'" b)

     (and (= a "quelli")
          (string? b)
          (re-find #"^(st|sc|[aeiou])" b))
     (str "quegli " b)

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
          (re-find #"^i " b))
     (str "ai " (string/replace b #"^il " ""))

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

     (and (string? a) (string? (get-in b '(:italian))))
     (str a " " (get-in b '(:italian)))

     (and (string? (get-in a '(:italian)))
          (string? b))
     (str (get-in a '(:italian)) " " b)

     (and (string? a)
          (map? b))
     (str a " ..")

     (and (string? b)
          (map? a))
     (str " .." b)

     true
     {:a (if (nil? a) :top a)
      :b (if (nil? b) :top b)})))

(declare get-english)
(declare plural-en)

(defn get-english-1 [word]
  (log/debug (str "get-english-1: " word))
  (cond

   (= word :top)
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

   (and (= (get-in word '(:infl)) :futuro)
        (get-in word '(:infinitive))
        (not (nil? (get-in word '(:agr :number))))
        (not (nil? (get-in word '(:agr :person)))))
   (let [infinitive (get-in word '(:infinitive))
         stem (string/replace infinitive #"^to " "")]
     (str "will " stem))

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

     (and (map? a)
          (nil? (get-in a '(:english)))
          (string? (get-in b '(:english))))
     (str ".. " (get-in b '(:english)))

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

     (and (string? a)
          (map? b))
     (str a " ..")

     (and (string? b)
          (map? a))
     (str ".. " b)

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



(defn capitalize [s]
  "Capitalize first char and leave the rest of the characters alone (compare with string/capitalize which lower-cases all chars after first."
  (if (nil? s) ""
      (let [s (.toString s)]
        (if (< (count s) 2)
          (.toUpperCase s)
          (str (.toUpperCase (subs s 0 1))
               (subs s 1))))))

(defn fo-ps [expr]
  "show the phrase-structure of a phrase structure tree, e.g [hh21 'mangiare (to eat)' [cc10 'il (the)' 'pane(bread)']]"
  (cond

   (and
    (or (set? expr)
        (seq? expr))
    (not (empty? expr)))

   ;; expr is a sequence or set: assume each is a phrase structure tree and show each.
   (reduce (fn [x y]
             ;; show a bar (|) between each phrase structure tree (assuming each member of expr is a phrase structure tree).
             (str x " | " y))
           (map (fn [each]
                  (fo-ps each))
                expr))

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
    (:italian expr)
    (:english expr))
   (str (get-italian-1 (get-in expr '(:italian)))
        " ("
        (get-english-1 (get-in expr '(:english)))
        ")")

   true
   expr))


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
;;; (formattare (over (over s (over (over np lexicon) (lookup {:synsem {:human true}}))) (over (over vp lexicon) (over (over np lexicon) lexicon))))
(defn formattare [expressions]
  "format a bunch of expressions (feature-structures) showing just the italian (and english in parentheses)."
  (cond
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

(defn fo [expressions]
  (formattare expressions))

(defn fof [expressions]
  (let [retval (lazy-seq (list (formattare expressions)))
        total-time (time (first retval))]
    (log/debug (str "tense info: " (get-in expressions '(:comp :synsem :sem :tense))))
    (log/debug (str "FINISHED FORMATTING: " (first retval))) ;; acts as a delimiter for this sentence's generation for debugging purposes.
    retval))

(defn finalize [expr]
  (if (seq? expr)
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

