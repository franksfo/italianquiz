(ns italianverbs.morphology
  (:require
   [italianverbs.fs :as fs]
   [clojure.tools.logging :as log]
   [clojure.string :as string]))

(defn final-char-of [string]
  (get string (- (.length string) 1)))

(defn next-to-final-char-of [string]
  (if (> (.length string) 1)
    (get string (- (.length string) 2))))

(defn conjugate-it [arg category & [ infl ]]
  "conjugate an italian expression."
  (do
    (log/info (str "=conjugate-it=="))
    (log/info (str "=>conjugate-it: arg=" arg  " ; category=" category))
    (log/info (str "arg: " arg))
    (log/info (str "category: " category))
    (log/info (str "arg's infl: " (fs/get-in arg '(:infl))))
    (log/info (str "infl: " infl))
    (log/info (str "agreement: " (fs/get-in arg '(:agr))))
    (log/info (str "futuro? " (= (fs/get-in arg '(:infl)) :futuro)))
    (log/info (str ":cat of arg:" (fs/get-in arg '(:cat)))))

  (let [retval
        (cond (nil? arg) ""
              (= (type arg) java.lang.String)
              arg

              (and (map? arg)
                   (contains? (set (keys arg)) :1)
                   (contains? (set (keys arg)) :2))
              (let [result1 (conjugate-it (:1 arg) category infl)
                    result2 (conjugate-it (:2 arg) category infl)]
                (if (and (= (type result1) java.lang.String)
                         (= (type result2) java.lang.String))
                  (string/join " " (list result1 result2))
                  {:1 result1
                   :2 result2}))

              ;; irregular present-tense (e.g. "fare")
              (and (map? arg)
                   (contains? (set (keys arg)) :agr)
                   (contains? (set (keys arg)) :infinitive)
                   ;; TODO: check (= :present) rather than (not (= :past)): requires that we have (:infl :present)
                   ;; explicitly rather than implicitly (i.e. it is the default)
                   (not (= :past (fs/get-in arg '(:infl))))
                   (not (= :futuro (fs/get-in arg '(:infl))))
                   (map? (fs/get-in arg '(:infinitive :irregular :present))))
              (let [root (fs/get-in arg '(:infinitive))
                    person (fs/get-in arg '(:agr :person) :notfound)
                    number (fs/get-in arg '(:agr :number) :notfound)
                    present (fs/get-in arg '(:infinitive :irregular :present))]
                (log/info "it:1 +agr +infinitive")
                (log/info (str "it:1 person: " person "; number: " number))
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
                 
                 :else
                 ;; nothing in the supplied agr is enough to tell us how to inflect this present-tense
                 ;; verb. Sometimes this should be treated as an error, sometimes not, as it's too soon
                 ;; in the generation process (e.g. generation of a vp) to say the situtation is an error.
                 ;; For now, hardwired to never be an error.
                 (let [error-on-unspecified false]
                   (if error-on-unspecified
                     {:error "no conjugation specified."
                      :num number
                      :person person
                      :arg arg}
                     (do
                       (log/warn (str "no conjugation specified in: " arg))
                       arg)))))

              (= (type arg) clojure.lang.Keyword)
              (throw (Exception. (str "cannot conjugate: " arg)))
              
              ;; <deprecated: use :italian instead of :root>
              (and (map? arg)
                   (contains? arg :root)
                   (contains? arg :agr)
                   (= (fs/get-in arg '(:agr :gender)) :fem)
                   (= (fs/get-in arg '(:agr :number)) :sing))
              (do
                (log/info "it:2 +root +fem +sing")
                (fs/get-in arg '(:root)))
              
              ;; irregular noun: plural
              (and (map? arg)
                   (not (= :notfound (fs/get-in arg '(:root :irregular :plur) :notfound)))
                   (= (fs/get-in arg '(:agr :number)) :plur))
              (do
                (log/info "it:3 +root +irreg +plur")
                (fs/get-in arg '(:root :irregular :plur)))
              
              ;; irregular noun: singular
              (and (map? arg)
                   (not (= :notfound (fs/get-in arg '(:root :irregular :plur) :notfound)))
                   (= (fs/get-in arg '(:agr :number)) :sing))
              (do
                (log/info "it:4 +root +irreg +sing")
                (fs/get-in arg '(:root :italian)))
              
              (and (map? arg)
                   (contains? arg :root)
                   (contains? arg :agr)
                   (= (fs/get-in arg '(:agr :gender)) :masc)
                   (= (fs/get-in arg '(:agr :number)) :sing))
              (do
                (log/info "it:5 +root +agr +masc +sing")
                (fs/get-in arg '(:root)))
              
              ;; regular masculine noun pluralization
              (and (map? arg)
                   (= :notfound (fs/get-in arg '(:italian :irregular :plur) :notfound))
                   (contains? arg :root)
                   (contains? arg :agr)
                   (= (fs/get-in arg '(:agr :gender)) :masc)
                   (= (fs/get-in arg '(:agr :number)) :plur))
              (do
                (log/info "it:6 +root +agr +masc +plur")
                (string/replace (fs/get-in arg '(:root))
                                #"[eo]$" "i")) ;; dottore => dottori; medico => medici
              
              ;; feminine noun pluralization
              (and (map? arg)
                   (= :notfound (fs/get-in arg '(:italian :irregular :plur) :notfound))
                   (contains? arg :root)
                   (contains? arg :agr)
                   (= (fs/get-in arg '(:agr :gender)) :fem)
                   (= (fs/get-in arg '(:agr :number)) :plur))
              (do
                (log/info "it:7 +root +agr +fem +plur +regular +deprecated")
                (string/replace (fs/get-in arg '(:root))
                                #"a$" "e"))
              
              ;; </deprecated: use :italian instead of :root>
              
              ;; <replace with>
              
              ;; irregular noun: masc plural
              (and (map? arg)
                   (not (= :notfound (fs/get-in arg '(:irregular :masc :plur) :notfound)))
                   (= (fs/get-in arg '(:agr :number)) :plur)
                   (= (fs/get-in arg '(:agr :gender)) :masc))
              (do
                (log/info "it:8 +map +irreg +plur +masc")
                (fs/get-in arg '(:irregular :masc :plur)))
              
              ;; irregular noun: fem plural
              (and (map? arg)
                   (not (= :notfound (fs/get-in arg '(:irregular :fem :plur) :notfound)))
                   (= (fs/get-in arg '(:agr :number)) :plur)
                   (= (fs/get-in arg '(:agr :gender)) :fem))
              (do
                (log/info "it:9 +map +irreg +plur +fem")
                (fs/get-in arg '(:irregular :fem :plur)))
              
              ;; regular noun: masc sing
              (and (map? arg)
                   (contains? arg :italian)
                   (contains? arg :agr)
                   (= (fs/get-in arg '(:agr :gender)) :masc)
                   (= (fs/get-in arg '(:agr :number)) :sing))
              (do
                (log/info "it:11 +masc +sing")
                (fs/get-in arg '(:italian)))
              
              ;; regular masculine noun pluralization
              (and (map? arg)
                   (contains? arg :italian)
                   (contains? arg :agr)
                   (= (fs/get-in arg '(:agr :gender)) :masc)
                   (= (fs/get-in arg '(:agr :number)) :plur))
              (do
                (log/info "it:12 +masc +plur")
                (string/replace (fs/get-in arg '(:italian))
                                #"[eo]$" "i")) ;; dottore => dottori; medico => medici
              
              ;; regular feminine noun pluralization
              (and (map? arg)
                   (or (= category :noun)
                       (= (fs/get-in arg '(:cat)) :noun))
                   (contains? arg :italian)
                   (contains? arg :agr)
                   (= (fs/get-in arg '(:agr :gender)) :fem)
                   (= (fs/get-in arg '(:agr :number)) :plur))
              (do
                (log/info "it:13 +fem +plur +regular")
                (string/replace (fs/get-in arg '(:italian))
                                #"[ao]$" "e"))
              ;; </replace with>
              
              
              ;; masculine adjective agreement: singular
              (and (map? arg)
                   (or (= category :adjective)
                       (= (fs/get-in arg '(:cat)) :adjective))
                   (contains? arg :italian)
                   (contains? arg :agr)
                   (= (fs/get-in arg '(:agr :gender)) :masc)
                   (= (fs/get-in arg '(:agr :number)) :sing))
              (do
                (log/info "it:14 +sing +adj +italian +masc")
                (fs/get-in arg '(:italian)))
              
              ;; masculine irregular plural adjective agreement.
              (and (map? arg)
                   (or (= category :adjective)
                       (= (fs/get-in arg '(:cat)) :adjective)
                       (= category :noun)
                       (= (fs/get-in arg '(:cat)) :noun))
                   (contains? arg :italian)
                   (contains? arg :agr)
                   (= (fs/get-in arg '(:agr :gender)) :masc)
                   (= (fs/get-in arg '(:agr :number)) :plur)
                   (not (= (fs/get-in arg '(:irregular :masc :plur) :notfound) :notfound)))
              (do
                (log/info "it:15 +irreg +plur +masc +(adj|noun)")
                (fs/get-in arg '(:irregular :masc :plur)))
              
              ;; feminine irregular plural adjective agreement.
              (and (map? arg)
                   (or (= category :adjective)
                       (= (fs/get-in arg '(:cat)) :adjective)
                       (= category :noun)
                       (= (fs/get-in arg '(:cat)) :noun))
                   (contains? arg :italian)
                   (contains? arg :agr)
                   (= (fs/get-in arg '(:agr :gender)) :fem)
                   (= (fs/get-in arg '(:agr :number)) :plur)
                   (not (= (fs/get-in arg '(:irregular :fem :plur) :notfound) :notfound)))
              (do
                (log/info "it:16 +fem +plur +(adj|noun)")
                (fs/get-in arg '(:irregular :fem :plur)))
              
              ;; masculine adjective agreement: plural
              (and (map? arg)
                   (or (= category :adjective)
                       (= (fs/get-in arg '(:cat)) :adjective))
                   (contains? arg :italian)
                   (contains? arg :agr)
                   (= (fs/get-in arg '(:agr :gender)) :masc)
                   (= (fs/get-in arg '(:agr :number)) :plur))
              (do
                (log/info "it:17 +masc +plur +adj")
                (string/replace (fs/get-in arg '(:italian))
                                #"[eo]$" "i")) ;; adjectives ending in "e" or "o" pluralize to "i"
              
              ;; feminine adjective agreement: singular
              (and (map? arg)
                   (or (= category :adjective)
                       (= (fs/get-in arg '(:cat)) :adjective)
                       (= category :noun)
                       (= (fs/get-in arg '(:cat)) :noun))
                   (contains? arg :italian)
                   (contains? arg :agr)
                   (= (fs/get-in arg '(:agr :gender)) :fem)
                   (= (fs/get-in arg '(:agr :number)) :sing))
              (do
                (log/info "it:18 +fem +sing +(adj|noun) +regular")
                (string/replace (fs/get-in arg '(:italian))
                                #"o$" "a"))
              
              ;; feminine adjective agreement: plural with adjective ending in "e": change to "i".
              (and (map? arg)
                   (or (= category :adjective)
                       (= (fs/get-in arg '(:cat)) :adjective))
                   (contains? arg :italian)
                   (contains? arg :agr)
                   (= (fs/get-in arg '(:agr :gender)) :fem)
                   (= (fs/get-in arg '(:agr :number)) :plur)
                   (= (final-char-of (fs/get-in arg '(:italian))) "e"))
              (do
                (log/info "it:19 +fem +plur +adj")
                (string/replace (fs/get-in arg '(:italian))
                                #"e$" "i"))
              
              ;; feminine adjective agreement: plural with adjective ending in "o": change to "e".
              (and (map? arg)
                   (or (= category :adjective)
                       (= (fs/get-in arg '(:cat)) :adjective)
                       (= category :noun)
                       (= (fs/get-in arg '(:cat)) :noun))
                   (contains? arg :italian)
                   (contains? arg :agr)
                   (= (fs/get-in arg '(:agr :gender)) :fem)
                   (= (fs/get-in arg '(:agr :number)) :plur))
              (do
                (log/info "it:20 +(adj|noun) +fem +plur")
                (string/replace (fs/get-in arg '(:italian))
                                #"o$" "e"))
              
              
              ;; feminine noun or adjective: underspecified number.
              (and (map? arg)
                   (contains? arg :agr)
                   (or (= category :adjective)
                       (= category :noun))
                   (= (fs/get-in arg '(:agr :gender)) :fem))
              (do
                (log/info "it:21 +(:adj|:noun) +fem UNDERSPEC1")
                arg)
              
              ;; masculine adjective agreement: singular
              (and (map? arg)
                   (= category :adjective)
                   (contains? arg :italian)
                   (contains? arg :agr)
                   (= (fs/get-in arg '(:agr :gender)) :masc)
                   (= (fs/get-in arg '(:agr :number)) :sing))
              (do
                (log/info "it:22 +:adj(cat) +masc +sing")
                (fs/get-in arg '(:italian)))
              
              ;; masculine adjective agreement: plural
              (and (map? arg)
                   (= category :adjective)
                   (contains? arg :italian)
                   (contains? arg :agr)
                   (= (fs/get-in arg '(:agr :gender)) :masc)
                   (= (fs/get-in arg '(:agr :number)) :plur))
              (do
                (log/info "it:23 +italian +agr +masc +plur")
                (string/replace (fs/get-in arg '(:italian))
                                #"o$" "i"))
              
              
              ;; masculine noun or adjective: underspecified number.
              (and (map? arg)
                   (contains? arg :agr)
                   (or (= category :adjective)
                       (= category :noun))
                   (= (fs/get-in arg '(:agr :gender)) :masc))
              (do
                (log/info "it:24 +agr +(adj|noun) +masc")
                arg)
              
              
              ;; irregular passato prossimo: essere case: agrees with number and gender, but number
              ;; (and likely gender, though we don't currently check) are not specified.
              (and (map? arg)
                   (contains? arg :infl)
                   (= (fs/get-in arg '(:infl)) :past)
                   (not (= (fs/get-in arg '(:infinitive :irregular :passato) :notfound)
                           :notfound))
                   (= (fs/get-in arg '(:infinitive :essere)) true)
                   (= :notfound (fs/get-in arg '(:agr :number) :notfound)))
              arg
              
              ;; irregular passato prossimo: essere case: agrees with number and gender.
              (and (map? arg)
                   (contains? arg :infl)
                   (= (fs/get-in arg '(:infl)) :past)
                   (not (= (fs/get-in arg '(:infinitive :irregular :passato) :notfound)
                           :notfound))
                   (= (fs/get-in arg '(:infinitive :essere)) true))
              ;; TODO: handle number and gender agreement for essere-true passato forms.
              (let [canonical (fs/get-in arg '(:infinitive :irregular :passato))
                    but-last (string/join "" (butlast canonical))
                    suffix (cond
                            (and (= (fs/get-in arg '(:agr :gender)) :fem)
                                 (= (fs/get-in arg '(:agr :number)) :sing)
                                 (= (fs/get-in arg '(:infinitive :essere)) true))
                            "a"
                            
                            (and (= (fs/get-in arg '(:agr :gender)) :fem)
                                 (= (fs/get-in arg '(:agr :number)) :plur)
                                 (= (fs/get-in arg '(:infinitive :essere)) true))
                            "e"
                            
                            (and (= (fs/get-in arg '(:agr :number)) :plur)
                                 (= (fs/get-in arg '(:infinitive :essere)) true))
                            "i"
                            
                            true
                            "o")]
                (str but-last suffix))
              ;; irregular passato prossimo: non-essere case.
              (and (map? arg)
                   (contains? arg :infl)
                   (= (fs/get-in arg '(:infl)) :past)
                   (not (= (fs/get-in arg '(:infinitive :irregular :passato) :notfound)
                           :notfound)))
              (do
                (str (fs/get-in arg '(:infinitive :irregular :passato))))
              
              (and (map? arg)
                   false
                   (= :past (fs/get-in arg '(:infl)))
                   (map? (fs/get-in arg '(:infinitive))))
              (do
                (throw (Exception. (str "Argument is a verb: " arg " with infinitive: "
                                        (fs/get-in arg '(:infinitive)) ","
                                        "but morph/conjugate-it doesn't know how to conjugate"
                                        "it. Needs more lexical information or morphological code to do so."))))
              
              (and (map? arg)
                   (contains? arg :infl)
                   (= (fs/get-in arg '(:infl)) :past)
                   (not (= (fs/get-in arg '(:agr :number) :not-found) :not-found)))
              ;; regular past inflection
              (let [root (fs/get-in arg '(:infinitive))
                    root (if (map? root)
                           (fs/get-in root '(:infinitive))
                           root)
                    root (if root root (str "(no-root-found: in" arg))
                    are-type (try (re-find #"are$" root)
                                  (catch Exception e
                                    (throw (Exception. (str "Can't regex-find on non-string: " root)))))
                    ere-type (re-find #"ere$" root)
                    ire-type (re-find #"ire$" root)
                    stem (string/replace root #"[iae]re$" "")
                    last-stem-char-is-i (re-find #"i$" stem)
                    suffix (cond
                            (and (= (fs/get-in arg '(:agr :gender)) :fem)
                                 (= (fs/get-in arg '(:agr :number)) :sing)
                                 (= (fs/get-in arg '(:infinitive :essere)) true))
                            "a"
                            
                            (and (= (fs/get-in arg '(:agr :gender)) :fem)
                                 (= (fs/get-in arg '(:agr :number)) :plur)
                                 (= (fs/get-in arg '(:infinitive :essere)) true))
                            "e"
                            
                            (and (= (fs/get-in arg '(:agr :number)) :plur)
                                 (= (fs/get-in arg '(:infinitive :essere)) true))
                            "i"
                            
                            true
                            "o")
                    
                    ]
                (log/info "regular past inflection")
                (cond
                 (or are-type ere-type)
                 (str stem "at" suffix) ;; "ato" or "ati"
                 (or are-type ire-type)
                 (str stem "it" suffix) ;; "ito" or "iti"
                 true
                 (str "(regpast:TODO):" stem)))
              
              ;; not used yet...
              (= (fs/get-in arg '(:infl)) :infinitive)
              (do
                (log/info "GOT HERE.")
                (fs/get-in arg '(:infinitive)))
              
              ;; .. used instead.
              (and (= :notfound (fs/get-in arg '(:infl) :notfound))
                   (= :notfound (fs/get-in arg '(:agr) :notfound))
                   (string? (fs/get-in arg '(:infinitive)))
                   (not (= infl :present)))
              (do
                (log/info "GOT HERE(2)")
                (fs/get-in arg '(:infinitive)))
              
              (= (fs/get-in arg '(:infl)) :futuro)
              (do
                (log/info "futuro")
                (log/info (str "arg: " arg))
                (let [root (fs/get-in arg '(:infinitive))
                      root (if (map? root)
                             (fs/get-in arg '(:infinitive :infinitive))
                             (if (nil? root) "" root))
                      person (fs/get-in arg '(:agr :person))
                      number (fs/get-in arg '(:agr :number))
                      stem (string/replace root #"e$" "")]
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
                   arg)))
              
              (or (= category :noun)
                  (= (fs/get-in arg '(:cat)) :noun)
                  (= category :adjective)
                  (= (fs/get-in arg '(:cat)) :adjective))
              arg
              
              (nil? (fs/get-in arg '(:infinitive)))
              arg
              
              :else
              ;; assume present tense verb with map with keys (:root and :agr).
              (let [root (fs/get-in arg '(:infinitive))
                    root (if (not (= (type root) java.lang.String))
                           (fs/get-in arg '(:infinitive :infinitive))
                           root)
                    root-check (if (nil? root)
                                 (do
                                   (log/error (str "Expected to find :infinitive in: " arg " with category: " category))
                                   (throw (Exception. (str "Expected to find :infinitive in: " arg "."))))
                                 root)
                    
                    person (fs/get-in arg '(:agr :person))
                    number (fs/get-in arg '(:agr :number))
                    stem (string/replace root #"[iae]re$" "")
                    are-type (re-find #"are$" root)
                    ere-type (re-find #"ere$" root)
                    ire-type (re-find #"ire$" root)
                    last-stem-char-is-i (re-find #"i$" stem)]
                (log/info "it: :else")
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
                 arg)))]
    (log/info (str "<= conjugate-it: " retval))
    retval))

(defn conjugate-en [arg category & [ infl ] ]
  (log/debug (str "==="))
  (log/debug (str "arg: " arg))
  (log/debug (str "cat: " category))
  (log/debug (str "infl: " (fs/get-in arg '(:infl))))
  (log/debug (str "agreement: " (fs/get-in arg '(:agr))))
  (log/debug (str "futuro? " (= (fs/get-in arg '(:infl)) :futuro)))
  (log/debug (str ":cat of arg:" (fs/get-in arg '(:cat))))

  (cond (nil? arg) ""
        (= (type arg) java.lang.String)
        arg
        (and (map? arg)
             (contains? (set (keys arg)) :1)
             (contains? (set (keys arg)) :2))
        (let [result1 (conjugate-en (:1 arg) category)
              result2 (conjugate-en (:2 arg) category)]
          (if (and (= (type result1) java.lang.String)
                   (= (type result2) java.lang.String))
            (string/join " " (list result1 result2))
            {:1 result1
             :2 result2}))

        (and (map? arg)
             (contains? (set (keys arg)) :agr)
             (contains? (set (keys arg)) :infinitive)
             (not (= :notfound (fs/get-in arg '(:infinitive :irregular :present) :notfound)))
             ;; TODO: check (= :present) rather than (not (= :past)) and (not (= :futuro)).
             (not (= :past (fs/get-in arg '(:infl) :notfound)))
             (not (= :futuro (fs/get-in arg '(:infl) :notfound)))
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
        (throw (Exception. (str "cannot conjugate: " arg)))

        ;; <deprecated: root>
        ;; irregular noun: plural
        (and (map? arg)
             (not (= :notfound (fs/get-in arg '(:root :irregular :plur) :notfound)))
             (= (fs/get-in arg '(:agr :number)) :plur))
        (do (log/debug "en: +irreg +plur")
            (fs/get-in arg '(:root :irregular :plur)))

        ;; irregular noun: singular
        (and (map? arg)
             (not (= :notfound (fs/get-in arg '(:root :irregular :plur) :notfound)))
             (= (fs/get-in arg '(:agr :number)) :sing))
        (do (log/debug "en: +root +sing")
            (fs/get-in arg '(:root :english)))

        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :number)) :sing))
        (do
          (log/debug "en: +root +sing")
          (str (fs/get-in arg '(:root))))

        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :number)) :plur))
        (do
          (log/debug "en: +root +agr +plur")
          (str (fs/get-in arg '(:root)) "s"))
        ;; </deprecated: root>

        ;; <use instead: english (below)>

        ;; irregular noun: plural
        (and (map? arg)
             (not (= :notfound (fs/get-in arg '(:english :irregular :plur) :notfound)))
             (= (fs/get-in arg '(:agr :number)) :plur))
        (do
          (log/debug "en: +plur +irreg")
          (fs/get-in arg '(:english :irregular :plur)))

        ;; irregular noun: singular
        (and (map? arg)
             (not (= :notfound (fs/get-in arg '(:english :irregular :plur) :notfound)))
             (= (fs/get-in arg '(:agr :number)) :sing))
        (do
          (log/debug "en: +sing +irreg")
          (fs/get-in arg '(:english :english)))

        ;; english (might have a note: plural)
        (and (map? arg)
             (contains? arg :english)
             (map? (fs/get-in arg '(:english)))
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :number)) :plur))
        (do
          (log/debug "+:plur +map(:english) +note")
          (str (fs/get-in arg '(:english :english)) "s" (fs/get-in arg '(:english :note))))

        ;; english with a note: singular (default)
        (and (map? arg)
             (contains? arg :english)
             (map? (fs/get-in arg '(:english)))
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :number)) :sing))
        (do
          (log/debug "en: +sing +map(:english) +(arg,:agr)")
          (str (fs/get-in arg '(:english :english)) (fs/get-in arg '(:english :note))))
        
        (and (map? arg)
             (contains? arg :english)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :number)) :sing))
        (do
          (log/debug "en: +sing +map(:english) +(arg,:agr)")
          (str (fs/get-in arg '(:english))))

        ;; </use instead>
        
        ;; TODO: replace :root with :english everwhere.
        ;; (just as we are doing here).
        ;; this rule currently does not function (because
        ;; nouns use :root still (they need to be ported to the new
        ;; :agr/:cat representation).
        (and (map? arg)
             (contains? arg :english)
             (contains? arg :agr)
             (= category :noun)
             (= (fs/get-in arg '(:agr :number)) :plur))
        (do
          (log/debug "+plur +english +agr +category(:noun)")
          (str (fs/get-in arg '(:english)) "s"))

        (and (map? arg)
             (contains? arg :english)
             (contains? arg :agr)
             (= category :adjective))
        (do
          (log/debug "+adj +english +:agr")
          (fs/get-in arg '(:english)))

        (and (map? arg)
             (contains? arg :english)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :number)) :sing))
        (do
          (log/debug "+english +agr +sing")
          (fs/get-in arg '(:english)))

        ;; number not specified or underspecified: keep unspecified.
        (and (map? arg)
             (contains? arg :english)
             (contains? arg :agr))
        (do (log/debug "unspec1")
            arg)

        ;; noun or verb: number not specified: use root form by default.
        (and (map? arg)
             (contains? arg :root)
             (contains? arg :agr)
             (= (fs/get-in arg '(:agr :number)) :sing))
        (do (log/debug "unspec2")
            (str (fs/get-in arg '(:root))))

        (and (map? arg)
             (contains? arg :english)
             (contains? arg :agr))
        (do (log/debug "unspec3")
            (str (fs/get-in arg '(:english))))

        ;; irregular past: agreement information specified.
        (and (map? arg)
             (contains? arg :infl)
             (= (fs/get-in arg '(:infl)) :past)
             (not (= (fs/get-in arg '(:infinitive :irregular :past) :notfound)
                     :notfound)))
        (do (log/debug "en: +past +irreg +continuing")
            (cond

             (and (= (fs/get-in arg '(:agr :number)) :sing)
                  (= (fs/get-in arg '(:agr :person)) :1st)
                  (not (= (fs/get-in arg '(:infinitive :irregular :past :1sing) :notfound) :notfound)))
             (do (log/debug "en: +past +irreg +1sing")
                 (str (fs/get-in arg '(:infinitive :irregular :past :1sing))))

             (and (= (fs/get-in arg '(:agr :number)) :sing)
                  (= (fs/get-in arg '(:agr :person)) :2nd)
                  (not (= (fs/get-in arg '(:infinitive :irregular :past :2sing) :notfound) :notfound)))
             (do (log/debug "en: +past +irreg +2sing")
                 (str (fs/get-in arg '(:infinitive :irregular :past :2sing))))

             (and (= (fs/get-in arg '(:agr :number)) :sing)
                  (= (fs/get-in arg '(:agr :person)) :3rd)
                  (not (= (fs/get-in arg '(:infinitive :irregular :past :3sing) :notfound) :notfound)))
             (do (log/debug "en: +past +irreg +3sing")
                 (str (fs/get-in arg '(:infinitive :irregular :past :3sing))))

             (and (= (fs/get-in arg '(:agr :number)) :plur)
                  (= (fs/get-in arg '(:agr :person)) :1st)
                  (not (= (fs/get-in arg '(:infinitive :irregular :past :1plur) :notfound) :notfound)))
             (do (log/debug "en: +past +irreg +1plur")
                 (str (fs/get-in arg '(:infinitive :irregular :past :1plur))))

             (and (= (fs/get-in arg '(:agr :number)) :plur)
                  (= (fs/get-in arg '(:agr :person)) :2nd)
                  (not (= (fs/get-in arg '(:infinitive :irregular :past :2plur) :notfound) :notfound)))
             (do (log/debug "en: +past +irreg +2plur")
                 (str (fs/get-in arg '(:infinitive :irregular :past :2plur))))

             (and (= (fs/get-in arg '(:agr :number)) :plur)
                  (= (fs/get-in arg '(:agr :person)) :3rd)
                  (not (= (fs/get-in arg '(:infinitive :irregular :past :3plur) :notfound) :notfound)))
             (do (log/debug "en: +past +irreg +3plur")
                 (str (fs/get-in arg '(:infinitive :irregular :past :3plur))))

             (string? (fs/get-in arg '(:infinitive :irregular :past)))
             (do (log/debug "en: +past +irreg +general")
                 (fs/get-in arg '(:infinitive :irregular :past)))
             true
             (do (log/debug "en: +past +irreg +unspec")
                 arg)))


        (and (map? arg)
             (contains? arg :infl)
             (= (fs/get-in arg '(:infl)) :past))
        (let [root (fs/get-in arg '(:infinitive))
              ;; TODO: throw exception rather than encoding error as part
              ;; of the english string (i.e. no 'nilrootz')
              root (if (nil? root) "(nilrootz)" root)
              root (if (not (= (type root) java.lang.String))
                     (fs/get-in arg '(:infinitive :infinitive))
                     root)
              stem (string/replace root #"^to " "")
              stem-minus-one (nth (re-find #"(.*).$" stem) 1)
              penultimate-stem-char (nth (re-find #"(.).$" stem) 1)
              last-stem-char (re-find #".$" stem)
              last-stem-char-is-e (re-find #"e$" stem)]
          (log/debug "+past +infl")
          (cond last-stem-char-is-e  ;; e.g. "write" => "written"
                (str stem-minus-one penultimate-stem-char "en")
                true
                (str stem "en")))

        (= (fs/get-in arg '(:infl)) :futuro)
        (let [root (fs/get-in arg '(:infinitive))
              root (if (map? root)
                     (fs/get-in root '(:infinitive))
                     (if (nil? root) ""
                         root))
              stem (string/replace root #"^to " "")]
          (log/debug "+futuro")
          (str "will " stem))

        ;; not used yet...
        (= (fs/get-in arg '(:infl)) :infinitive)
        (fs/get-in arg '(:infinitive))

        ;; .. used instead.
        (and (= :notfound (fs/get-in arg '(:infl) :notfound))
             (= :notfound (fs/get-in arg '(:agr) :notfound))
             (string? (fs/get-in arg '(:infinitive))))
        (str (fs/get-in arg '(:infinitive)))
        
        :else
        ;; assume a map with keys (:infinitive and :agr), and conjugate present tense.
        (let [root (fs/get-in arg '(:infinitive))
              ;; TODO: throw exception rather than encoding error "nilrootz" as part
              ;; of the english string.
              root (if (nil? root) "(nilrootz)" root)
              root (if (not (= (type root) java.lang.String))
                      (fs/get-in arg '(:infinitive :infinitive))
                      root)
              person (fs/get-in arg '(:agr :person))
              number (fs/get-in arg '(:agr :number))
              stem (string/replace root #"^to " "")
              last-stem-char-is-e (re-find #"e$" stem)
              last-stem-char-is-vowel (re-find #"[aeiou]$" stem)]
          (log/debug "+else")
          (log/debug (str "(english):arg: " arg))
          (cond

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
           :else arg))))


(declare get-italian-stub)

(defn get-italian-stub-1 [word]
  (cond

   (and
    (= (fs/get-in word '(:agr :gender)) :fem)
    (= (fs/get-in word '(:agr :number)) :plur)
    (= (fs/get-in word '(:cat)) :adjective))
   (string/replace (fs/get-in word '(:italian))
                   #"[eo]$" "e") ;; nero => nere
   
   (and
    (fs/get-in word '(:a))
    (fs/get-in word '(:b)))
   (get-italian-stub 
    (fs/get-in word '(:a))
    (fs/get-in word '(:b)))

   (and
    (= (fs/get-in word '(:agr :gender)) :masc)
    (= (fs/get-in word '(:agr :number)) :sing)
    (= (fs/get-in word '(:cat)) :noun)
    (fs/get-in word '(:root)))
   (fs/get-in word '(:root))

   ;; handle lexical exceptions:
   (and
    (= (fs/get-in word '(:agr :gender)) :masc)
    (= (fs/get-in word '(:agr :number)) :plur)
    (= (fs/get-in word '(:cat)) :noun)
    (string? (fs/get-in word '(:irregular :plur))))
   (fs/get-in word '(:irregular :plur))
   
   
   (and
    (= (fs/get-in word '(:agr :gender)) :masc)
    (= (fs/get-in word '(:agr :number)) :plur)
    (= (fs/get-in word '(:cat) :noun))
    (fs/get-in word '(:italian)))
   (string/replace (fs/get-in word '(:italian))
                   #"[eo]$" "i") ;; dottore => dottori; medico => medici

   ;; deprecated: remove support for :root.
   (and
    (= (fs/get-in word '(:agr :gender)) :masc)
    (= (fs/get-in word '(:agr :number)) :plur)
    (= (fs/get-in word '(:cat) :noun))
    (fs/get-in word '(:root)))
   (string/replace (fs/get-in word '(:root))
                   #"[eo]$" "i") ;; dottore => dottori; medico => medici
   

   (and
    (= (fs/get-in word '(:agr :gender)) :fem)
    (= (fs/get-in word '(:agr :number)) :plur)
    (= (fs/get-in word '(:cat) :noun))
    (fs/get-in word '(:italian)))
   (string/replace (fs/get-in word '(:italian))
                   #"[a]$" "e") ;; donna => donne

   (and
    (= (fs/get-in word '(:agr :gender)) :fem)
    (= (fs/get-in word '(:agr :number)) :plur)
    (= (fs/get-in word '(:cat)) :noun)
    (fs/get-in word '(:root)))
   (string/replace (fs/get-in word '(:root))
                   #"[a]$" "e") ;; donna => donne


   ;; deprecated: remove support for :root.
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
    (= (fs/get-in word '(:agr :gender)) :masc)
    (= (fs/get-in word '(:agr :number)) :plur)
    (= (fs/get-in word '(:cat)) :adjective))
   (string/replace (fs/get-in word '(:italian))
                   #"[eo]$" "i") ;; nero => neri

   (and
    (= (fs/get-in word '(:agr :gender)) :fem)
    (= (fs/get-in word '(:agr :number)) :sing)
    (= (fs/get-in word '(:cat)) :adjective))
   (string/replace (fs/get-in word '(:italian))
                   #"[eo]$" "a") ;; nero => nera


   (and
    (= (fs/get-in word '(:agr :gender)) :fem)
    (= (fs/get-in word '(:agr :number)) :plur)
    (= (fs/get-in word '(:cat)) :adjective)
    ;; handle lexical exceptions.
    (string? (fs/get-in word '(:irregular :fem :plur))))
   (fs/get-in word '(:irregular :fem :plur))

   :else
  word))

(defn get-italian-stub [a b]
  (let [a (get-italian-stub-1 a)
        b (get-italian-stub-1 b)]
    (cond

     (and (= a "di i")
          (string? b))
     (str "dei " b)
     (and (= a "di il")
          (string? b))
     (str "del " b)
     (and (= a "di la")
          (string? b))
     (str "della " b)
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

     (and (= a "il")
          (string? b)
          (re-find #"^[aeiou]" b))
     (str "l'" b)

     
     (and (string? a) (string? b))
     (str a " " b)

     (and (string? a) (string? (fs/get-in b '(:italian))))
     (str a " " (fs/get-in b '(:italian)))

     (and (string? (fs/get-in a '(:italian)))
          (string? b))
     (str (fs/get-in a '(:italian)) " " b)
     
     true
     {:a a
      :b b})))

(declare get-english-stub)

(defn get-english-stub-1 [word]
  (cond

   (and
    (fs/get-in word '(:a))
    (fs/get-in word '(:b)))
   (get-english-stub 
    (fs/get-in word '(:a))
    (fs/get-in word '(:b)))
   
   (and
    (fs/get-in word '(:root :irregular :plur))
    (= (fs/get-in word '(:agr :number)) :plur)
    (= (fs/get-in word '(:cat) :noun)))
   (fs/get-in word '(:root :irregular :plur))
   
   ;; TODO: remove support for deprecated :root - use :english instead.
   (and
    (= (fs/get-in word '(:agr :number)) :sing)
    (= (fs/get-in word '(:cat) :noun))
    (string? (fs/get-in word '(:root))))
   (fs/get-in word '(:root))
   (and
    (= (fs/get-in word '(:agr :number)) :sing)
    (= (fs/get-in word '(:cat) :noun))
    (string? (fs/get-in word '(:english))))
   (fs/get-in word '(:english))

   ;; TODO: remove support for deprecated :root - use :english instead.
   (and
    (= (fs/get-in word '(:agr :number)) :sing)
    (= (fs/get-in word '(:cat) :noun))
    (string? (fs/get-in word '(:root :english))))
   (fs/get-in word '(:root :english))
   (and
    (= (fs/get-in word '(:agr :number)) :sing)
    (= (fs/get-in word '(:cat) :noun))
    (string? (fs/get-in word '(:english :english))))
   (fs/get-in word '(:english :english))

   ;; TODO: remove support for deprecated :root - use :english instead.
   (and (= (fs/get-in word '(:agr :number)) :plur)
        (= (fs/get-in word '(:cat)) :noun)
        (string? (fs/get-in word '(:root))))
   (str (fs/get-in word '(:root)) "s")
   (and (= (fs/get-in word '(:agr :number)) :plur)
        (= (fs/get-in word '(:cat)) :noun)
        (string? (fs/get-in word '(:english))))
   (str (fs/get-in word '(:english)) "s")
   (and (= (fs/get-in word '(:agr :number)) :plur)
        (= (fs/get-in word '(:cat)) :noun)
        (string? (fs/get-in word '(:english :english))))
   (str (fs/get-in word '(:english :english))
        "s"
        (if (fs/get-in word '(:english :note))
          (str (fs/get-in word '(:english :note)))))


   (and (= (fs/get-in word '(:cat)) :adjective)
        (string? (fs/get-in word '(:english))))
   (fs/get-in word '(:english))

   (map? word)
   (merge {:morphology-is-done false}
           word)
   
   :else
  word))

(defn get-english-stub [a b]
  (let [re-a (get-english-stub-1 a)
        re-b (get-english-stub-1 b)]
    (cond
     
     (and (string? re-a) (string? re-b)
          (= (fs/get-in a '(:cat)) :noun)
          (= (fs/get-in b '(:cat)) :adjective))
     ;; If a is a noun, and b is a adj, reverse a and b in string,
     ;;  so that italian word order is reversed to english word order.
     (str re-b " " re-a)
          
     (and (string? re-a) (string? re-b)
          (= re-a "a")
          (re-find #"^[aeiou]" re-b))
     (str "an " re-b)

     (and (string? re-a) (string? re-b))
     (str re-a " " re-b)

     :else
     {:a a
      :b b})))

(defn get-italian [a b & [ a-category b-category a-infl b-infl]]
  (log/info (str "<get-italian>"))
  (log/info (str "get-italian: a : " a))
  (log/info (str "get-italian: b : " b))
  (log/debug (str "get-italian: cat-a : " a-category))
  (log/debug (str "get-italian: cat-b : " b-category))
  (log/info (str "get-italian: infl-a : " a-infl))
  (log/info (str "get-italian: infl-b : " b-infl))
  (let [conjugated-a (conjugate-it a a-category a-infl)
        conjugated-b (if (not (nil? b)) (conjugate-it b b-category b-infl) "..")]
    (log/info (str "conjugated-a: " conjugated-a))
    (log/info (str "conjugated-b: " conjugated-b))
    (if (and
         (string? conjugated-a)
         (string? conjugated-b))
      (string/trim
       (cond

        ;; TODO: use features of a and b (e.g. gender, number)
        ;; rather than string comparison.
        ;; 1. definite article + noun
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

        ;; 2. partitive article + noun
        (and (= conjugated-a "di il")
             (re-find #"^s[t]" conjugated-b))
        (str "dello " conjugated-b)

        (and (= conjugated-a "di i")
             (re-find #"^s[t]" conjugated-b))
        (str "degli " conjugated-b)

        (and (= conjugated-a "di i")
             (re-find #"^[aeiou]" conjugated-b))
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


        ;; 3. prepositions
        (and (= conjugated-a "a")
             (re-find #"^il " conjugated-b))
        (str "al " (string/replace conjugated-b #"^il " ""))
        
        true
        (string/trim (str conjugated-a " " conjugated-b))))
      {:1 conjugated-a
       :2 conjugated-b})))

(defn get-english [a b & [ a-category b-category a-infl b-infl]]
  "Take two constituents and combine them.
   The two category params may reverse word order
     (e.g.: get-english 'cat' 'black' 'noun' 'adj' => 'black cat')"

  (log/debug (str "get-english: a : " a))
  (log/debug (str "get-english: b : " b))
  (log/debug (str "get-english: cat-a : " a-category))
  (log/debug (str "get-english: cat-b : " b-category))

  ;; TODO: sometimes ".." is being added necessarily (e.g.
  ;; (formattare (over s1 pronouns (over vp-present present-aux-verbs (over vp-past "andare"))))))
  ;; )
  (let [conjugated-a (conjugate-en a a-category)
        conjugated-b (if (not (nil? b)) (conjugate-en b b-category) "..")]
    (log/debug (str "conjugated-a: " conjugated-a))
    (log/debug (str "conjugated-b: " conjugated-b))
    (cond
     ;; "<to be> + past participle => past participle (e.g. "is went" => "went")
     (and (= a-category :verb)
          (= b-category :verb)
          (map? a)
          (= (fs/get-in a '(:infinitive :infinitive)) "to be"))
     conjugated-b
     
     ;; in english, order of noun + adjective is reversed: (adjective + noun)
     (and
      (= (type conjugated-a) java.lang.String)
      (= (type conjugated-b) java.lang.String)
      (= a-category :noun)
      (= b-category :adjective))
     (string/trim (str conjugated-b " " conjugated-a))

     (and
      (= (type conjugated-a) java.lang.String)
      (= (type conjugated-b) java.lang.String))
     (string/trim (str conjugated-a " " conjugated-b))

     ;; in english, order of noun + adjective is reversed: (adjective + noun)
     (and
      (= a-category :noun)
      (= b-category :adjective))
     {:1 conjugated-b
      :2 conjugated-a}

     ;; "can" + "to X" => "can X"
     (= (fs/get-in conjugated-a '(:infinitive :infinitive)) "to be able")
     {:1 conjugated-a
      :2 (string/replace conjugated-b #"^to " "")}

     true
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

(defn if-isco [verb]
  (if (= (get verb :isco) true)
    "isc"
    ""))

(defn conjugate-present-italian-verb-regular [verb-head subject-head]
  (let [root-form (fs/get-in-r verb-head '(:italian))
        regex #"^([^ ]*)([aei])re[ ]*$"]
    (log/debug (str "conjugate-italian-verb-regular: " verb-head "," subject-head))
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

(defn conjugate-futuro [verb-head subject-head]
  (let [root-form (fs/get-in-r verb-head '(:italian))
        regex #"^([^ ]*)([aei])re[ ]*$"]
    (log/debug (str "conjugate-italian-verb-regular: " verb-head "," subject-head))
    (println (str "conjugate-italian-verb-regular: " verb-head "," subject-head))
    (cond

     (and (or (= (fs/get-in-r subject-head '(:person)) "1st")
              (= (fs/get-in-r subject-head '(:person)) :1st))
          (or (= (fs/get-in-r subject-head '(:number)) "singular")
              (= (fs/get-in-r subject-head '(:number)) :singular)))
     (string/replace root-form regex
                        (fn [[_ stem vowel space]] (str stem
                                                        (if-isco verb-head)
                                                        "ò"
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
  (log/debug (str "CONJUGATE-ITALIAN-VERB"))
  (cond
   (or (= (get verb-phrase :infl) :passato-prossimo)
       (= (get verb-phrase :infl) "passato-prossimo"))
   (conjugate-passato-prossimo verb-phrase subject)

   (= (get verb-phrase :infl) :futuro)
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
                       (conjugate-futuro
                        (fs/get-head verb-phrase) subject)
                       except-first)))))

   true ;; assume present
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
                       (conjugate-present-italian-verb-regular
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

