;; RESTARTING OF RING REQUIRED FOR CHANGES TO THIS FILE. (purtroppo)
(ns italianverbs.generate
  (:use [clojure.stacktrace]
        [italianverbs.rdutest]
        [italianverbs.lexiconfn])
  (:require
   [italianverbs.lev :as lev]
   [italianverbs.morphology :as morph]
   [italianverbs.grammar :as gram]
   [italianverbs.fs :as fs]
   [italianverbs.config :as config]
   [italianverbs.html :as html]
   [italianverbs.lexicon :as lex]
   [italianverbs.search :as search]
   [clojure.string :as string]
   [clojure.contrib.duck-streams :as duck]))

;(defn print [fs]
;  "print a feature structure to a file. filename will be something easy to derive from the fs."
;  (let [filename "foo.html"]  ;; TODO: some conventional default if deriving from fs is too hard.
;    (duck/spit filename (html/static-page (html/tablize fs)))))

(defn random-lexeme [& constraints]
  (let [lexemes (seq
                 (apply search/query constraints))]
    (if lexemes
      (if (> (.size lexemes) 0)
        (nth lexemes (rand-int (.size lexemes)))))))

(defn random-symbol [& symbols]
  (let [symbols (apply list symbols)]
    (nth symbols (rand-int (.size symbols)))))

;; TODO: learn why string/join doesn't work for me:
;; (string/join '("foo" "bar") " ")
;; => " " (should be: "foo bar").
(defn join [coll separator]
  (apply str (interpose separator coll)))

(defn conjugate-pp [preposition np]
  {:english (join (list (:english preposition) (:english np)) " ")
   :italian (morph/conjugate-italian-prep preposition np)
   :head preposition
   :comp np})

(defn conjugate-italian-prep [preposition np]
  (morph/conjugate-italian-prep preposition np))

(defn conjugate-english-verb [vp subject]
  ;; TODO: lookup irregular form (if any) using vp and subject.
  (morph/conjugate-english-verb vp subject))

(defn conjugate-italian-verb [vp subject]
  ;; TODO: lookup irregular form (if any) using vp and subject.
  (morph/conjugate-italian-verb vp subject))

;; TODO : factor out commonalities between random-present and random-passato-prossimo.
(defn random-present-old []
  (let [;; choose a random verb in the infinitive form.
        verb-inf (gram/choose-lexeme
                  (fs/merge {:cat :verb
                             :infl :infinitive}
                            config/random-present-inf))


        verb-present-constraints
        (fs/merge 
         {:root.cat :verb :infl :present}
         {:root.italian (get verb-inf :italian)})

        verb-present
        (let [lexical
              (gram/choose-lexeme verb-present-constraints)]
          (if (not (= (get lexical :cat) :error))
            lexical
              ;; else, use a pronoun as a source of a random person/number pair,
              ;; and generate a present verb.
            (let [pronoun (gram/choose-lexeme
                           (fs/merge
                            {:pronoun true}
                            config/random-present-subj))
                  number (get pronoun :number)
                  person (get pronoun :person)]
              {:italian (conjugate-italian-verb verb-inf
                                                {:person person
                                                 :number number})
                                                
               :english (conjugate-english-verb verb-inf
                                                {:person person
                                                 :number number}
                                                {:infl :present})
                                               
               :root verb-inf
               :person person
               :number number})))

;        obj (gram/np
;             {:case {:$ne :nom}})
        vp (gram/vp verb-inf)
        subj-constraints
        (fs/merge
         {:cat :noun
          :case {:$ne :acc}}
         (get (get verb-present :root) :subj)
         (get verb-inf :subj)
         {:person (get verb-present :person)
          :number (get verb-present :number)})
        subject (cond
                 (or (= (get verb-present :person) "1st")
                     (= (get verb-present :person) "2nd"))
                 (gram/choose-lexeme subj-constraints)
                 true
                 (gram/np subj-constraints))]
    (fs/merge
     {:verb-inf verb-inf
      :verb-present verb-present
      :subject subject
;      :object obj
;      :vp vp
      :verb-constraints verb-present-constraints
      :subj-constraints subj-constraints
      :english (str (get subject :english) " "
                    (if (get verb-present :english)
                      (get verb-present :english)
                      (conjugate-english-verb verb-inf subject {:infl :present})))
      :italian (str (get subject :italian) " " (get verb-present :italian))}
     {:type-is-fs (set '(:vp :object :verb-present :subject :verb-inf :subj-constraints :verb-constraints))})))

(defn random-infinitivo []
  (gram/choose-lexeme
   (fs/merge {:cat :verb
           :infl :infinitive}
          config/random-infinitivo)))

(defn espressioni []
  (gram/choose-lexeme {:cat :espressioni}))


(defn random-futuro-semplice [& constraints]
  (let [
        ;; 1. choose a random verb in the passato-prossimo form.
        verb-future (gram/choose-lexeme
                     (fs/merge
                      (if constraints (first constraints) {})
                      {:infl :futuro-semplice}
                      config/futuro-semplice))]
    verb-future))

(defn random-passato-prossimo []
  (let [
        ;; 1. choose a random verb in the passato-prossimo form.
        verb-past (gram/choose-lexeme
                   (fs/merge
                    {:root.cat :verb :infl :passato-prossimo}
                    config/random-passato-prossimo-verb-past))

        ;; 2. find the infinitive for this form.
        verb-inf (gram/choose-lexeme {:cat :verb
                                      :infl :infinitive
                                      :italian (get verb-past :aux)})

        ;; 3. get the appropriate auxiliary for that verb.
        ;; TODO: more complicated matching: i.e. {:root verb-inf}
        verb-aux (gram/choose-lexeme
                  (fs/merge
                   {:infl :present
                    :root.italian (get verb-inf :italian)}
                   (if (get verb-past :person)
                     {:person (get verb-past :person)}
                     {})
                   (if (get verb-past :number)
                     {:number (get verb-past :number)}
                     {})))
        ;; 4. generate subject according to verb's constraints.
        subj-constraints
        (fs/merge
         {:cat :noun
          :case {:$ne :acc}}
         (get verb-inf :subj)
         (get (get verb-past :root) :subj)
         (get verb-inf :subj)
         config/random-passato-prossimo-subj
         {:person (get verb-aux :person)
          :number (get verb-aux :number)}
         )
        subject (cond
                 (or (= (get verb-aux :person) "1st")
                     (= (get verb-aux :person) "2nd"))
                 (gram/choose-lexeme subj-constraints)
                 true ;; 3rd person: can be any NP (TODO: verify that gram/np will never generate a :person 1st or :person 2nd np).
                 (gram/np subj-constraints))]
    (fs/merge
     {:verb-inf verb-inf
      :verb-aux verb-aux
      :verb-past verb-past
      :subject subject
      :subj-constraints subj-constraints
      :english (str (get subject :english) " "
                    (conjugate-english-verb verb-past subject) " ")
                    ;(get verb-past :english))
      :italian (str (get subject :italian) " " (get verb-aux :italian) " "
                    (conjugate-italian-verb verb-past subject))}

    {:type-is-fs (set '(:verb-past :subject :verb-inf :subj-constraints :verb-aux))})))

(defn edible-vp []
  (let [verb (random-lexeme {:cat :verb :obj {:edible true}})
        noun (random-lexeme (fs/get-path verb '(:obj)))]
    (gram/left verb (gram/np noun))))

(defn legible-vp []
  (let [verb (random-lexeme {:cat :verb :obj {:legible true}})
        noun (random-lexeme (fs/get-path verb '(:obj)))]
    (gram/left verb (gram/np noun))))

;; cf. grammar/vp: this will replace that.
;(defn vp [ & verb noun]
;  (let [verb (if verb (first verb) (random-lexeme {:cat :verb :infl :infinitive}))
 ;       noun (if noun (first noun)
;                 (if (search/get-path verb '(:obj))
;                   (random-lexeme (search/get-path verb '(:obj)))))]
;    (if noun
;      ;; transitive:
;      (gram/left verb (gram/np noun))
;      ;; else intransitive:
;    verb)))

(defn inflect [verb complement]
  "modify head based on complement: e.g. modify 'imparare la parola' => 'impara la parola' if
   complement is {:person :3rd}."
    (fs/merge verb
           {:italian (conjugate-italian-verb verb complement)
            :english (conjugate-english-verb verb complement)}))

(defn subj [verb]
  "generate a lexical subject that's appropriate given a verb."
  (random-lexeme
   (fs/merge
    {:cat :noun}
    {:case {:not :acc}}
    (get verb :subj))))

(defn random-verb [] (random-lexeme {:cat :verb :infl :infinitive :obj {:not nil}}))

(defn sentence1 []
  (let [verb (random-lexeme {:cat :verb :infl :infinitive :obj {:not nil}})
        object (random-lexeme (get verb :obj))
        subject (random-lexeme (fs/merge {:case {:not :acc}} (get verb :subj)))]
    (list verb object subject)))

(defn np [constraints]
  (let [article (random-lexeme {:cat :det})]
    article))

;; cf. grammar/sentence: this will replace that.
(defn sentence []
  (let [verb (random-lexeme {:cat :verb :infl :infinitive :obj {:cat :noun}})
        object (random-lexeme (:obj verb))
        subject (random-lexeme (:subj verb))]
    {:subject subject :object object :verb verb}))

(defn n-sentences [n]
  (if (> n 0)
    (cons (sentence)
          (n-sentences (- n 1)))))

(defn conjugate-verb [verb subject]
  (let [irregulars
        (search/search (fs/merge {:root (fs/m verb {:infl :infinitive})}
                                 {:subj
                                  (select-keys
                                   subject
                                   (list :person :number))}))]
    (if (first irregulars)
      (first irregulars)
      (merge verb
             {:italian (string/trim (morph/conjugate-italian-verb verb subject))
              :english (string/trim (morph/conjugate-english-verb verb subject))}))))

(defn conjugate-np [noun & [determiner]]
  "conjugate a noun with a determiner (if the noun takes a determiner ((:det noun) is not nil)) randomly chosen using the 'determiner' spec."
  (let [plural-exception (if (or (= (:number noun) :plural) (= (:number noun) "plural"))
                           (let [search (search/search noun)] ;; search for plural exception.
                             (if (and search (> (.size search) 0))
                               (nth search 0))))
        italian (if (or (= (:number noun) :plural)
                        (= (:number noun) "plural"))
                  (if (and plural-exception (:italian plural-exception))
                    (:italian plural-exception)
                    (if (or (= (:gender noun) "masc") (= (:gender noun) :masc))
                      (morph/plural-masc (:italian noun))
                      (morph/plural-fem (:italian noun))))
                  (:italian noun))
        english (if (or (= (:number noun) :plural)
                        (= (:number noun) "plural"))
                  (if (and plural-exception (:english plural-exception))
                    (:english plural-exception)
                    (if (not (:pronoun noun)) ;; pronouns should not be pluralized: e.g. "we" doesn't become "wes".
                      (morph/plural-en (:english noun))
                      (:english noun)))
                  (:english noun))
        article-search (if (not (= (:det noun) nil)) (search/search (fs/m determiner {:cat :det :gender (:gender noun) :number (:number noun)})))
        article (if (and (not (= article-search nil))
                         (not (= (.size article-search) 0)))
                  (nth article-search (rand-int (.size article-search))))]
    {:italian (string/trim (join (list (:italian article) italian) " "))
     :article-search (:det noun)
     :english (string/trim (join (list (:english article) english) " "))
     :article article
     :number (:number noun)
     :gender (:gender noun)
     :person (:person noun)
     :noun noun}))

(defn conjugate-vp [verb subject object]
  (let [conjugated-verb (conjugate-verb verb subject)]
    {:italian (join (list (:italian conjugated-verb) (:italian object)) " ")
     :english (join (list (:english conjugated-verb) (:english object)) " ")
     :root-verb verb
     :subject subject
     :verb conjugated-verb
     :object object}))

(defn conjugate-sent [verb-phrase subject]
  {:italian (join (list (:italian subject) (:italian verb-phrase)) " ")
   :english (join (list (:english subject) (:english verb-phrase)) " ")
   :verb-phrase verb-phrase
   :subject subject})

(defn mobili []
  (let [prep (random-lexeme {:cat :prep :furniture true})
        subject (conjugate-np (fs/m (random-lexeme {:cat :noun :on {:ruggable true}}) {:number (random-symbol :singular :plural)} (:subj prep)))
        object (conjugate-np (fs/m (random-lexeme {:cat :noun :furniture true :on (:on subject)}) {:number (random-symbol :singular :plural)}))]
    (conjugate-sent (conjugate-vp (lookup "essere") subject (conjugate-pp prep object))
                    subject)))

(defn random-present []
  (let [vp
        (let [root-verb (random-lexeme {:cat :verb :infl :infinitive})
              subject (conjugate-np (random-lexeme {:cat :noun} (:subj root-verb))
                                    {:number (random-symbol :singular :plural)})
              object (conjugate-np (random-lexeme {:cat :noun} (:obj root-verb))
                                   {:number (random-symbol :singular :plural)})]
          (if root-verb
            (conjugate-vp (fs/m root-verb {:infl :present})
                          subject
                          object)
            {:fail "verb root not found."}))]
    (conjugate-sent vp (:subject vp))))

(defn rand-sv []
  (let [subjects (search/search {:cat :noun :case {:not :nom}})
        subject (nth subjects (rand-int (.size subjects)))
        root-verbs (search/search {:cat :verb :infl :infinitive})
        root-verb (nth root-verbs (rand-int (.size root-verbs)))
        objects (search/search {:cat :noun :case {:not :acc}})
        object (conjugate-np (nth objects (rand-int (.size objects)))
                             {:def :def})]
    (list subject root-verb object)))

(defn random-transitive []
  (let [vp (let [verbs (search/search {:obj {:not nil} :cat :verb :infl :infinitive})
                 root-verb (nth verbs (rand-int (.size verbs)))
                 objects (search/search (fs/get-path root-verb '(:obj)))
                 subjects (search/search (fs/get-path root-verb '(:subj)))
                 object (conjugate-np (fs/m (nth objects (rand-int (.size objects))) {:def :def}))
                 subject (conjugate-np (nth subjects (rand-int (.size subjects))))]
             (conjugate-vp (fs/m root-verb
                                 {:infl :present})
                           subject
                           object))]
    (conjugate-sent vp (:subject vp))))

;;(map (fn [sent] (:english sent)) (n-random-trans 20))
;; (map (fn [sent] (:italian sent)) (n-random-trans 20))
(defn n-random-trans [n]
  (if (> n 0)
    (cons (random-transitive)
          (n-random-trans (- n 1)))))

(defn random-verb []
  (let [verbs (search/search {:obj {:not nil} :cat :verb :infl :infinitive})]
    (nth verbs (rand-int (.size verbs)))))

(defn check-objs []
  "check that every inf. verb has at least one satisfying object."
  (let [verbs (search/search {:obj {:not nil} :cat :verb :infl :infinitive})
        objs (map (fn [verb]
                    (let [obj-feat (fs/get-path verb '(:obj))
                          objs (search/search obj-feat)]
                      {:italian (:italian verb) :objs (if objs (.size objs) 0)}))
                  verbs)]
    objs))
    
(defn n-vps [n]
  (if (> n 0)
    (let [random-verb (random-verb)
          objects (search/search (fs/get-path random-verb '(:obj)))
          object (conjugate-np (fs/m (nth objects (rand-int (.size objects))) {:def :def}))]
      (cons (cons random-verb object)
            (n-vps (- n 1))))))

(defn random-svo []
  (let [subjects (search/search {:cat :noun :case {:not :nom}})]
    (if (> (.size subjects) 0)
      (let [subject (nth subjects (rand-int (.size subjects)))
            root-verbs (search/search {:cat :verb :infl :infinitive})]
        (if (> (.size root-verbs) 0)
          (let [root-verb (nth root-verbs (rand-int (.size root-verbs)))
                objects (search/search {:cat :noun :case {:not :acc}})]
            (if (> (.size objects) 0)
              (let [object (conjugate-np (fs/m (nth objects (rand-int (.size objects))) {:def :def}))]
                "vp")
                                        ;(conjugate-vp root-verb subject object {:infl :present}))
              "no objects"))
          "no root verbs"))
      "no subjects")))
;        
                 
;             root-verb)]
;    vp))
;                 objects (search/search {:cat :noun})
;                 object (conjugate-np (nth objects (rand-int (.size objects)))
;                                      {:def :def})]
;             root-verb)]
;    vp))
;             (conjugate-vp root-verb
;                           subject
;                           object
;                           {:infl :present}))]
;    (conjugate-sent vp subject)))

(def generate-tests
  (let [five-sentences
        (rdutest
         "Generate a bunch of subjects and make sure they all are really subjects (not something degenerate like {:number :singular}). having an :italian will be the test of subjecthood for now"
         (n-sentences 10)
         (fn [sentences]
           (= 0 (.size (remove #(= true %)
                               (map (fn [sentence]
                                      (let [subject (:subject sentence)]
                                        (not (= nil (:italian (:subject sentence))))))
                                    sentences)))))
         :five-sentences)]

    (list
     (rdutest
      "The 1st person singular present form of 'fare' should be findable."
      (search/search {:root {:infl :infinitive
                             :italian "fare"}})
      (fn [results]
        (and (not (= nil results))
             (not (= nil (first results)))))
      :first-sing-root-of-fare)

;     :merged-fs
     (rdutest
      "Merged fs."
      (fs/merge {:root (first (search/search {:cat :verb :italian "fare" :infl :infinitive}))}
                {:infl :present}
                (select-keys
                 (first (search/search {:italian "io" :pronoun true}))
                 (list :person :number)))
      (fn [resulting-fs]
        (> (count resulting-fs) 0))
      :merged-fs)
     
;     :search-for-conjugation
     (rdutest
      "Look up an irregular verb by searching for a map created by a subject."
      ;; Look up the first singular conjugation for "fare" (which is 'facio')
      (search/search {:root {:cat :verb
                             :italian "fare"
                             :infl :infinitive}
                      :infl :present
                      :subj {:person :1st
                             :number :singular}})
      (fn [results]
        (and (not (= nil results))
             (not (= nil (first results)))))
      :search-for-conjugation)
     
;     :io-facio
     (rdutest
      "Conjugate 'io' + 'fare' => 'io  facio'"
      (conjugate-verb (fs/m (nth (search/search {:italian "fare" :infl :infinitive}) 0) {:infl :present})
                      (nth (search/search {:italian "io" :case :nom}) 0))
      (fn [conjugated]
        (= (:italian conjugated) "facio"))
      :io-facio)

;     :five-sentences
     five-sentences
     
;     :subjects-exist
     (rdutest
      "Make sure subjects are all real lexical entries by checking for non-null :italian feature"
      (map (fn [sentence] (:italian (:subject sentence))) (:test-result five-sentences))
      (fn [sentences]
        (= 0 (.size (remove #(not (= nil %)) sentences))))
      :subjects-exist)
     
;     :subjects-have-nonfail-number
     (rdutest
      "Make sure subject's :number value is valid (non-:fail)."
      (map (fn [sentence] (:subject sentence)) (:test-result five-sentences))
      (fn [subjects]
        (= (.size subjects)
           (.size (remove (fn [subject] (= (:number subject) :fail)) subjects))))
      :subjects-have-nonfail-number)

;     :subjects-case
     (rdutest
      "Make sure subject's :case value is ok (non-:acc and non-:fail). nil is ok: common nouns (in italian
       and english) don't have case."
      (map (fn [sentence] (:case (:subject sentence))) (:test-result five-sentences))
      (fn [cases]
        (= (.size cases)
           (.size (remove (fn [case] (or (= case :fail) (= case "acc"))) cases))))
      :subjects-case)

;     :il-libro
     (rdutest
      "Conjugate 'libro' + '{definite}' => 'il libro'."
      (conjugate-np (lookup "libro") {:def :def})
      (fn [conjugated]
        (= (:italian conjugated) "il libro"))
      :il-libro)

     (rdutest
      "Conjugate 'libro' + '{definite,plural}' => 'i libri'."
      (conjugate-np (fs/m (lookup "libro") {:number :plural}) {:def :def})
      (fn [conjugated]
        (= (:italian conjugated) "i libri"))
      :il-libro)

     (rdutest
      "Conjugate 'sedia' + '{definite,plural}' => 'le sedie'."
      (conjugate-np (fs/m (lookup "sedia") {:number :plural}) {:def :def})
      (fn [conjugated]
        (= (:italian conjugated) "le sedie"))
      :le-sedie)

     
;     :leggo-il-libro
     (rdutest
      "Conjugate 'leggere/[1st sing]-il-libro' => 'leggo il libro'."
      (let [root-verb (nth (search/search {:italian "leggere" :cat :verb :infl :infinitive}) 0)
            object (conjugate-np (nth (search/search {:italian "libro" :cat :noun}) 0) {:def :def})]
        (if root-verb
          (conjugate-vp (fs/m root-verb {:infl :present})
                        (nth (search/search {:italian "io" :case :nom}) 0)
                        object)
          {:fail "verb 'leggere' not found."}))
      (fn [vp]
        (= (:italian vp) "leggo il libro"))
      :leggo-il-libro)


;     :io-leggo-il-libro
     (rdutest
      "Conjugate 'leggere/[1st sing]-il-libro' => 'leggo il libro' / 'io' => 'io leggo il libro'."
      (let [vp
            (let [root-verb (nth (search/search {:italian "leggere" :cat :verb :infl :infinitive}) 0)
                  object (conjugate-np (nth (search/search {:italian "libro" :cat :noun}) 0) {:def :def})]
              (if root-verb
                (conjugate-vp (fs/m root-verb {:infl :present})
                              (nth (search/search {:italian "io" :case :nom}) 0)
                              object)
                {:fail "verb 'leggere' not found."}))]
        (conjugate-sent vp (:subject vp)))
      (fn [sentence]
        (= (:italian sentence) "io leggo il libro"))
      :io-leggo-il-libro)

     
     (rdutest
      "Make sure every transitive verb has at least one noun that satisfies its :obj spec."
      (check-objs)
      (fn [pairs]
        (not (some (fn [pair] (= (:objs pair) 0)) pairs)))
      :every-trans-verb-has-a-possible-object)

     (rdutest
      "essere vp"
      (let [root-verb (lookup "essere")]
        (conjugate-vp (fs/m root-verb {:infl :present})
                      (lookup "io")
                      (conjugate-np (lookup "libro") {:def :indef})))
      (fn [vp]
        (= (:italian vp) "sono un libro"))
      :sono-un-tavolo)

     (rdutest
      "essere sentence"
      (let [root-verb (lookup "essere")
            vp (conjugate-vp (fs/m root-verb {:infl :present})
                             (lookup "voi")
                             (conjugate-np (lookup "libro") {:def :indef}))]
        (conjugate-sent vp (:subject vp)))
      (fn [sentence]
        (= (:italian sentence) "voi siete un libro"))
      :io-sono-un-tavolo)

     (rdutest
      "furniture sentence"
      (mobili)
      (fn [sentence]
        (= (:furniture (:head (:object (:verb-phrase sentence)))) true))
      :mobili)

     (rdutest
      "random present svo sentence"
      (random-present)
      (fn [sentence]
        (and (not (= (:case (:noun (:object (:verb-phrase sentence)))) :nom))
             (not (= (:case (:noun (:subject (:verb-phrase sentence)))) :acc))))
      :random-svo)

     (rdutest
      "soccer does not take an article in both english and italian (i.e. 'calcio', not 'il calcio')"
      (random-present :object {:italian "calcio"})
      (fn [sentence]
        (= (:italian (:obj (:vp sentence))) "calcio"))
      :soccer)
     
     )))

;(defn test []
;  "this should contain a list of all the tests for the generate package. each test can
;  return a map or a list or a function. a function will be applied against an
;  empty argument list"
;  (list
;   {:comment "random 'passato prossimo' sentence."
;    :test (random-passato-prossimo)};;

;   {:comment "random 'futuro semplice' sentence."
;    :test (random-futuro-semplice)}

;   {:comment "sceglieremo (we will choose)"
;    :test (random-futuro-semplice {:italian "sceglieremo"})}

;   {:comment "testing english translation of future modal verbs e.g. 'dovere' (must)"
;    :test (random-futuro-semplice
;           {:italian "dovrai"})}

;   {:comment "testing english translation of future modal verbs e.g. 'potere' (can)"
;    :test (random-futuro-semplice
;           {:italian "potrai"})}))


