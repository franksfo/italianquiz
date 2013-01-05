;; RESTARTING OF RING REQUIRED FOR CHANGES TO THIS FILE. (purtroppo)
(ns italianverbs.generate
  (:use [clojure.stacktrace]
        [italianverbs.lexiconfn])
  (:require
   [clojure.tools.logging :as log]
   [italianverbs.lev :as lev]
   [italianverbs.morphology :as morph]
   [italianverbs.grammar :as gram]
   [italianverbs.fs :as fs]
   [italianverbs.config :as config]
   [italianverbs.html :as html]
   [italianverbs.lexicon :as lex]
   [italianverbs.search :as search]
   [clojure.string :as string]))

(defn printfs [fs & filename]
  "print a feature structure to a file. filename will be something easy to derive from the fs."
  (let [filename (if filename (first filename) "foo.html")]  ;; TODO: some conventional default if deriving from fs is too hard.
    (spit filename (html/static-page (html/tablize fs) filename))))

(defn random-symbol [& symbols]
  (let [symbols (apply list symbols)]
    (nth symbols (rand-int (.size symbols)))))

(defn lookup-exception [fs]
  (let [spec
        (dissoc
         (dissoc
          (dissoc
           fs
           :use-number)
          :fn)
         :morph)
        exception
        (search/random-lexeme {:root spec
                               :number :plural})]
    exception))

(defn morph-noun [fs]
  ;; choose number randomly from {:singular,:plural}.
  (let [number (if (:use-number fs)
                 (:use-number fs)
                 (random-symbol :singular :plural))]
    (if (= number :singular)
      ;;
      fs ;; nothing needed to be done: we are assuming fs comes from lexicon and that singular is the canonical lexical form.
      ;; else, plural
      ;; TODO: check for exceptions in both :english and :italian side.
      (fs/unify fs
            {:number :plural}
            {:comp (fs/unify
                    {:number :plural}
                    (:comp (:comp fs)))}
            (let [exception (lookup-exception fs)]
              (if exception exception
                  (if (= (:gender fs) :fem)
                    {:italian (morph/plural-fem (:italian fs))
                     :english (morph/plural-en (:english fs))}
                    ;; else, assume masculine.
                    ;; :checked-for is for debugging only.
                    {:checked-for {:root (dissoc (dissoc fs :_id) :use-number)}
                     :italian (morph/plural-masc (:italian fs))
                     :english (morph/plural-en (:english fs))})))))))
    
(defn random-morph [& constraints]
  "apply the :morph function to the constraints."
  ;; TODO: constantly switching between variable # of args and one-arg-which-is-a-list...be consistent in API.
  (let [merged (apply fs/unify constraints)
        morph-fn (:morph merged)]
    (if morph-fn
      (fs/unify-and-apply (list (fs/unify merged {:fn morph-fn})))
      (fs/unify {:random-morph :no-morph-fn-default-used}
             merged))))

;; TODO: learn why string/join doesn't work for me:
;; (string/join '("foo" "bar") " ")
;; => " " (should be: "foo bar").

;; note: italianverbs.generate> (string/join " " (list "foo" "bar"))
;; ==> "foo bar" <-- works fine.
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

(defn random-infinitivo []
  (gram/choose-lexeme
   (fs/unify {:cat :verb
           :infl :infinitive}
          config/random-infinitivo)))

(defn espressioni []
  (gram/choose-lexeme {:cat :espressioni}))


(defn random-futuro-semplice [& constraints]
  (let [
        ;; 1. choose a random verb in the passato-prossimo form.
        verb-future (gram/choose-lexeme
                     (fs/unify
                      (if constraints (first constraints) {})
                      {:infl :futuro-semplice}
                      config/futuro-semplice))]
    verb-future))

;; cf. grammar/vp: this will replace that.
;(defn vp [ & verb noun]
;  (let [verb (if verb (first verb) (search/random-lexeme {:cat :verb :infl :infinitive}))
 ;       noun (if noun (first noun)
;                 (if (search/get-path verb '(:obj))
;                   (search/random-lexeme (search/get-path verb '(:obj)))))]
;    (if noun
;      ;; transitive:
;      (gram/left verb (gram/np noun))
;      ;; else intransitive:
;    verb)))

(defn inflect [verb complement]
  "modify head based on complement: e.g. modify 'imparare la parola' => 'impara la parola' if
   complement is {:person :3rd}."
    (fs/unify verb
           {:italian (conjugate-italian-verb verb complement)
            :english (conjugate-english-verb verb complement)}))

(defn subj [verb]
  "generate a lexical subject that's appropriate given a verb."
  (search/random-lexeme
   (fs/unify
    {:cat :noun}
    {:case {:not :acc}}
    (get verb :subj))))

(defn random-verb [] (search/random-lexeme {:cat :verb :infl :infinitive :obj {:not nil}}))

(defn sentence1 []
  (let [verb (search/random-lexeme {:cat :verb :infl :infinitive :obj {:not nil}})
        object (search/random-lexeme (get verb :obj))
        subject (search/random-lexeme (fs/unify {:case {:not :acc}} (get verb :subj)))]
    (list verb object subject)))

(defn np [& constraints]
  (let [det-value (:comp (apply fs/unify constraints))]
    (let [article (if (or (= nil constraints) det-value )
                    (search/random-lexeme {:cat :det}))]
      article)))

;; cf. grammar/sentence: this will replace that.
(defn sentence []
  (let [verb (search/random-lexeme {:cat :verb :infl :infinitive :obj {:cat :noun}})
        object (search/random-lexeme (:obj verb))
        subject (search/random-lexeme (:subj verb))]
    {:subject subject :object object :verb verb}))

(defn n-sentences [n]
  (if (> n 0)
    (cons (sentence)
          (n-sentences (- n 1)))))

(defn mylookup [italian]
  (search/random-lexeme {:italian italian}))

(defn conjugate-verb [verb subject]
  (let [irregulars
        (search/search (fs/unify
                        {:root (fs/merge verb {:infl :infinitive})}
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
  "conjugate a noun with a determiner (if the noun takes a determiner (:comp is not nil)) randomly chosen using the 'determiner' spec."
  (log/info (str "(conjugate-np: " noun "," determiner ")"))
  (let [plural-exception (if (or
                              (= (:number noun) :plural) (= (:number noun) "plural")
                              (and (= (type (:number noun)) clojure.lang.Ref)
                                   (or (= @(:number noun) :plural)
                                       (= @(:number noun) "plural"))))
                           (let [search (search/search noun)] ;; search lexicon for plural exception.
                             (if (and search (> (.size search) 0))
                               (nth search 0))))
        ;; TODO: make these long plural- and masc- checking conditionals much shorter.
        italian (if (or (= (:number noun) :plural)
                        (= (:number noun) "plural")
                        (and (= (type (:number noun)) clojure.lang.Ref)
                             (or (= @(:number noun) :plural)
                                 (= @(:number noun) "plural"))))
                  (if (and plural-exception (:italian plural-exception))
                    (:italian plural-exception)
                    (if (or (= (:gender noun) "masc")
                            (= (:gender noun) :masc)
                            (and (= (type (:gender noun)) clojure.lang.Ref)
                                 (or (= @(:gender noun) :masc)
                                     (= @(:gender noun) "masc"))))
                      (morph/plural-masc (:italian noun))
                      (morph/plural-fem (:italian noun))))
                  (:italian noun))
        english (if (or (= (:number noun) :plural)
                        (= (:number noun) "plural")
                        (and (= (type (:number noun)) clojure.lang.Ref)
                             (or (= @(:number noun) :plural)
                                 (= @(:number noun) "plural"))))
                  (if (and plural-exception (:english plural-exception))
                    (:english plural-exception)
                    (if (not (:pronoun noun)) ;; pronouns should not be pluralized: e.g. "we" doesn't become "wes".
                      (morph/plural-en (:english noun))
                      (:english noun)))
                  (:english noun))
        article-search (if (not (= (:comp noun) nil))
                         (search/search
                          (fs/unify (if determiner determiner {:cat :det})
                                    (:comp noun))))
        article (if (and (not (= article-search nil))
                         (not (= (.size article-search) 0)))
                  (nth article-search (rand-int (.size article-search))))]
    {:italian (string/trim (join (list (:italian article) italian) " "))
     :article-search (:comp noun)
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
     :subject subject
     :verb conjugated-verb
     :object object}))

(defn conjugate-sent [verb-phrase subject]
  {
   :italian (join (list (:italian subject) (:italian verb-phrase)) " ")
   :english (join (list (:english subject) (:english verb-phrase)) " ")
   :verb-phrase verb-phrase
   :subject subject})

(defn mobili []
  (let [prep (search/random-lexeme {:cat :prep :furniture true})
        subject (conjugate-np (fs/merge (search/random-lexeme {:cat :noun :ruggable true}) {:number (random-symbol :singular :plural)} (:subj prep)))
        object (conjugate-np (fs/merge (search/random-lexeme {:cat :noun :furniture true :on (:on subject)}) {:number (random-symbol :singular :plural)}))]
    (conjugate-sent (conjugate-vp (search/lookup "essere") subject (conjugate-pp prep object))
                    subject)))
                          
(defn random-object-for-verb [verb]
  (search/random-lexeme (:obj verb)))

(defn random-verb-for-svo [& svo-maps]
  (let [svo-maps (if svo-maps svo-maps (list {}))]
    (search/random-lexeme (fs/merge (apply :verb svo-maps)
                                    {:cat :verb :infl :infinitive
                                     :obj (fs/merge (apply :obj svo-maps))}))))

(defn random-present [& svo-maps]
  (let [root-verb (apply search/random-lexeme (list (fs/merge {:cat :verb :infl :infinitive}
                                                              svo-maps)))]
    (let [subject (conjugate-np (search/random-lexeme
                                 (:subj root-verb)))
          object
          (if (:obj root-verb)
            (conjugate-np (search/random-lexeme (:obj root-verb))))]
      (let [svo {:subject subject
                 :object object
                 :vp (conjugate-vp (fs/merge root-verb {:infl :present}) subject object)}]
        (fs/unify svo
              (conjugate-sent (:vp svo) subject))))))

;; TODO: refactor with (random-present).
(defn random-past [& svo-maps]
  (let [svo-maps (if svo-maps svo-maps (list {}))
        root-verb (eval `(random-verb-for-svo ~@svo-maps))]
    (let [subject (conjugate-np (search/random-lexeme {:cat :noun} (:subj root-verb)
                                               {:number (random-symbol :singular :plural)}))
          object
          (if (:obj root-verb)
            (conjugate-np (search/random-lexeme {:cat :noun} (:obj root-verb))
                          {:number (random-symbol :singular :plural)}))]
      (let [svo {:subject subject
                 :aux (fs/unify (search/random-lexeme {:italian (:passato-aux root-verb) :infl :infinitive})
                            subject)
                 :object object
                 :vp (fs/unify root-verb {:infl :passato-prossimo})}]
         (conjugate-sent svo subject)))))
    
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
                 objects (search/search (get-in root-verb '(:obj)))
                 subjects (search/search (get-in root-verb '(:subj)))
                 object (conjugate-np (fs/unify (nth objects (rand-int (.size objects))) {:def :def}))
                 subject (conjugate-np (nth subjects (rand-int (.size subjects))))]
             (conjugate-vp (fs/unify root-verb
                                 {:infl :present})
                           subject
                           object))]
    (conjugate-sent vp)))

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
                    (let [obj-feat (get-in verb '(:obj))
                          objs (search/search obj-feat)]
                      {:italian (:italian verb)
                       :objs (if objs (.size objs) 0)}))
                  verbs)]
    objs))
    
(defn n-vps [n]
  (if (> n 0)
    (let [random-verb (random-verb)
          objects (search/search (get-in random-verb '(:obj)))
          object (conjugate-np (fs/unify (nth objects (rand-int (.size objects))) {:def :def}))]
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
              (let [object (conjugate-np (fs/unify (nth objects (rand-int (.size objects))) {:def :def}))]
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

(defn take-article [map]
  (fs/unify map
        {:take-article "taken"}))

(defn random-phrase [head & [comp-constraints]]
  (let [comp-c comp-constraints
        comp (search/random-lexeme (fs/unify comp-c ;; TODO: should be random-phrase, not random-lexeme.
                                  (:comp head)))]
    ;; comp-c is only for debugging.
    {:head head
     :comp-c comp-c
     :comp comp
     :italian (str (:italian comp) " " (:italian head))})) ;; TODO: abstract word order to (serialize) function.

(def examples
  (list {:label "fare: 1st singular"
         :value
         (search/search-one {:root {:infl :infinitive
                                    :italian "fare"}
                             :person :1st
                             :number :singular})}))

(defn generate-signs []
  (str "<table class='generate'>"
       (string/join ""
             (map
              (fn [html]
                (str "<tr>" "<td>"
                     "<div class='result'>" html "</div>"
                     "</td>" "</tr>"))
              (map
               (fn [fs]
                 (html/fs (:value fs)))
               examples)))
       "</table>"))

