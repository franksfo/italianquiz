;; RESTARTING OF RING REQUIRED FOR CHANGES TO THIS FILE. (purtroppo)
(ns italianverbs.generate
  (:use [clojure.stacktrace]
        [italianverbs.rdutest])
  (:require
   [italianverbs.lev :as lev]
   [italianverbs.morphology :as morph]
   [italianverbs.grammar :as gram]
   [italianverbs.fs :as fs]
   [italianverbs.config :as config]
   [italianverbs.html :as html]
   [italianverbs.lexicon :as lex]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.search :as search]
   [clojure.string :as string]
   [clojure.contrib.duck-streams :as duck]))

(defn print [fs]
  "print a feature structure to a file. filename will be something easy to derive from the fs."
  (let [filename "foo.html"]  ;; TODO: some conventional default if deriving from fs is too hard.
    (duck/spit filename (html/static-page (html/tablize fs)))))

(defn mobili []
  (let [fn gram/sv
        head
        (fs/merge
         {:infl :present}
         (let [fn gram/vp-pp
               head (gram/choose-lexeme
                     {:cat :verb
                      :italian "essere"})
               comp
               (let [fn gram/pp
                     head (fs/merge
                           {:already-looked-up true}
                           (gram/choose-lexeme
                            {:cat :prep
                             :furniture-prep true}))
                     comp (gram/np-with-post-conditions 
                            (get head :obj)
                            ;; note that fn name must be unique
                            ;; to avoid a fatal runtime error
                            ;; due to namespace collision.
                            (defn mobili-np-filterfn1 [fs]
                              (= (get fs :def) "def")))]
                 (apply fn (list head comp)))]
           (apply fn (list head comp))))
        comp
        (gram/np-with-post-conditions 
          {:furniture true}
          (defn mobili-np-filterfn2 [fs]
            (= (get fs :def) "def")))]
    (fs/merge {:question-type :mobili}
           (apply fn (list head comp)))))

(defn conjugate-italian-prep [preposition prep np]
  (morph/conjugate-italian-prep preposition prep np))

(defn conjugate-english-verb [vp subject]
  ;; TODO: lookup irregular form (if any) using vp and subject.
  (morph/conjugate-english-verb vp subject))

(defn conjugate-italian-verb [vp subject]
  ;; TODO: lookup irregular form (if any) using vp and subject.
  (morph/conjugate-italian-verb vp subject))

;; TODO : factor out commonalities between random-present and random-passato-prossimo.
(defn random-present []
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

(defn random-lexeme [& constraints]
  (let [constraints (first constraints)
        lexemes (seq
                 (search/query constraints))]
    (if lexemes
      (if (> (.size lexemes) 0)
        (nth lexemes (rand-int (.size lexemes)))))))

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
        object (fs/merge (random-lexeme (:obj verb))
                         {:number (gram/random-number)})
        ;; note that you currently can't use fs/merge, because if you use it,
        ;; atomic values fail when unified.
        subject (let [random-lexeme (random-lexeme (:subj verb))]
                  (if (= nil (:number random-lexeme))
                    (merge random-lexeme
                           {:number (gram/random-number)})
                    random-lexeme))]
    {:subject subject :object object :verb verb}))

(defn n-sentences [n]
  (if (> n 0)
    (cons (sentence)
          (n-sentences (- n 1)))))

(def tests
  (let [five-sentences
        (rdutest
         "Generate a bunch of subjects and make sure they all are really subjects (not something degenerate like {:number :singular}). having an :italian will be the test of subjecthood for now"
         (n-sentences 10)
         (fn [sentences]
           (= 0 (.size (remove #(= true %)
                               (map (fn [sentence]
                                      (let [subject (:subject sentence)]
                                        (not (= nil (:italian (:subject sentence))))))
                                    sentences))))))]

    {:five-sentences
     five-sentences

     :subjects-exist
     (rdutest
      "Make sure subjects are all real lexical entries by checking for non-null :italian feature"
      (map (fn [sentence] (:italian (:subject sentence))) (:test-result five-sentences))
      (fn [sentences]
        (= 0 (.size (remove #(not (= nil %)) sentences)))))
     
     :subjects-have-nonfail-number
     (rdutest
      "Make sure subject's :number value is valid (non-:fail)."
      (map (fn [sentence] (:subject sentence)) (:test-result five-sentences))
      (fn [subjects]
        (= (.size subjects)
           (.size (remove (fn [subject] (= (:number subject) :fail)) subjects)))))

     :subjects-case
     (rdutest
      "Make sure subject's :case value is ok (non-:acc and non-:fail). nil is ok: common nouns (in italian
       and english) don't have case."
      (map (fn [sentence] (:case (:subject sentence))) (:test-result five-sentences))
      (fn [cases]
        (= (.size cases)
           (.size (remove (fn [case] (or (= case :fail) (= case "acc"))) cases)))))
     


     }))

;; diagnostic.
(def verbs
  (map (fn [sentence]
         {:verb (:verb sentence)})
       (:test-result (:five-sentences tests))))

(def subjects
  (map (fn [sentence]
         (:subject sentence))
       (:test-result (:five-sentences tests))))

(def objects
  (map (fn [sentence]
         {:obj (:italian (:object sentence))})
       (:test-result (:five-sentences tests))))

(defn conjugate [verb subject]
  (morph/conjugate-italian-verb-regular verb subject))

(def inflected
  (map (fn [sentence]
         (let [merge
               (fs/merge
                (:verb sentence)
                {:italian-inflected (conjugate (:verb sentence)
                                               (:subject sentence))})]
           {:italian (:italian-inflected merge)}))
       (:test-result (:five-sentences tests))))

(def concatted
  (map (fn [sentence]
         (str
          (:italian (:subject sentence))
          " "
          (conjugate (:verb sentence)
                     (:subject sentence))))
       (:test-result (:five-sentences tests))))
  


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



