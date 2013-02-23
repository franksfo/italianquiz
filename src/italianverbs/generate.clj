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

(defn get-terminal-head-in [phrase-structure]
  (let [local-head (fs/get-in phrase-structure '(:head))]
    (if (not (nil? local-head))
      (get-terminal-head-in local-head)
      phrase-structure)))

(defn random-np-random-lexical-head [head-spec]
  (let [skeleton gram/np
        head-specification (unify head-spec (get-terminal-head-in skeleton))
        matching-lexical-heads (mapcat (fn [lexeme] (if (not (fs/fail? lexeme)) (list lexeme)))
                                       (map (fn [lexeme] (fs/match (fs/copy head-specification) (fs/copy lexeme))) lex/lexicon))]
    (if (> (.size matching-lexical-heads) 0)
      (nth matching-lexical-heads (rand-int (.size matching-lexical-heads))))))

(defn filter-by-match [spec lexicon]
  (mapcat (fn [lexeme] (if (not (fs/fail? lexeme)) (list lexeme)))
          (map (fn [lexeme] (fs/match (fs/copy spec) (fs/copy lexeme))) lex/lexicon)))

(defn random-np [head-spec]
  (let [matching-lexical-heads (filter-by-match head-spec lex/lexicon)
        random-lexical-head (if (> (.size matching-lexical-heads) 0)
                              (nth matching-lexical-heads (rand-int (.size matching-lexical-heads))))]
;    (println (str "random-np: skel:" np))
;    (println (str "np head-spec   :" head-spec))
;    (println (str "random-np: rlh :" random-lexical-head))
    (let [matching-lexical-comps (filter-by-match {:synsem (fs/get-in random-lexical-head '(:synsem :subcat :1))}
                                                  lex/lexicon)
          random-lexical-comp (if (> (.size matching-lexical-comps) 0)
                                (nth matching-lexical-comps (rand-int (.size matching-lexical-comps))))]
      (let [unified (unify gram/np {:head random-lexical-head
                               :comp random-lexical-comp})]
        (if (not (fs/fail? unified))
          (merge
           {:italian (morph/get-italian
                      (fs/get-in unified '(:1 :italian))
                      (fs/get-in unified '(:2 :italian)))
            :english (morph/get-english
                      (fs/get-in unified '(:1 :english))
                      (fs/get-in unified '(:2 :english)))}
           unified)
          unified)))))

(defn random-subject-np [head-spec]
  (let [rand (rand-int 2)]
    (if (= rand 0)
      (random-np {:synsem
                  (unify
                   head-spec
                   {:subcat {:1 {:cat :det}}})})
      (let [matching (filter-by-match
                      {:synsem
                       (unify
                        head-spec
                        {:cat :noun
                         :agr {:case :nom}
                         :subcat :nil!})}
                      lex/lexicon)]
        (if (> (.size matching) 0)
          (nth matching (rand-int (.size matching))))))))

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

(defn random-infinitivo []
  (choose-lexeme
   (fs/unify {:cat :verb
           :infl :infinitive}
          config/random-infinitivo)))

(defn espressioni []
  (choose-lexeme {:cat :espressioni}))


(defn random-futuro-semplice [& constraints]
  (let [
        ;; 1. choose a random verb in the passato-prossimo form.
        verb-future (choose-lexeme
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

(defn generate-np [& constraints]
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

(defn conjugate-sent [verb-phrase subject]
  {
   :italian (join (list (:italian subject) (:italian verb-phrase)) " ")
   :english (join (list (:english subject) (:english verb-phrase)) " ")
   :verb-phrase verb-phrase
   :subject subject})
                          
(defn random-object-for-verb [verb]
  (search/random-lexeme (:obj verb)))

(defn random-verb-for-svo [& svo-maps]
  (let [svo-maps (if svo-maps svo-maps (list {}))]
    (search/random-lexeme (fs/merge (apply :verb svo-maps)
                                    {:cat :verb :infl :infinitive
                                     :obj (fs/merge (apply :obj svo-maps))}))))

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

(defn over-parent-child [parent child]
  (cond

   (= (type child) java.lang.String)
   (over-parent-child parent (lex/it child))

   (seq? parent)
   (flatten
    (map (fn [each-parent]
           (over-parent-child each-parent child))
         parent))

   (or (set? child) (seq? child))
   (remove (fn [result]
             (or (fs/fail? result)
                 (nil? result)))
           (flatten
            (map (fn [each-child]
                   (let [parent parent
                         child each-child]
                     (over-parent-child parent child)))
                 child)))

   :else ; both parent and child are non-lists.
   ;; First, check to make sure complement matches head's (:synsem :sem) value; otherwise, fail.
   (let [
         ;; "add-child-where": find where to attach child (:1 or :2), depending on value of current left child (:1)'s :italian.
         ;; if (:1 :italian) is nil, the parent has no child at :1 yet, so attach new child there at :1.
         ;; Otherwise, a :child exists at :1, so attach new child at :2.
         add-child-where (if (nil?
                              (fs/get-in parent '(:1 :italian)))
                           :1
                           :2)
         head-is-where (if (= (fs/get-in parent '(:head))
                              (fs/get-in parent '(:1)))
                         :1
                         :2)
         child-is-head (= head-is-where add-child-where)
         comp (if child-is-head
                (fs/get-in parent '(:comp))
                child)
         head (if child-is-head
                child
                (fs/get-in parent '(:head)))
         sem-filter (fs/get-in head '(:synsem :subcat :2 :sem)) ;; :1 VERSUS :2 : make this more explicit about what we are searching for.
         comp-sem (fs/get-in comp '(:synsem :sem))
         do-match
         (if (and (not (nil? sem-filter))
                  (not (nil? comp-sem)))
           (fs/match {:synsem (fs/copy sem-filter)}
                     (fs/copy {:synsem (fs/copy comp-sem)})))]
     (if (= do-match :fail)
       :fail
       (let [unified (unify parent
                            {add-child-where
                             (unify
                              (let [sem (fs/get-in child '(:synsem :sem) :notfound)]
                                (if (not (= sem :notfound))
                                  {:synsem {:sem (lex/sem-impl sem)}}
                                  {}))
                             child)})]
         (if (not (fs/fail? unified))
           (merge ;; use merge so that we overwrite the value for :italian.
            unified
            {:italian (morph/get-italian
                       (fs/get-in unified '(:1 :italian))
                       (fs/get-in unified '(:2 :italian))
                       (fs/get-in unified '(:head :synsem :cat))
                       (fs/get-in unified '(:comp :synsem :cat)))
             :english (morph/get-english
                       (fs/get-in unified '(:1 :english))
                       (fs/get-in unified '(:2 :english))
                       (fs/get-in unified '(:head :synsem :cat))
                       (fs/get-in unified '(:comp :synsem :cat)))})
           :fail))))))

(defn over [& args]
  "usage: (over parent child) or (over parent child1 child2)"
  (let [parent (first args)
        child1 (second args)
        child2 (if (> (.size args) 2) (nth args 2))]
    (if (not (nil? child2))
      (over-parent-child (over-parent-child parent child1) child2)
      (over-parent-child parent child1))))

(defn overall [& args]
  "'overall' rules: try all rules as parents, with the args as children."
  (let [child1 (first args)
        child2 (if (> (.size args) 1) (nth args 1))]
    (if (not (nil? child2))
      (over-parent-child (over-parent-child gram/rules child1) child2)
      (over-parent-child gram/rules child1))))

(defn get-morph [expr morph-fn]
  (let [head-cat (fs/get-in expr '(:head :synsem :cat) :notfound)
        comp-cat (fs/get-in expr '(:comp :synsem :cat) :notfound)]
    (morph-fn
     (cond
      (and (map? expr)
           (not (nil? (fs/get-in expr '(:1 :infinitive :infinitive)))))
      (string/join " " (list (fs/get-in expr '(:1 :infinitive :infinitive))
                             "(finite)"
                             (morph-fn (fs/get-in expr '(:2)) "")))
      (and (map? expr)
           (not (nil? (fs/get-in expr '(:1 :infinitive)))))
      (string/join " " (list (morph-fn (fs/get-in expr '(:1 :infinitive)) "")
                             "(finite)"
                             (morph-fn (fs/get-in expr '(:2)) "")))
      (and (map? expr)
           (not (nil? (fs/get-in expr '(:irregular)))))
      (str (fs/get-in expr '(:infinitive)) " (finite)")
      (and (map? expr)
           (not (nil? (fs/get-in expr '(:infinitive))))
           (= java.lang.String (type (fs/get-in expr '(:infinitive)))))
      (str (fs/get-in expr '(:infinitive)) " (finite)")
      (and (map? expr)
           (not (nil? (fs/get-in expr '(:infinitive)))))
      (fs/get-in expr '(:infinitive :infinitive))
      :else
      expr)
     "")))

;;; e.g.:
;;; (formattare (over (over s (over (over np lexicon) (lookup {:synsem {:human true}}))) (over (over vp lexicon) (over (over np lexicon) lexicon))))
;; TO move this to html.clj: has to do with presentation.
(defn formattare [expressions]
  "format a bunch of expressions (feature-structures) showing just the italian (and english in parentheses)."
  (if (map? expressions)
    ;; wrap this single expression in a list and re-call.
    (formattare (list expressions))

    ;; show the italian and english for each expression.
    (map (fn [expr]
           (let [english
                 (string/capitalize
                  (get-morph (fs/get-in expr '(:english))
                             morph/get-english))
                 italian
                 (string/capitalize
                  (get-morph (fs/get-in expr '(:italian))
                             morph/get-italian))]
             (string/trim
              (str italian " (" english ")."))))
         expressions)))

(defn random-extension [phrase]
  (if (= :notfound (:extend phrase :notfound))
    (throw (Exception. (str "Can't generate using this rule because it has no :extend feature.")))
    (let [random-key (nth (keys (:extend phrase)) (rand-int (.size (keys (:extend phrase)))))]
      (get (:extend phrase) random-key))))

(defn eval-symbol [symbol]
  (cond
   (= symbol 'adjectives) lex/adjectives
   (= symbol 'nouns) lex/nouns
   (= symbol 'verbs) lex/verbs
   (= symbol 'present-verbs) lex/present-verbs
   (= symbol 'present-intransitive-verbs) lex/present-intransitive-verbs
   (= symbol 'present-transitive-verbs) lex/present-transitive-verbs
   (= symbol 'future-intransitive-verbs) lex/future-intransitive-verbs
   (= symbol 'future-transitive-verbs) lex/future-transitive-verbs
   (= symbol 'present-aux-verbs) lex/present-aux-verbs
   (= symbol 'past-verbs) lex/past-verbs
   (= symbol 'past-intransitive-verbs) lex/past-intransitive-verbs
   (= symbol 'past-transitive-verbs) lex/past-transitive-verbs
   (= symbol 'future-transitive-verbs) lex/future-transitive-verbs
   (= symbol 'determiners) lex/determiners
   (= symbol 'pronouns) lex/pronouns
   (= symbol 'nbar) gram/nbar
   (= symbol 'np) gram/np
   (= symbol 'vp-present) gram/vp-present
   (= symbol 'vp-past) gram/vp-past
   (= symbol 'vp-future) gram/vp-future
   true (throw (Exception. (str "(italianverbs.generate/eval-symbol could not evaluate symbol: '" symbol "'")))))

(defn random-head-and-comp-from-phrase [phrase expansion]
  (let [head (eval-symbol (:head expansion))
        comp (:comp expansion)] ;; leave comp as just a symbol for now: we will evaluate it later in random-comp-from-head.
    {:head head
     :comp comp}))

;; filter by unifying each candidate against parent's :head value -
;; if configured to do so. If no filtering, we assume that all candidates
;; will unify successfully without checking, so we can avoid the time spent
;; in this filtering.
(def filter-head true)

(defn head-candidates [phrase expansion]
  (let [head-and-comp (random-head-and-comp-from-phrase phrase expansion)
        parent phrase]
    (let [head (:head head-and-comp)
;          debug (println "HC: HEAD: " head)
;          debug (println "head's type is: " (type head))
          head-filter (fs/unifyc (fs/get-in parent '(:head))
                                 (if (not (nil? (fs/get-in parent '(:head :synsem :sem))))
                                   {:head {:synsem {:sem (lex/sem-impl (fs/get-in parent '(:head :synsem :sem)))}}}
                                   :top))
;          debug (println (str "HC:HF: " head-filter))
          candidates
          (if (and filter-head (seq? head))
            (filter (fn [head-candidate]
                      (let [head-candidate
                            (if (not (nil? (fs/get-in head-candidate '(:synsem :sem))))
                              (fs/unifyc head-candidate
                                         {:synsem {:sem (lex/sem-impl (fs/get-in head-candidate '(:synsem :sem)))}})
                              head-candidate)]
                        (not (fs/fail? (fs/unifyc head-filter head-candidate)))))
                    head)
            ;; map: unify with parent's head constraints
            (fs/unifyc head (fs/get-in parent '(:head))))]
      (if (nil? candidates)
        (throw (Exception. (str "Candidates is nil."))))
;;     (println (str "HC:HF(2): " head-filter))
;;      (println (str "HC:CA: (" (.size candidates) ") " (join (map (fn [x] x) candidates) " ; ")))
;      (if (seq? candidates) (println (str "HC CANDIDATES: " (join (map (fn [x] (fs/get-in x '(:italian))) candidates) " ; "))))
;      (if (map? candidates) (println (str "HC CANDIDATES IS A MAP (must recursively generate).")))
      (if (= (.size candidates) 0)
        (throw (Exception. (str "No candidates found for filter: " (if (nil? head-filter) "(nil)" head-filter)))))
      candidates)))

(declare generate)

(defn random-head [head-and-comp parent expansion]
;  (println (str "RH parent: " parent))
;  (println (str "RH head-filter: " (fs/get-in parent '(:head))))
  (let [head (:head head-and-comp)
        head-filter (fs/get-in parent '(:head))
        head (head-candidates parent expansion)]
    (cond
     (map? head)
     ;; recursively expand.
     (generate head)
     (seq? head) ;; head is a list of candidate heads (lexemes).
     (rand-nth head)
     true
     (throw (Exception. (str "- don't know how to get a head from: " head))))))

(defn expansion-to-candidates [comp-expansion comp-spec]
;  (println (str "ETC1: COMP-EXP: " comp-expansion))
;  (println (str "ETC1: COMP-SPEC: " comp-spec))
  (let [comps
        (if (symbol? comp-expansion)
          (eval-symbol comp-expansion)
          comp-expansion)]
    (cond
     (seq? comps)
     comps
     (map? comps) ;; a map such as np:
     (let [unified-spec (fs/unifyc comps comp-spec)
;           debug (println (str "ETC: COMPS: " comps))
;           debug (println (str "ETC: COMP-SPEC: " comp-spec))
;           debug (println (str "ETC: UNIFIED SPEC: " unified-spec))
           generated (generate unified-spec)] ;; recursively generate a phrase.
       (if (nil? generated)
         (throw (Exception.
                 (str "(generate) could not generate: returned nil with input: " unified-spec)))
         (list generated))) ;; wrap the generated phrase in a list (so we choose 'randomly' from amongst a singleton set).
     (nil? comp-expansion)
     (throw (Exception. (str "comp-expansion must not be null.")))
     true
     (do
       (log/error (str "input values: " comp-expansion " and " comp-spec " are not supported yet."))
       (throw (Exception. (str "TODO: recursively expand rules.")))))))

(defn random-comp-for-parent [parent comp-expansion]
  (let [candidates (expansion-to-candidates comp-expansion (fs/get-in parent '(:comp)))
        path-to-comp-sem (if (not (nil? (fs/get-in parent '(:comp :synsem :sem :mod))))
                           '(:comp :synsem :sem :mod)
                           '(:comp :synsem :sem))
        sem-impl-input (if (map? (fs/get-in parent path-to-comp-sem))
                         (dissoc (dissoc (fs/get-in parent path-to-comp-sem) :mod) :pred))
        sem-impl-result (if (not (nil? sem-impl-input))
                          (do
;                            (println (str "NON-NULL SEMANTICS: " (fs/get-in parent path-to-comp-sem)))
                            (lex/sem-impl sem-impl-input)))
;        debug (println (str "SEM-IMPL INPUT: " sem-impl-input))
;        debug (println (str "SEM-IMPL: " sem-impl-result))
        complement-filter (if (not (nil? sem-impl-result))
                            (if (= path-to-comp-sem '(:comp :synsem :sem :mod))
                              (unify {:synsem {:sem {:mod sem-impl-result}}}
                                     (fs/get-in parent '(:comp)))
                              (unify {:synsem {:sem sem-impl-result}}
                                     (fs/get-in parent '(:comp))))
                            (fs/get-in parent '(:comp)))
;        debug (println (str "CF: " complement-filter))
        filtered  (if (> (.size candidates) 0)
                    (filter (fn [x]
                              (not (fs/fail?
                                    (if (not (= complement-filter {}))
                                      (fs/unifyc complement-filter x)
                                      :top))))
                            candidates)
                    (throw (Exception. (str "No candidates found for comp-expansion: "
                                            comp-expansion))))]
    {:comp-candidates-unfiltered candidates
     :filtered filtered
     :comp (if (> (.size filtered) 0)
             (nth filtered (rand-int (.size filtered)))
             (throw (Exception. (str "None of the candidates: "
                                     (join (map (fn[x] (str "'" (:italian x) "/" (fs/get-in x '(:synsem :sem)) "'")) candidates)
                                           " ") " matched the filter: (sem:" (fs/get-in complement-filter '(:synsem :sem))
                                           " from parent: " (fs/get-in parent '(:head :italian))))))
     :parent parent
     :expansion comp-expansion}))

(defn generate-with-parent [random-head-and-comp phrase expansion]
;  (println (str "GWP: RHAC: " random-head-and-comp))
;  (println (str "GWP: PHRA: " phrase))
  (let [random-head (random-head random-head-and-comp phrase expansion)]
    (unify
     (fs/copy phrase)
     {:head (fs/copy random-head)})))

(defn generate [phrase]
;  (println (str "GENERATE: PHRASE: " phrase))
  (let [chosen-extension (random-extension phrase)
        random-head-and-comp (random-head-and-comp-from-phrase phrase chosen-extension)
;        debug (println (str "GENERATE: RHAC: " random-head-and-comp))
        unified-parent (generate-with-parent random-head-and-comp phrase chosen-extension)
;        debug (println (str "GENERATE: UP: " unified-parent))
        comp-expansion (:comp random-head-and-comp)]
      ;; now get complement given this head.
    (if (nil? comp-expansion) ;; a phrase with only a single child constituent: just return the parent.
      (merge
       unified-parent
       {:extend chosen-extension}
       {:italian (morph/get-italian
                  (fs/get-in unified-parent '(:1 :italian))
                  ""
                  (fs/get-in unified-parent '(:1 :synsem :cat))
                  nil)
        :english (morph/get-english
                  (fs/get-in unified-parent '(:1 :english))
                  ""
                  (fs/get-in unified-parent '(:1 :synsem :cat))
                  nil)})

      ;; else there's a complement.
      (let [random-comp
            (random-comp-for-parent unified-parent comp-expansion)
            unified-with-comp
            (fs/unifyc
             unified-parent
             {:comp
              (fs/get-in random-comp '(:comp))})]
        (log/debug (str "italian 1:" (fs/get-in unified-with-comp '(:1 :italian))))
        (log/debug (str "italian 2:" (fs/get-in unified-with-comp '(:2 :italian))))
        (log/debug (str "1 cat:" (fs/get-in unified-with-comp '(:1 :synsem :cat))))
        (log/debug (str "2 cat:" (fs/get-in unified-with-comp '(:2 :synsem :cat))))
        (let [result
              (merge
               unified-with-comp
               {:extend chosen-extension}
               {:italian (morph/get-italian
                          (fs/get-in unified-with-comp '(:1 :italian))
                          (fs/get-in unified-with-comp '(:2 :italian))
                          (fs/get-in unified-with-comp '(:1 :synsem :cat))
                          (fs/get-in unified-with-comp '(:2 :synsem :cat)))
                
                :english (morph/get-english
                          (fs/get-in unified-with-comp '(:1 :english))
                          (fs/get-in unified-with-comp '(:2 :english))
                          (fs/get-in unified-with-comp '(:1 :synsem :cat))
                          (fs/get-in unified-with-comp '(:2 :synsem :cat)))})]
          (if (fs/fail? result)
            (do
              (log/error (str "Generation failed: parent: " (:comment unified-parent)))
              (log/error (str "Generation failed: comp: " (:comment random-comp)))
              (throw (Exception. (str "Generation failed: parent: " (:comment unified-parent)
                                      "; comp: " random-comp))))
            (fs/copy-trunc result)))))))

(defn random-sentence []
    (generate gram/s1))

(defn random-sentences [n]
  (repeatedly n (fn [] (random-sentence))))

(defn speed-test [ & times]
  "TODO: show benchmark results and statistics (min,max,95%tile,stddev,etc)"
  (let [times (if (first times) (first times) 10)]
    (dotimes [n times] (time (random-sentence)))))
