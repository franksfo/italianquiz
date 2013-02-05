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

(def sentence-skeleton-1
  (unify gram/s {:comp gram/np :head (unify gram/vp-present {:comp gram/np})}))

(def sentence-skeleton-2
  (unify gram/s {:head gram/s
                 :comp gram/prep-phrase}))

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

                                        ;(def head-specification (get-terminal-head-in sentence-skeleton-1))
(def head-specification (get-terminal-head-in gram/vp-present))
(def matching-lexical-heads (mapcat (fn [lexeme] (if (not (fs/fail? lexeme)) (list lexeme)))
                                    (map (fn [lexeme] (fs/match (fs/copy head-specification) (fs/copy lexeme))) lex/lexicon)))
(def random-lexical-head (if (> (.size matching-lexical-heads) 0)
                           (nth matching-lexical-heads (rand-int (.size matching-lexical-heads)))))
(def obj-spec (fs/get-in random-lexical-head '(:synsem :subcat :2)))

(def object-np (random-np {:synsem obj-spec}))

(def subj-spec (fs/get-in random-lexical-head '(:synsem :subcat :1)))
(def subject-np (random-np {:synsem (unify subj-spec
                                           {:subcat {:1 {:cat :det}}})}))

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
                       (fs/get-in unified '(:2 :italian)))
             :english (morph/get-english
                       (fs/get-in unified '(:1 :english))
                       (fs/get-in unified '(:2 :english)))})

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

(defn lots-of-sentences-1 []
  (over
   (over gram/s
         lex/lexicon)
   (over
    (over gram/vp-present lex/lexicon)
    (over (over gram/np lex/lexicon) lex/lexicon))))

(defn lots-of-sentences-2 []
  (over
   (over gram/s
         (over (over gram/np lex/lexicon) lex/lexicon))
   (over
    (over gram/vp-present lex/lexicon)
    (over (over gram/np lex/lexicon) lex/lexicon))))

(defn lots-of-sentences []
  (concat
   (lots-of-sentences-1)
   (lots-of-sentences-2)))

(def my-vp-rules
  (list
   (let [obj-sem (ref :top)
         obj-synsem (ref {:sem obj-sem})
         obj (ref {:synsem obj-synsem})
         subj-sem (ref :top)
         subj-synsem (ref {:sem subj-sem})
         head-synsem (ref {:cat :verb
                           :infl {:not :infinitive}
                           :sem {:subj subj-sem
                                 :obj obj-sem}
                           :subcat {:1 subj-synsem
                                    :2 obj-synsem}})
         head (ref {:synsem head-synsem})]
     (fs/unifyc gram/head-principle
                {:comment "vp -> head comp"
                 :head head
                 :synsem {:subcat {:1 subj-synsem}}
                 :comp obj
                 :1 head
                 :2 obj}))))


(def myrules (concat gram/np-rules my-vp-rules gram/sentence-rules))

(defn get-morph [expr morph-fn]
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
   ""))

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

(defn subject-spec [verb]
  {:synsem (fs/get-in (nth (lex/it verb) 0)
                      '(:synsem :subcat :1))})

(defn object-spec [verb]
  {:synsem (fs/get-in (nth (lex/it verb) 0)
                      '(:synsem :subcat :2))})

(defn args1 [head]
  "lookup lexical entries that can satisfy the first subcat position of the given head."
  (let [head-fs
        (if (map? head) head
            ;; assume string:
            (first (lex/it head)))]
    (lex/lookup {:synsem (fs/get-in head-fs
                                  '(:synsem :subcat :1))})))

(def arg1-index
  (zipmap lex/lexicon (map (fn [lexeme] (args1 lexeme)) lex/lexicon)))

(defn args1-cached [head]
  "lookup lexical entries that can satisfy the first subcat position of the given head."
  (let [head-fs
        (if (map? head) head
            ;; assume string:
            (first (lex/it head)))]
    (get arg1-index head-fs)))

(defn args2 [head]
  "lookup lexical entries that can satisfy the second subcat position of the given head."
  (lex/lookup {:synsem (fs/get-in (nth (lex/it head) 0)
                                  '(:synsem :subcat :2))}))

(defn subjects [verb]
  "lookup lexical entries that can be subjects of the given verb."
  (args1 verb))

(defn objects [verb]
  "lookup lexical entries that can be objects of the given verb."
  (args2 verb))

(def vp-head-spec
  (fs/get-in (nth gram/vp-rules 0) '(:head)))

(defn rp1 [& head-spec]
  (let [head-spec
        (if (not (nil? head-spec))
          head-spec
          vp-head-spec)]
    (let [heads (filterv
                 (fn [lexeme]
                   (not (fs/fail?
                         (fs/match (fs/copy head-spec)
                                   (fs/copy lexeme)))))
                 lex/lexicon)
          ;; choose one of the above heads at random:
          head (if (> (.length heads) 0)
                 (nth heads (rand-int (.length heads))))]
      head)))

(defn random-phrase [& head-spec]
  (let [head-spec (first head-spec)
        head-spec (if (not (nil? head-spec))
                    head-spec
                    vp-head-spec)]
    (let [heads (if (string? head-spec)
                  (vec (lex/it head-spec))
                  (filterv
                   (fn [lexeme]
                     (not (fs/fail?
                           (fs/match (fs/copy head-spec)
                                     (fs/copy lexeme)))))
                   lex/lexicon))
          ;; choose one of the above heads at random:
          head (if (> (.length heads) 0)
                 (nth heads (rand-int (.length heads))))
          comp2 (let [subcat (fs/get-in head '(:synsem :subcat :2) :none)]
                  (if (not (= subcat :none))
                    (random-phrase {:synsem subcat})))
          comp1 (let [subcat (fs/get-in head '(:synsem :subcat :1) :none)]
                  (if (not (= subcat :none))
                  (random-phrase {:synsem subcat})))]
      (conj
       (if head
         {:head head}
         {})
       (if (not (nil? comp1))
         {:comp1 comp1}
         {})
       (if (not (nil? comp2))
         {:comp2 comp2}
         {})))))

(defn random-sentence []
  (let [rand (rand-int 2)
        sentences
        (cond (= rand 0)
              (over gram/s lex/pronouns lex/verbs)
              (and true (= rand 1))
              (let [sentences
                    (over gram/s lex/pronouns
                          (first (over gram/vp-present
                                       lex/present-aux-verbs
                                       (first (over (over gram/vp-past (first lex/past-verbs))
                                                    (over gram/np lex/determiners lex/nouns))))))]
                (take-last 1
                           (take (rand-int (.size sentences))
                                 sentences))))]
    (nth sentences (rand-int (.size sentences)))))

(defn random-sentence-busted []
  (let [head-specification
        (fs/copy (get-terminal-head-in gram/vp-present))
        matching-lexical-verb-heads
        (mapcat (fn [lexeme] (if (not (fs/fail? lexeme)) (list lexeme)))
                (map (fn [lexeme] (fs/match head-specification lexeme)) lex/lexicon))
        random-verb (if (> (.size matching-lexical-heads) 0)
                              (nth matching-lexical-heads (rand-int (.size matching-lexical-heads))))
        obj-spec (fs/get-in random-verb '(:synsem :subcat :2))
        object-np
        (random-np (unify {:synsem (unify obj-spec
                                          {:subcat {:1 {:cat :det}}})}))
        subj-spec (fs/get-in random-verb '(:synsem :subcat :1))
        subject-np (random-subject-np subj-spec)]
    (let [unified (unify sentence-skeleton-1
                         {:head
                          (let [unified
                                (unify
                                 (fs/get-in sentence-skeleton-1 '(:head))
                                 {:head random-verb
                                  :comp object-np})]
                            (fs/merge
                             {:italian
                              (morph/get-italian
                               (fs/get-in unified '(:1 :italian))
                               (fs/get-in unified '(:2 :italian)))
                              :english
                              (morph/get-english
                               (fs/get-in unified '(:1 :english))
                               (fs/get-in unified '(:2 :english)))}
                             unified))}
                         {:comp subject-np})]
      (if (not (fs/fail? unified))
        (merge
         {:italian (morph/get-italian
                    (fs/get-in unified '(:1 :italian))
                    (fs/get-in unified '(:2 :italian)))
          :english (morph/get-english
                    (fs/get-in unified '(:1 :english))
                    (fs/get-in unified '(:2 :english)))}
         unified)
        unified))))

(defn random-sentences [n]
  (repeatedly n (fn [] (random-sentence))))

(defn random-extend [phrase]
  "return a random expansion of the given phrase, taken by looking at the phrase's :extend value, which list all possible expansions."
  (nth (vals (fs/get-in phrase '(:extend))) (int (* (rand 1) (.size (fs/get-in phrase '(:extend)))))))
