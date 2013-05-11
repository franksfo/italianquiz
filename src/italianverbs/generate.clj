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

(defn capitalize [s]
  "Capitalize first char and leave the rest of the characters alone (compare with string/capitalize which lower-cases all chars after first."
  (if (nil? s) ""
      (let [s (.toString s)]
        (if (< (count s) 2)
          (.toUpperCase s)
          (str (.toUpperCase (subs s 0 1))
               (subs s 1))))))

(defn formattare-1 [expr]
  (cond
   (fs/fail? expr)
   "<tt>fail</tt>"
   :else
   (let [english
         (capitalize
          (cond
           (string? (fs/get-in expr '(:english)))
           (fs/get-in expr '(:english))

           (string? (fs/get-in expr '(:english :english)))
           (fs/get-in expr '(:english :english))

           (string? (fs/get-in expr '(:english :infinitive)))
           (fs/get-in expr '(:english :infinitive))

           (and (string? (fs/get-in expr '(:english :a :english)))
                (string? (fs/get-in expr '(:english :b :english))))
           (str (fs/get-in expr '(:english :a :english))
                " "
                (fs/get-in expr '(:english :b :english)))


           (and (string? (fs/get-in expr '(:english :a :infinitive)))
                (string? (fs/get-in expr '(:english :b))))
           (str (fs/get-in expr '(:english :a :infinitive))
                " "
                (fs/get-in expr '(:english :b)))

           (and (string? (fs/get-in expr '(:english :a :infinitive)))
                (string? (fs/get-in expr '(:english :b :a :infinitive)))
                (string? (fs/get-in expr '(:english :b :b))))
           (str (fs/get-in expr '(:english :a :infinitive))
                " "
                (fs/get-in expr '(:english :b :a :infinitive))
                " "
                (fs/get-in expr '(:english :b :b)))

           (string? (fs/get-in expr '(:english :a :english)))
           (str (fs/get-in expr '(:english :a :english)) "...")

           (string? (fs/get-in expr '(:english :a :infinitive)))
           (str (fs/get-in expr '(:english :a :infinitive)) "...")

           true
           (fs/get-in expr '(:english))))

         italian
         (capitalize
          (cond
           (string? (fs/get-in expr '(:italian)))
           (fs/get-in expr '(:italian))

           (string? (fs/get-in expr '(:italian :italian)))
           (fs/get-in expr '(:italian :italian))

           (string? (fs/get-in expr '(:italian :infinitive)))
           (fs/get-in expr '(:italian :infinitive))

           (and
            (string? (fs/get-in expr '(:italian :a :italian)))
            (string? (fs/get-in expr '(:italian :b :italian))))
           (str
            (fs/get-in expr '(:italian :a :italian)) " "
            (fs/get-in expr '(:italian :b :italian)))

           (string? (fs/get-in expr '(:italian :a)))
           (str (fs/get-in expr '(:italian :a)) "...")

           (string? (fs/get-in expr '(:italian :a :italian)))
           (str (fs/get-in expr '(:italian :a :italian)) "...")

           (and (string? (fs/get-in expr '(:italian :a :infinitive)))
                (string? (fs/get-in expr '(:italian :b))))
           (str (fs/get-in expr '(:italian :a :infinitive))
                " "
                (fs/get-in expr '(:italian :b)))

           (string? (fs/get-in expr '(:italian :a :infinitive)))
           (str (fs/get-in expr '(:italian :a :infinitive)) "...")

           true
           (fs/get-in expr '(:italian)))
          )
         ]
     (string/trim
      (str italian " (" english ").")))))

;;; e.g.:
;;; (formattare (over (over s (over (over np lexicon) (lookup {:synsem {:human true}}))) (over (over vp lexicon) (over (over np lexicon) lexicon))))
;; TO DO: move this to morphology.clj since it has to do with surface representation.
(defn formattare [expressions]
  "format a bunch of expressions (feature-structures) showing just the italian (and english in parentheses)."
  (do
    (if (map? expressions)
      ;; wrap this single expression in a list and re-call.
      (list (formattare-1 expressions))
      (cond (nil? expressions) nil
            (fs/fail? expressions)
            ":fail"
            (= (.size expressions) 0) nil
            true
            (cons
             (formattare-1 (first expressions))
             (formattare (rest expressions)))))))

(defn fo [expressions]
  (formattare expressions))

(defn unify-and-merge [parent child1 child2]
  (let [unified
        (unify parent
               {:1 child1
                :2 child2}
               {:1 {:synsem {:sem (lex/sem-impl (fs/get-in child1 '(:synsem :sem)))}}
                :2 {:synsem {:sem (lex/sem-impl (fs/get-in child2 '(:synsem :sem)))}}})
        fail (fs/fail? unified)]
    (if (= fail true)
      :fail
      (merge unified
             {:italian (morph/get-italian
                        (fs/get-in unified '(:1 :italian))
                        (fs/get-in unified '(:2 :italian)))
              :english (morph/get-english
                        (fs/get-in unified '(:1 :english))
                        (fs/get-in unified '(:2 :english)))}))))

(defn unify-lr-hc [parent head comp]
  (let [with-head (unify parent
                         {:head head})
        with-comp (unify parent
                         {:head comp})
        unified
        (unify parent
               {:head head}
               {:comp comp}
               {:head {:synsem {:sem (lex/sem-impl (fs/get-in head '(:synsem :sem)))}}
                :comp {:synsem {:sem (lex/sem-impl (fs/get-in comp '(:synsem :sem)))}}})]
    (if (fs/fail? unified)
      :fail
      (merge unified
             {:italian (morph/get-italian
                        (fs/get-in unified '(:1 :italian))
                        (fs/get-in unified '(:2 :italian)))
              :english (morph/get-english
                        (fs/get-in unified '(:1 :english))
                        (fs/get-in unified '(:2 :english)))}))))

;; TODO: use multiple dispatch.
(defn over2 [parent child1 child2]
  (if (vector? child1)
    (do
      (log/debug (str "over2 with child1 size: " (.size child1)))))
  (if (vector? child2)
    (do
      (log/debug (str "over2 with child2 size: " (.size child2)))))

  (cond

   (vector? parent)
   (over2 (lazy-seq parent) child1 child2)

   (vector? child1)
   (over2 parent (lazy-seq child1) child2)

   (vector? child2)
   (over2 parent child1 (lazy-seq child2))

   (= (type child1) java.lang.String)
   (over2 parent (lex/it child1) child2)

   (= (type child2) java.lang.String)
   (over2 parent child1 (lex/it child2))

   (nil? parent)
   nil

   (and (seq? parent)
        (= (.size parent) 0))
   nil

   (nil? child1)
   nil

   (nil? child2)
   nil

   (and (seq? child1)
        (= (.size child1) 0))
   nil

   (and (seq? child2)
        (= (.size child2) 0))
   nil

   (or (set? parent) (seq? parent))
   (lazy-cat (over2 (first parent) child1 child2)
             (over2 (rest parent) child1 child2))

   (or (set? child1) (seq? child1))
   (do
     (log/info (str (fs/get-in parent '(:comment)) ":child1: " (.size child1)))
     (lazy-cat (over2 parent (first child1) child2)
               (over2 parent (rest child1) child2)))

   (or (set? child2) (seq? child2))
   (do
;     (log/info (str "child2: " (.size child2)))
     (lazy-cat (over2 parent child1 (first child2))
               (over2 parent child1 (rest child2))))

   :else ; both parent and children are non-lists.
   ;; First, check to make sure complement matches head's (:synsem :sem) value, if it exists, otherwise, fail.
   (let [unified (unify-and-merge parent child1 child2)
         fail (fs/fail? unified)]
     (if (not fail)
       (list unified)))))

;; TODO: use multiple dispatch.
(defn over-parent-child [parent child]
  (log/debug (str "parent: " parent))
  (log/debug (str "child: " child))
  (cond

   (= (type child) java.lang.String)
   (over-parent-child parent (lex/it child))

   (nil? parent)
   nil

   (and (seq? parent)
        (= (.size parent) 0))
   nil

   (or (set? parent) (seq? parent))
   (lazy-cat
    (remove #(fs/fail? %)
            (over-parent-child (first parent) child))
    (over-parent-child (rest parent) child))

   (nil? child)
   nil

   (and (seq? child)
        (= (.size child) 0))
   nil

   (or (set? child) (seq? child))
   (let [retval (over-parent-child parent (first child))]
     (lazy-cat
      (remove #(fs/fail? %)
              (over-parent-child parent (first child)))
      (over-parent-child parent (rest child))))

   :else ; both parent and child are non-lists.
   ;; First, check to make sure complement matches head's (:synsem :sem) value, if it exists, otherwise, fail.
   (let [
         ;; "add-child-where": find where to attach child (:1 or :2), depending on value of current left child (:1)'s :italian.
         ;; if (:1 :italian) is nil, the parent has no child at :1 yet, so attach new child there at :1.
         ;; Otherwise, a :child exists at :1, so attach new child at :2.
         add-child-where (if (and
                              (not
                               (string?
                                (fs/get-in parent '(:1 :italian))))
                              (not
                               (string?
                                (fs/get-in parent '(:1 :italian :infinitive))))
                              ;; TODO: remove: :root is going away.
                              (not
                               (string?
                                (fs/get-in parent '(:1 :italian :root))))
                              (not
                               (string?
                                (fs/get-in parent '(:1 :italian :italian)))))
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
     ;; wrap the single result in a list so that it can be consumed by (over).
     (list
     (if (= do-match :fail)
       (do
         (log/debug (str "sem-filter: " sem-filter))
         (log/debug (str "comp-sem: " (fs/copy comp-sem)))
         (log/debug "failed match.")
         :fail)
       (let [unified (unify parent
                            {add-child-where
                             (unify
                              (let [sem (fs/get-in child '(:synsem :sem) :notfound)]
                                (if (not (= sem :notfound))
                                  {:synsem {:sem (lex/sem-impl sem)}}
                                  {}))
                             child)})]
         (if (fs/fail? unified)
           (log/debug "Failed attempt to add child to parent: " (fs/get-in parent '(:comment))
                     " at: " add-child-where))

          ;;
          (if (or true (not (fs/fail? unified))) ;; (or true - even if fail, still show it)
            (merge ;; use merge so that we overwrite the value for :italian.
             unified
             {:italian (morph/get-italian
                       (fs/get-in unified '(:1 :italian))
                       (fs/get-in unified '(:2 :italian)))
              :english (morph/get-english
                        (fs/get-in unified '(:1 :english))
                        (fs/get-in unified '(:2 :english)))})
            :fail)))))))

(defn over-parent [parent children]
  (cond
   (= (.size children) 0)
   nil
   true
   (lazy-seq
    (cons
     (over-parent-child parent (first children))
     (over-parent parent (rest children))))))

(defn over [& args]
  "usage: (over parent child) or (over parent child1 child2)"
  (let [parent (first args)
        child1 (second args)
        child2 (if (> (.size args) 2) (nth args 2))]
    (if (not (nil? child2))
      (over-parent-child (over-parent-child parent child1) child2)
      (over-parent-child parent child1))))

;; TODO: figure out how to encode namespaces within rules so we don't need
;; to have this difficult-to-maintain static mapping.
(defn eval-symbol [symbol]
  (cond
   (= symbol 'adjectives) lex/adjectives
   (= symbol 'nouns) lex/nouns
   (= symbol 'intransitive-verbs) lex/intransitive-verbs
   (= symbol 'transitive-verbs) lex/transitive-verbs
   (= symbol 'verbs-taking-pp) lex/verbs-taking-pp
   (= symbol 'modal-verbs) lex/modal-verbs
   (= symbol 'prepositions) lex/prepositions
   (= symbol 'determiners) lex/determiners
   (= symbol 'nominative-pronouns) lex/nominative-pronouns
   (= symbol 'accusative-pronouns) lex/accusative-pronouns
   (= symbol 'proper-nouns) lex/proper-nouns
   (= symbol 'common-nouns) lex/common-nouns
   (= symbol 'verbs) lex/verbs

   (= symbol 'nbar) gram/nbar
   (= symbol 'np) gram/np
   (= symbol 'prep-phrase) gram/prep-phrase
                                        ; doesn't exist yet:
                                        ;   (= symbol 'vp-infinitive-intransitive) gram/vp-infinitive-intransitive
   (= symbol 'vp-infinitive-transitive) gram/vp-infinitive-transitive

   (= symbol 'vp) gram/vp
   (= symbol 'vp-present) gram/vp-present
   (= symbol 'vp-past) gram/vp-past

   (= symbol 'aux-verbs) (list lex/aux-verbs)
   (= symbol 'essere-aux) (list lex/essere-aux)
   (= symbol 'avere-aux) (list lex/avere-aux)


   true (throw (Exception. (str "(italianverbs.generate/eval-symbol could not evaluate symbol: '" symbol "'")))))

(defn random-head-and-comp-from-phrase [phrase expansion]
  (let [head-sym (:head expansion)
        head (eval-symbol (:head expansion))
        comp (:comp expansion)] ;; leave comp as just a symbol for now: we will evaluate it later in random-comp-from-head.
    {:head head
     :head-sym head-sym ;; saving this for diagnostics
     :comp comp}))

(declare head-by-comps)

(defn heads-by-comps [parent heads comps]
  (if (not (empty? heads))
    (if (fs/fail? (first heads))
      (heads-by-comps parent (rest heads) comps)
      (lazy-cat
       (head-by-comps parent (first heads) comps)
       (heads-by-comps parent (rest heads) comps)))))

(defn hc-expands [parent]
  (map (fn [expansion]
         (let [head (eval-symbol (:head expansion))
               comp (eval-symbol (:comp expansion))]
           {:head (if (seq? head) (shuffle
                                   (remove #(fs/fail? %)
                                           (map (fn [head-candidate] (unify head-candidate (fs/get-in parent '(:head))))
                                                head)))
                      (list (unify (fs/get-in parent '(:head)) head)))
            :comp (if (seq? comp) (shuffle
                                   (remove #(fs/fail? %)
                                           (map (fn [comp-candidate] (unify comp-candidate (fs/get-in parent '(:comp))))
                                                comp)))
                      (list (unify (fs/get-in parent '(:comp)) comp)))}))
       (shuffle (vals (fs/get-in parent '(:extend))))))

(defn generate-all-from-expands [parent expands]
  (if (not (empty? expands))
    (lazy-cat
     (heads-by-comps parent
           (:head (first expands))
           (:comp (first expands)))
     (generate-all-from-expands parent (rest expands)))))

(defn generate [parent]
  (if (= :not-exists (fs/get-in parent '(:comment-plaintext) :not-exists))
    (log/debug (str "terminal: no comment.")))
  (if (fs/get-in parent '(:comment-plaintext))
    (log/debug (str "generating: " (fs/get-in parent '(:comment-plaintext)))))
  (cond (= :not-exists (fs/get-in parent '(:comment-plaintext) :not-exists))
        (do
          (log/debug (str "terminal: " (fo parent))))

        (or
         (nil? (fs/get-in parent '(:italian)))
         (and (map? (fs/get-in parent '(:italian :a)))
              (nil? (fs/get-in parent '(:italian :a :italian)))
              (nil? (fs/get-in parent '(:italian :a :infinitive)))))
        (do
          (log/debug (str "generating: " (fs/get-in parent '(:comment-plaintext))))
          (if (= (fs/get-in parent '(:comment-plaintext)) "vp -> head comp")
            (log/debug (str " gen vp: " (fs/get-in parent '(:italian)))))
          (generate-all-from-expands parent (hc-expands parent)))

        :else
        (do
          (log/debug (str "finished: " (fs/get-in parent '(:comment-plaintext)) " -> " (fo parent)))
          nil)))

(defn head-by-comps [parent head comps]
  "Returns a lazy sequence of expressions generated by adjoining (head,comp_i) to parent, for all
   comp_i in comps."
  (log/debug (str "head-by-comps: " (fs/get-in parent '(:comment-plaintext))))
  (if (not (empty? comps))
    (let [comp (first comps)
          head-expand (generate head)
          comp-specification
           (unify comp
                  {:synsem {:sem (lex/sem-impl
                                  (fs/get-in
                                   (unify comp
                                          {:synsem {:sem (lex/sem-impl (fs/get-in comp '(:synsem :sem)))}}
                                          (fs/get-in (unify parent
                                                            {:head head}
                                                            {:head {:synsem {:sem (lex/sem-impl (fs/get-in head '(:synsem :sem)))}}})
                                                     '(:comp)))
                                   '(:synsem :sem)))}})
          logit (log/debug (str "head: " (fo head)))
          logit1 (log/debug (str "pred of comp: " (fs/get-in comp-specification '(:synsem :sem :pred))))
          logit2 (log/debug (str "semantic constraints on comp: " (fs/get-in comp-specification '(:synsem :sem))))
          comp-expand (generate comp-specification)]
      (cond (and (not (nil? head-expand))
                 (not (nil? comp-expand)))
            (lazy-cat
             (heads-by-comps parent head-expand comp-expand)
             (heads-by-comps parent head-expand (rest comps)))

            (not (nil? head-expand))
            (lazy-cat
             (heads-by-comps parent head-expand (list comp))
             (heads-by-comps parent head-expand (rest comps)))

            (not (nil? comp-expand))
            (lazy-cat
             (heads-by-comps parent (list head) comp-expand)
             (heads-by-comps parent (list head) (rest comps)))

            :else
            (remove #(fs/fail? %)
                    (lazy-seq
                     (cons (unify-lr-hc parent head comp)
                           (head-by-comps parent head (rest comps)))))))))

(defn random-sentence []
  (first (take 1 (generate gram/s-present))))

(defn random-sentences [n]
  (repeatedly n (fn [] (random-sentence))))

(defn speed-test [ & times]
  "TODO: show benchmark results and statistics (min,max,95%tile,stddev,etc)"
  (let [times (if (first times) (first times) 10)]
    (dotimes [n times] (time (random-sentence)))))

(defn espressioni []
  (choose-lexeme {:cat :espressioni}))

(defn random-infinitivo []
  (choose-lexeme
   (fs/unify {:cat :verb
           :infl :infinitive}
          config/random-infinitivo)))

(defn random-futuro-semplice [& constraints]
  (let [
        ;; 1. choose a random verb in the passato-prossimo form.
        verb-future (choose-lexeme
                     (fs/unify
                      (if constraints (first constraints) {})
                      {:infl :futuro-semplice}
                      config/futuro-semplice))]
    verb-future))

