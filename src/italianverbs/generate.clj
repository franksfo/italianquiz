(ns italianverbs.generate
  (:use [clojure.stacktrace])
  (:require
   [clojure.tools.logging :as log]
   [italianverbs.lev :as lev]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.morphology :as morph]
   [italianverbs.grammar :as gram]
   [italianverbs.unify :as unify]
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
   (unify/fail? expr)
   "<tt>fail</tt>"
   :else
   (let [english
         (capitalize
          (morph/get-english-1 (unify/get-in expr '(:english))))
         italian
         (capitalize
          (morph/get-italian-1 (unify/get-in expr '(:italian))))]
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
            (unify/fail? expressions)
            ":fail"
            (empty? expressions) nil
            true
            (lazy-seq
             (cons
              (formattare-1 (first expressions))
              (formattare (rest expressions))))))))

(defn fo [expressions]
  (formattare expressions))

(defn unify-and-merge [parent child1 child2]
  (let [unified
        (lexfn/unify parent
                     {:1 child1
                      :2 child2}
                     {:1 {:synsem {:sem (lex/sem-impl (unify/get-in child1 '(:synsem :sem)))}}
                      :2 {:synsem {:sem (lex/sem-impl (unify/get-in child2 '(:synsem :sem)))}}})
        fail (unify/fail? unified)]
    (if (= fail true)
      :fail
      (merge unified
             {:italian (morph/get-italian
                        (unify/get-in unified '(:1 :italian))
                        (unify/get-in unified '(:2 :italian)))
              :english (morph/get-english
                        (unify/get-in unified '(:1 :english))
                        (unify/get-in unified '(:2 :english)))}))))

(defn italian-head-initial? [parent]
  (or
   ;; new-style
   (and (unify/ref= parent '(:head :italian) '(:italian :a))
        (unify/ref= parent '(:comp :italian) '(:italian :b)))
   ;; old-style
   (and (unify/ref= parent '(:head) '(:1))
        (unify/ref= parent '(:comp) '(:2)))))

(defn english-head-initial? [parent]
  (or
   ;; new-style
   (and (unify/ref= parent '(:head :english) '(:english :a))
        (unify/ref= parent '(:comp :english) '(:english :b)))
   ;; old-style
   (and (unify/ref= parent '(:head) '(:1))
        (unify/ref= parent '(:comp) '(:2)))))

(defn unify-comp [parent comp]
  (let [unified
        (lexfn/unify parent
               {:comp comp})]
    (if (unify/fail? unified)
      :fail
      (if true unified
      (merge unified
             (if (italian-head-initial? unified)
               {:italian (morph/get-italian
                          (unify/get-in unified '(:head :italian))
                          (unify/get-in unified '(:comp :italian)))}
               {:italian (morph/get-italian
                          (unify/get-in unified '(:comp :italian))
                          (unify/get-in unified '(:head :italian)))})
             (if (english-head-initial? unified)
               {:english (morph/get-english
                          (unify/get-in unified '(:head :english))
                          (unify/get-in unified '(:comp :english)))}
               {:english (morph/get-english
                          (unify/get-in unified '(:comp :english))
                          (unify/get-in unified '(:head :english)))}))))))


(defn unify-lr-hc-debug [parent head comp]
  (let [with-head (lexfn/unify parent
                               {:head head})
        with-comp (lexfn/unify parent
                               {:head comp})]

    (do
      (log/debug (str "PARENT: " (unify/get-in parent '(:comment-plaintext))))
      (log/debug (str "PARENT FAIL?" (unify/fail? parent)))
      (log/debug (str "HEAD: " (fo head)))
      (log/debug (str "HEAD FAIL?" (unify/fail? head)))
      (log/debug (str "WITH-HEAD: " with-head)))))

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
     (log/debug (str (unify/get-in parent '(:comment)) ":child1: " (.size child1)))
     (lazy-cat (over2 parent (first child1) child2)
               (over2 parent (rest child1) child2)))

   (or (set? child2) (seq? child2))
   (do
     (log/debug (str "child2: " (.size child2)))
     (lazy-cat (over2 parent child1 (first child2))
               (over2 parent child1 (rest child2))))

   :else ; both parent and children are non-lists.
   ;; First, check to make sure complement matches head's (:synsem :sem) value, if it exists, otherwise, fail.
   (let [unified (unify-and-merge parent child1 child2)
         fail (unify/fail? unified)]
     (if (not fail)
       (list unified)))))

(defn add-child-where [parent]
  (if (seq? parent)
    (throw (Exception. (str "add-child-where was passed a seq: " seq " - should only be a map."))))

  (cond

   (morph/phrase-is-finished? parent)
   nil

   (and (italian-head-initial? parent)
        (morph/phrase-is-finished? (unify/get-in parent '(:head))))
   :comp

   (italian-head-initial? parent)
   :head

   (and (not (italian-head-initial? parent))
        (morph/phrase-is-finished? (unify/get-in parent '(:comp))))
   :head

   (not (italian-head-initial? parent))
   :comp

   :else
   (throw (Exception. (str "could not determine where to place child (new-style): "
                           " head italian: " (unify/get-in parent '(:head :italian))
                           " comp italian: " (unify/get-in parent '(:comp :italian))
                           " ref test1: " (unify/ref= parent '(:head :italian) '(:italian :b))
                           " ref test2: " (unify/ref= parent '(:comp :italian) '(:italian :a)))))))

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
    (remove #(unify/fail? %)
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
      (remove #(unify/fail? %)
              (over-parent-child parent (first child)))
      (over-parent-child parent (rest child))))

   :else ; both parent and child are non-lists.
   ;; First, check to make sure complement matches head's (:synsem :sem) value, if it exists, otherwise, fail.
   (let [
         ;; "add-child-where": find where to attach child (:1 or :2), depending on value of current left child (:1)'s :italian.
         ;; if (:1 :italian) is nil, the parent has no child at :1 yet, so attach new child there at :1.
         ;; Otherwise, a :child exists at :1, so attach new child at :2.
         where-child (add-child-where parent)

         ;; head-is-where no longer used with new-style constituency semantics.
         ;; so with new-style, head-is-where is not used.
         head-is-where (if (= (unify/get-in parent '(:head))
                              (unify/get-in parent '(:1)))
                         :1
                         :2)

         child-is-head
         (if (= where-child :head) ;; new-style
           true
           (if (= where-child :comp) ;; new-style
             (= head-is-where where-child))) ;; old-style

         comp (if child-is-head
                ;; child is head, so far complement, return parent's version of complement.
                (unify/get-in parent '(:comp))
                ;; else child is complement, so use it.
                child)

         head (if child-is-head
                ;; child is head, so use it.
                child
                ;; else child is complement, so return parent's version of head.
                (unify/get-in parent '(:head)))

         do-log
         (do
           (log/debug (str "head-is-where: " (unify/get-in parent '(:comment-plaintext)) ":" head-is-where))
           (log/debug (str "child-is-head: " (unify/get-in parent '(:comment-plaintext)) ":" child-is-head)))

         sem-filter (unify/get-in head '(:synsem :subcat :2 :sem)) ;; :1 VERSUS :2 : make this more explicit about what we are searching for.
         comp-sem (unify/get-in comp '(:synsem :sem))
         do-match
         (if (and (not (nil? sem-filter))
                  (not (nil? comp-sem)))
           (unify/match {:synsem (unify/copy sem-filter)}
                        (unify/copy {:synsem (unify/copy comp-sem)})))]
     ;; wrap the single result in a list so that it can be consumed by (over).
     (list
     (if (= do-match :fail)
       (do
         (log/debug (str "sem-filter: " sem-filter))
         (log/debug "failed match.")
         :fail)
       (let [unified (lexfn/unify parent
                                  {where-child
                                   (lexfn/unify
                                    (let [sem (unify/get-in child '(:synsem :sem) :notfound)]
                                      (if (not (= sem :notfound))
                                        {:synsem {:sem (lex/sem-impl sem)}}
                                        {}))
                                    child)})]
         (if (unify/fail? unified)
           (log/debug "Failed attempt to add child to parent: " (unify/get-in parent '(:comment))
                     " at: " where-child))
         unified))))))

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
   (= symbol 'lexicon) (lazy-seq (cons (first lex/lexicon)
                                       (rest lex/lexicon)))
   (= symbol 'tinylex) lex/tinylex
   (= symbol 'adj-phrase) gram/adj-phrase
   (= symbol 'nbar) gram/nbar
   (= symbol 'np) gram/np
   (= symbol 'prep-phrase) gram/prep-phrase
   (= symbol 'intensifier-phrase) gram/intensifier-phrase
                                        ; doesn't exist yet:
                                        ;   (= symbol 'vp-infinitive-intransitive) gram/vp-infinitive-intransitive
   (= symbol 'vp-infinitive-transitive) gram/vp-infinitive-transitive


   (= symbol 'vp) gram/vp
   (= symbol 'vp-present) gram/vp-present
   (= symbol 'vp-past) gram/vp-past
   (= symbol 'vp-pron) gram/vp-pron

   true (throw (Exception. (str "(italianverbs.generate/eval-symbol could not evaluate symbol: '" symbol "'")))))

(declare head-by-comps)

(defn depth-str [depth]
  (if (and (not (nil? depth)) (> depth 0))
    (str "    "
         (depth-str (- depth 1)))
    ""))

(defn heads-by-comps [parent heads comps depth]
  (log/debug (str (depth-str depth) "heads-by-comps begin: " (unify/get-in parent '(:comment-plaintext))))
  (log/debug (str "type of comps: " (type comps)))
  (if (map? comps)
    (log/debug (str "the comp is a map: " comps)))
  (if (not (empty? heads))
    (log/debug (str "heads-by-comps first heads: " (fo (first heads))))
    (log/debug (str "heads-by-comps no more heads.")))

  (log/debug (str "HEADS-BY-COMPS: fail status of first heads: " (unify/fail? (first heads))))

  (if (not (empty? heads))
    (if (unify/fail? (first heads))
      (do
        (log/debug (str "heads-by-comps: " (unify/get-in parent '(:comment-plaintext)) ": first head: " (first heads) "  is fail; continuing."))
        (heads-by-comps parent (rest heads) comps depth))
      (let [unified
            (lexfn/unify parent
                         (lexfn/unify {:head (first heads)}
                                      {:head {:synsem {:sem (lex/sem-impl (unify/get-in (first heads) '(:synsem :sem)))}}}))
            unified-parent
            (if (not (unify/fail? unified))
              unified
              :fail)]

      (if (unify/fail? unified-parent)
        (do
          (log/debug (str "heads-by-comps: " (unify/get-in parent '(:comment-plaintext)) ": first head: " (first heads) "  is fail; continuing."))
          (log/debug (str "parent head: " (unify/get-in parent '(:head :english))))
          (log/debug (str "failed head: " (unify/get-in (first heads) '(:english))))
          (log/debug (str "fail path:" (unify/fail-path unified-parent)))
          (log/debug (str "head english: " (unify/get-in (first heads) '(:english))))
          (log/debug (str "head english b: " (unify/get-in (first heads) '(:english :b))))
          (log/debug (str "parent head english: " (unify/get-in parent '(:head :english))))
          (log/debug (str "parent head english b: " (unify/get-in parent '(:head :english :b))))
          (log/debug (str "unify english: " (lexfn/unify (unify/get-in (first heads) '(:english))
                                                        (unify/get-in parent '(:head :english)))))
          (log/debug (str "unify head: " (lexfn/unify (first heads)
                                                     (unify/get-in parent '(:head)))))
;          (throw (Exception. (str "failed to unify head.")))
          (heads-by-comps parent (rest heads) comps depth))

        (do
          (log/debug (str "heads-by-comps: " (unify/get-in parent '(:comment-plaintext)) " first head: " (fo (first heads))))
          (lazy-cat
           (let [comp-cat (unify/get-in unified-parent '(:comp :synsem :cat))
                 comp-synsem (unify/get-in unified-parent '(:comp :synsem))]
             (head-by-comps unified-parent
                            (first heads)
                            (filter (fn [lex]
                                      (not (unify/fail? (lexfn/unify (unify/get-in lex '(:synsem)) comp-synsem))))
                                    comps)
                            depth
                            (morph/phrase-is-finished? (first heads))))
           (heads-by-comps parent (rest heads) comps depth))))))))

(defn lazy-head-filter [parent expansion sem-impl heads]
  (if (not (empty? heads))
    (let [head-candidate (first heads)
          result (lexfn/unify head-candidate
                              (lexfn/unify
                               (do (log/debug (str "trying head candidate of " (unify/get-in parent '(:comment-plaintext)) " : " (fo head-candidate)))
                                   (lexfn/unify
                                    sem-impl
                                    (unify/get-in parent '(:head))))))]
      (if (not (unify/fail? result))
        (lazy-seq
         (cons result
               (lazy-head-filter parent expansion sem-impl (rest heads))))
        (lazy-head-filter parent expansion sem-impl (rest heads))))))

(defn lazy-head-expands [parent expansion]
  (let [head (eval-symbol (:head expansion))
        sem-impl {:synsem {:sem (lex/sem-impl
                                 (unify/get-in parent '(:head :synsem :sem)))}}]
    (log/debug (str "doing hc-expands:"
                    (unify/get-in head '(:comment-plaintext))
                    " (" (if (not (seq? head)) (fo head) "(lexicon)") "); for: " (unify/get-in parent '(:comment-plaintext))))
    (if (seq? head)
      ;; a sequence of lexical items: shuffle and filter by whether they fit the :head of this rule.
      (lazy-head-filter parent expansion sem-impl (shuffle head))
      ;; else: treat as rule: should generate at this point.
      (list (lexfn/unify (unify/get-in parent '(:head)) head)))))

(defn hc-expands [parent expansion depth]
  (log/debug (str (depth-str depth) "hc-expands: " (unify/get-in parent '(:comment-plaintext)) " with expansion: " expansion))
  (if expansion
    (let [head (eval-symbol (:head expansion))
          comp (eval-symbol (:comp expansion))]
      (log/debug (str "doing hc-expands:"
                       (unify/get-in head '(:comment-plaintext))
                       " (" (if (not (seq? head)) (fo head) "(lexicon)") "); for: " (unify/get-in parent '(:comment-plaintext))))
      (let [head (lazy-head-expands parent expansion)]
        {:head head
         :comp
         (do
           (log/debug (str "hc-expands: head: " head))
           (log/debug (str "hc-expands: comp: " comp))
           (let [compx (:comp-expands head)]
             (log/debug (str "hc-expands: compx: " compx))
             (if (seq? comp) (shuffle comp)
                 (list (lexfn/unify (unify/get-in parent '(:comp)) comp)))))}))))

(defn hc-expand-all [parent extend-vals depth]
  (if (not (empty? extend-vals))
    (lazy-seq
     (cons
      (do
        (log/debug (str "hc-expand-all: extend: " (first extend-vals)))
        (hc-expands parent (first extend-vals) depth))
      (hc-expand-all parent (rest extend-vals) depth)))))

(defn comps-of-expand [expand]
  (:comp expand))

(defn generate [parent & [ hc-exps depth shuffled-expansions]]
  (if (nil? (:extend parent))
    (throw (Exception. (str "Parent: " (unify/get-in parent '(:comment-plaintext)) " did not supply any :extend value, which (generate) needs in order to work.")))
    (let [depth (if depth depth 0)
          hc-expands-orig hc-exps
          shuffled-expansions (if shuffled-expansions shuffled-expansions (shuffle (vals (:extend parent))))
          hc-exps (if hc-exps hc-exps (hc-expand-all parent shuffled-expansions depth))
          parent-finished (morph/phrase-is-finished? parent)]
      (log/debug (str (depth-str depth) "generate: " (unify/get-in parent '(:comment-plaintext)) " with exp: " (first shuffled-expansions)))
      (log/debug (str "cond1: " (= :not-exists (unify/get-in parent '(:comment-plaintext) :not-exists))))
      (log/debug (str "cond2: " (empty? hc-exps)))
      (log/debug (str "cond3: (not parent-finished?)" (not parent-finished)))
      (cond (= :not-exists (unify/get-in parent '(:comment-plaintext) :not-exists))
          nil
          (empty? hc-exps)
          nil
          (not parent-finished)
          (let [expand (first hc-exps)]
            (log/debug "parent not finished.")
            (lazy-cat
             (heads-by-comps parent
                             (:head expand)
                             (comps-of-expand expand)
                             depth)
             (generate parent (rest hc-exps) depth (rest shuffled-expansions))))
          :else
          nil))))

(defn binding-restrictions [sign]
  "Enforce binding restrictions to avoid things like 'I see me'."
  (cond
   (and (or (= (unify/get-in sign '(:synsem :sem :subj :pred)) :io)
            (= (unify/get-in sign '(:synsem :sem :subj :pred)) :noi))
        (or (= (or (unify/get-in sign '(:synsem :sem :obj :pred)) :io)
               (or (unify/get-in sign '(:synsem :sem :obj :pred)) :noi))))
   (do (log/debug "caught binding violation (io/noi)")
       :fail)
   (and (or (= (unify/get-in sign '(:synsem :sem :subj :pred)) :tu)
            (= (unify/get-in sign '(:synsem :sem :subj :pred)) :voi))
        (or (= (or (unify/get-in sign '(:synsem :sem :obj :pred)) :tu)
               (or (unify/get-in sign '(:synsem :sem :obj :pred)) :voi))))
   (do (log/debug "caught binding violation (tu/voi)")
       :fail)
   :else (do (log/debug "binding is ok.")
             sign)))

(defn head-by-comps [parent head comps depth head-is-finished?]
  "Returns a lazy sequence of expressions generated by adjoining (head,comp_i) to parent, for all
   comp_i in comps."
  (let [comp-spec
        (if head-is-finished?
          (unify/get-in
           (lexfn/unify parent
                        {:head head})
           '(:comp))
          :top)]

    (log/debug (str (depth-str depth) "head-by-comps begin: " (unify/get-in parent '(:comment-plaintext)) "; head:" (fo head)))
    (if (unify/fail? parent) (log/debug (str "head-by-comps begin: parent fail? " (unify/fail? parent))))
    (if (not (empty? comps))
      (let [comp (first comps)
            head-expand (unify/get-in head '(:extend))]
        (log/debug (str (depth-str depth) "HEAD-IS-FINISHED?  " (unify/get-in head '(:comment-plaintext)) ":" head-is-finished?))
        (if (not head-is-finished?)
          (log/debug (str (depth-str depth) "head is not finished:  " head)))
        (log/debug (str "non-null head-expand and head not finished? "
                        (unify/get-in parent '(:comment-plaintext)) " : "
                        (and (not (nil? head-expand))
                             (not head-is-finished?))))
        (cond (and (not (nil? head-expand))
                   (not head-is-finished?))
              (do
                (log/debug "1. (hs X cs)")
                (heads-by-comps parent (generate head nil (+ 1 depth)) comps depth))
              :else
              (let [comp-specification
                    (lexfn/unify comp
                                 (lexfn/unify
                                  comp-spec
                                  {:synsem {:sem (lex/sem-impl (lexfn/unify (unify/get-in parent '(:comp :synsem :sem) :top)
                                                                            (unify/get-in comp '(:synsem :sem) :top)))}}))
                    ;; TODO: Move this before computation of comp-specification: if (phrase-is-finished? comp) == true, no need to compute comp-specification.
                    ;; TODO: Do not compute comp-specification if (not head-is-finished?) == true, since head is not complete yet - compute comp-specification should
                    ;;       be deferred until head-is-finished? == true.
                    comp-is-finished? (morph/phrase-is-finished? comp)
                    comp-generate
                    (if (not head-is-finished?)
                      (do
                        (log/debug "deferring comp generation until head generation is done.")
                        comp)
                      (if comp-is-finished?
                        (do
                          (log/debug (str "comp generation for: " (unify/get-in parent '(:comment-plaintext)) " is finished: " (fo comp)))
                          comp)
                        (if (not (unify/fail? comp-specification))
                          (do
                            (log/debug (str "generating comp now since head generation is done."))
                            (log/debug (str (depth-str depth) "generating comp: " (unify/get-in comp '(:comment-plaintext)) " for parent: "
                                            (unify/get-in parent '(:comment-plaintext)) " given head: " (fo head)))
                            (let [comp-gen
                                  (generate comp-specification nil (+ 1 depth))]
                              (log/debug (str "generated complements for " (unify/get-in parent '(:comment-plaintext))
                                              " from: " (unify/get-in comp '(:comment-plaintext))))
                              comp-gen)))))]
                (cond
                 (unify/fail? comp-specification)
                 (do
                   (log/debug "2. h X (rest c)")
                   (head-by-comps parent head (rest comps) depth head-is-finished?))

                 (and (not (nil? head-expand))
                      (not head-is-finished?)
                      (not (unify/fail? comp-specification)))
                 (do
                   (log/debug "3. hs X c")
                   (heads-by-comps parent (generate head nil (+ 1 depth)) (lazy-seq (cons comp-specification (rest comps))) depth))

                 (and (not comp-is-finished?)
                      (not (nil? comp-generate)))
                 (do
                   (log/debug "4. lazy-cat (h X cg) (h (rest c)")
                   (lazy-cat
                    (head-by-comps parent head comp-generate depth head-is-finished?)
                    (head-by-comps parent head (rest comps) depth head-is-finished?)))

                 (unify/fail? parent)
                 (do
                   (log/debug "parent fail: " (unify/get-in parent '(:comment-plaintext)))
                   nil)

                 :else
                 (let [result (binding-restrictions (unify-comp parent comp-specification))]
                   (log/debug "5. unify")
                   (if (unify/fail? result)
                     (do
                       (log/debug (str "failed unification: " (fo head) " (" (unify/get-in head '(:comment-plaintext)) ") and " (fo comp-specification)))
                       (head-by-comps parent head (rest comps) depth head-is-finished?))
                     (do

                       (if (or (= (unify/get-in parent '(:comment-plaintext)) "s[present] -> ..")
                               (= (unify/get-in parent '(:comment-plaintext)) "s[future] -> .."))
                         (log/debug (str (depth-str depth) "successful unification: " (fo comp-specification) " and " (fo head) " for " (unify/get-in parent '(:comment-plaintext))))
                         (log/debug (str (depth-str depth) "successful unification: " (fo head) " and " (fo comp-specification) " for " (unify/get-in parent '(:comment-plaintext)))))

                       (lazy-seq
                        (cons result
                              (head-by-comps parent head (rest comps) depth head-is-finished?))))))))))
      (log/debug (str "no more comps to try for parent: "
                      (unify/get-in parent '(:comment-plaintext)))))))

(defn finalize [expr]
  (let [english
        (morph/get-english-1 (unify/get-in expr '(:english)))
        italian
        (morph/get-italian-1 (unify/get-in expr '(:italian)))]
    (merge expr
           {:italian italian
            :english english})))

(defn random-sentence []
  (finalize (first (take 1 (generate
                            (first (take 1 (shuffle
                                            (list gram/s-present
                                                  gram/s-future
                                                  )))))))))

(defn random-sentences [n]
  (repeatedly n (fn [] (random-sentence))))

(defn speed-test [ & times]
  "TODO: show benchmark results and statistics (min,max,95%tile,stddev,etc)"
  (let [times (if (first times) (first times) 10)]
    (dotimes [n times] (time (random-sentence)))))

(defn espressioni []
  (lexfn/choose-lexeme {:cat :espressioni}))

(defn random-infinitivo []
  (lexfn/choose-lexeme
   (unify/unify {:cat :verb
           :infl :infinitive}
          config/random-infinitivo)))

(defn random-futuro-semplice [& constraints]
  (let [
        ;; 1. choose a random verb in the passato-prossimo form.
        verb-future (lexfn/choose-lexeme
                     (unify/unify
                      (if constraints (first constraints) {})
                      {:infl :futuro-semplice}
                      config/futuro-semplice))]
    verb-future))

