(ns italianverbs.generate
  (:use [clojure.stacktrace]
        [italianverbs.morphology :only (fo)]
        [clojure.core :exclude (get-in)])
  (:require
   [clojure.tools.logging :as log]
   [italianverbs.lev :as lev]
   ;; TODO: remove this: generate should not need access to lexfn/ at all.
   [italianverbs.lexiconfn :as lexfn]
   ;; TODO: remove this: generate should not need access to morph/ at all.
   [italianverbs.morphology :as morph]
   [italianverbs.unify :as unify]
   [italianverbs.config :as config]
   [italianverbs.html :as html]
   ;; TODO: remove this: generate should not need access to lex/ at all.
   [italianverbs.lexicon :as lex]
   [italianverbs.search :as search]
   [clojure.string :as string]))

(defn printfs [fs & filename]
  "print a feature structure to a file. filename will be something easy to derive from the fs."
  (let [filename (if filename (first filename) "foo.html")]  ;; TODO: some conventional default if deriving from fs is too hard.
    (spit filename (html/static-page (html/tablize fs) filename))))

(defn unify-and-merge [parent child1 child2]
  (let [unified
        (merge {:extend (:extend parent)}
               (lexfn/unify parent
                          {:1 child1
                           :2 child2}
                          {:1 {:synsem {:sem (lexfn/sem-impl (unify/get-in child1 '(:synsem :sem)))}}
                           :2 {:synsem {:sem (lexfn/sem-impl (unify/get-in child2 '(:synsem :sem)))}}}))
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
  (and (unify/ref= parent '(:head :italian) '(:italian :a))
       (unify/ref= parent '(:comp :italian) '(:italian :b))))

(defn english-head-initial? [parent]
  (and (unify/ref= parent '(:head :english) '(:english :a))
       (unify/ref= parent '(:comp :english) '(:english :b))))


(defn unify-comp [parent comp]
  (let [unified
        (lexfn/unify parent
               {:comp comp})]
    (if (= :fail unified)
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
   ;; First, check to make sure complement matches head's (:synsem
   ;; :sem) value, if it exists, otherwise, :fail.
   (let [unified (unify-and-merge parent child1 child2)
         fail (unify/fail? unified)]
     (if (not fail)
       (list unified)))))

(defn head-filled-in? [phrase]
  (and (not (nil? phrase))
       (= (:head-filled phrase) true)))

(defn comp-filled-in? [phrase]
  (and (not (nil? phrase))
       (= (:comp-filled phrase) true)))

(defn add-child-where [parent]
  (if (seq? parent)
    (throw (Exception. (str "add-child-where was passed a seq: " seq " - should only be a map."))))

  (cond

   (and (head-filled-in? parent)
        (comp-filled-in? parent))
   nil

   (head-filled-in? parent)
   :comp

   (comp-filled-in? parent)
   :head

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

(defn get-path-to-last-subcat [head]
  (let [path-to-3 (unify/get-in head '(:synsem :subcat :3) :notfound)]
    (if (not (= path-to-3 :notfound))
      path-to-3
      (let [path-to-2 (unify/get-in head '(:synsem :subcat :2) :notfound)]
        (if (not (= path-to-2 :notfound))
          path-to-2
          (unify/get-in head '(:synsem :subcat :1) :notfound))))))

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
         ;; "add-child-where": find where to attach child (:head or :comp)
         where-child (add-child-where parent)

         child-is-head (= where-child :head)

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

         path-to-last-subcat
         (get-path-to-last-subcat head)

         do-log
         (do
           (log/debug (str "child-is-head: " (unify/get-in parent '(:comment-plaintext)) ":" child-is-head))
           (log/debug (str "head: " head))
           (log/debug (str "path-to-last-subcat: " path-to-last-subcat)))

         sem-filter (if (not (= path-to-last-subcat :notfound))
                      (unify/get-in head (concat path-to-last-subcat '(:sem))))
         comp-sem (unify/get-in comp '(:synsem :sem))
         do-match
         (if (and (not (nil? sem-filter))
                  (not (nil? comp-sem)))
           (unify/unify {:synsem (unify/copy sem-filter)}
                  (unify/copy {:synsem (unify/copy comp-sem)})))]
     ;; wrap the single result in a list so that it can be consumed by (over).
     (list
     (if (= do-match :fail)
       (do
         (log/debug (str "failed match: sem-filter: " sem-filter " and comp-sem: " comp-sem))
         :fail)
       (let [unified (lexfn/unify parent
                                  (if child-is-head
                                    {:head-filled true}
                                    {:comp-filled true})
                                  {where-child
                                   (lexfn/unify
                                    (let [sem (unify/get-in child '(:synsem :sem) :notfound)]
                                      (if (not (= sem :notfound))
                                        {:synsem {:sem (lexfn/sem-impl sem)}}
                                        {}))
                                    child)})]
         (if (unify/fail? unified)
           (log/debug "Failed attempt to add child:" (fo child) " to parent: " (unify/get-in parent '(:comment))
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

(defn eval-symbol [symbol]
  (cond
   (fn? symbol) (apply symbol nil)
   (= symbol 'lexicon) (lazy-seq (cons (first lex/lexicon)
                                       (rest lex/lexicon)))
   true (throw (Exception. (str "(italianverbs.generate/eval-symbol could not evaluate symbol: '" symbol "'")))))

(declare head-by-comps)

(defn depth-str [depth]
  (if (and (not (nil? depth)) (> depth 0))
    (str "    "
         (depth-str (- depth 1)))
    ""))

(defn che [parent]
  "display some basic info about the sign."
  (if (seq? parent)
    (if (not (nil? (first parent)))
      (lazy-seq
       (cons
        (che (first parent))
        (che (rest parent)))))
    {:sem (get-in parent '(:synsem :sem))
     :italiano-crudo (get-in parent '(:italian))
     :inglese-crudo (get-in parent '(:english))
     :english (morph/get-english (get-in parent '(:english)))
     :italian (morph/get-italian (get-in parent '(:italian)))}))

(defn che-log [parent]
  "log some basic info about the sign."
  (if (seq? parent)
    (if (not (nil? (first parent)))
      (lazy-seq
       (cons
        (che-log (first parent))
        (che-log (rest parent)))))
    {:sem (get-in parent '(:synsem :sem))
     :english (morph/get-english (get-in parent '(:english)))
     :italian (morph/get-italian (get-in parent '(:italian)))}))

(defn heads-by-comps [parent heads comps depth]
  (log/debug (str (depth-str depth) "heads-by-comps begin: " (unify/get-in parent '(:comment-plaintext))))
  (log/debug (str "type of comps: " (type comps)))
  (log/debug (str "type of heads: " (type heads)))
  (if (map? comps)
    (log/debug (str "the comp is a map: " comps)))
  (if (not (empty? heads))
    (log/debug (str "heads-by-comps first heads: " (che-log (first heads))))
    (log/debug (str "heads-by-comps no more heads.")))

  (log/debug (str "HEADS-BY-COMPS: fail status of first heads: " (unify/fail? (first heads))))

  (if (not (empty? heads))
    (if (unify/fail? (first heads))
      (do
        (log/warn (str "heads-by-comps: "
                       (unify/get-in parent '(:comment-plaintext)) ": first head: " (first heads) "  is unexpectedly fail; continuing with rest of heads."))
        (heads-by-comps parent (rest heads) comps depth))
      (let [unified
            (lexfn/unify parent
                       (lexfn/unify {:head (first heads)}
                                  {:head {:synsem {:sem (lexfn/sem-impl (unify/get-in (first heads) '(:synsem :sem)))}}}))
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
                 comp-synsem (unify/get-in unified-parent '(:comp :synsem))
                 head (first heads)
                 head-is-finished? (morph/phrase-is-finished? head)
                 comp-spec (if head-is-finished?
                             (unify/get-in
                              (lexfn/unify parent
                                           {:head head})
                              '(:comp))
                             :top)]
             (head-by-comps unified-parent
                            (first heads)
                            comp-spec
                            (let [filtered-complements
                                  (filter (fn [lex]
                                            (not (unify/fail? (lexfn/unify (unify/get-in lex '(:synsem)) comp-synsem))))
                                          comps)]
                              (log/debug (str  (unify/get-in parent '(:comment-plaintext)) ": size of pre-filtered complements: " (.size comps)))
                              (log/debug (str  (unify/get-in parent '(:comment-plaintext)) ": size of filtered complements: " (.size filtered-complements)))
                              (if (and (> (/ (.size filtered-complements)
                                             (.size comps))
                                          0)
                                       (> 1/20 (/ (.size filtered-complements)
                                                  (.size comps))))
                                (log/warn (str  (unify/get-in parent '(:comment-plaintext)) ": comp filter ratio < 5%: "
                                                (/ (.size filtered-complements) (.size comps)))))
                              filtered-complements)
                            depth
                            head-is-finished?))
           (heads-by-comps parent (rest heads) comps depth))))))))

(defn lazy-head-filter [expansion head-of-parent-and-sem-impl heads]
  (if (not (empty? heads))
    (let [head-candidate (first heads)
          result (lexfn/unify head-candidate
                              head-of-parent-and-sem-impl)]
      (if (unify/fail? result)
        (log/debug (str " head candidate failed: " (fo head-candidate)))
        (log/debug (str " head candidate succeeded: " (fo head-candidate))))
      (if (not (unify/fail? result))
        (lazy-seq
         (cons result
               (lazy-head-filter expansion head-of-parent-and-sem-impl (rest heads))))
        (lazy-head-filter expansion head-of-parent-and-sem-impl (rest heads))))))

(defn lazy-head-expands [parent expansion]
  (let [head (eval-symbol (:head expansion))
        sem-impl {:synsem {:sem (lexfn/sem-impl
                                 (unify/get-in parent '(:head :synsem :sem)))}}]
    (log/debug (str "doing hc-expands:"
                    (unify/get-in head '(:comment-plaintext))
                    " (" (if (not (seq? head)) (fo head) "(lexicon)") "); for: " (unify/get-in parent '(:comment-plaintext))))
    (if (seq? head)
      ;; a sequence of lexical items: shuffle and filter by whether they fit the :head of this rule.
      ;; TODO: pre-compile set of candidates heads for each parent.
      (lazy-head-filter expansion (lexfn/unify sem-impl (unify/get-in parent '(:head))) (shuffle head))
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

(defn generate [parent & [ hc-exps depth shuffled-expansions expansions-map]]
  (if (seq? parent)
    (generate (first parent))
    (if (unify/fail? parent)
      :fail
      (if (nil? (:extend parent))
        (throw (Exception. (str "Parent: " (unify/get-in parent '(:comment-plaintext)) " did not supply any :extend value, which (generate) needs in order to work.")))
        (let [depth (if depth depth 0)
              hc-expands-orig hc-exps

              new-shuffled-expansions
              (do
                (let [retval
                      (if expansions-map
                        (vec (vals (get expansions-map (unify/get-in parent '(:comment-plaintext))))))]
                                        ;                    expansions-map)]
                                        ;                      (shuffle (vals (:np expansions-map))))]
                  (log/debug (str "(new) expansions map: " expansions-map))
                  (log/debug (str "new-shuffled-expansions: " retval))
                  retval))

              shuffled-expansions
              (if (not (nil? new-shuffled-expansions))
                new-shuffled-expansions
                (do
                  (let [retval
                        (if
                            shuffled-expansions shuffled-expansions
                            (shuffle (vals (:extend parent))))]
                    (log/debug (str "shuffled-expansions: " retval))
                    retval)))

              hc-exps (if hc-exps hc-exps (hc-expand-all parent shuffled-expansions depth))
            parent-finished (morph/phrase-is-finished? parent)]
          (log/debug (str (depth-str depth) "generate: " (unify/get-in parent '(:comment-plaintext)) " with exp: " (first shuffled-expansions)))
          (cond (empty? hc-exps)
                (do
                  (log/debug (str "no expansions left to do for "(unify/get-in parent '(:comment-plaintext) :not-exists)))
                  nil)
                (not parent-finished)
                (let [expand (first hc-exps)]
                  (log/debug "parent not finished.")
                  (lazy-cat
                   (heads-by-comps parent
                                   (:head expand)
                                   (comps-of-expand expand)
                                   depth)
                   (generate parent
                             (rest hc-exps) depth (rest shuffled-expansions))))
                :else
                nil))))))

(defn generate-with [parent expands-map]
  (generate parent nil nil nil expands-map))

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

(defn head-by-comps [parent head comp-spec comps depth head-is-finished?]
  "Returns a lazy sequence of expressions generated by adjoining (head,comp_i) to parent, for all
   comp_i in comps."
  (log/debug (str (depth-str depth) "head-by-comps begin: " (unify/get-in parent '(:comment-plaintext)) "; head:" (che-log head)))
  (if (not (empty? comps))
    (let [comp (first comps)
          head-expand (unify/get-in head '(:extend))]
      (log/debug (str (depth-str depth) "HEAD-IS-FINISHED?  " (unify/get-in head '(:comment-plaintext)) ":" head-is-finished?))
      (if (not head-is-finished?)
        (log/debug (str (depth-str depth) "head is not finished:  " (che-log head))))
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
                                {:synsem {:sem (lexfn/sem-impl (lexfn/unify (unify/get-in parent '(:comp :synsem :sem) :top)
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
                      (if (not (= :fail comp-specification))
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
                 (head-by-comps parent head comp-spec (rest comps) depth head-is-finished?))

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
                  (head-by-comps parent head comp-spec comp-generate depth head-is-finished?)
                  (head-by-comps parent head comp-spec (rest comps) depth head-is-finished?)))

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
                     (head-by-comps parent head comp-spec (rest comps) depth head-is-finished?))
                   (do
                     (if (or (= (unify/get-in parent '(:comment-plaintext)) "s[present] -> ..")
                             (= (unify/get-in parent '(:comment-plaintext)) "s[future] -> .."))
                       (log/debug (str (depth-str depth) "successful unification: " (fo comp-specification) " and " (fo head) " for " (unify/get-in parent '(:comment-plaintext))))
                       (log/debug (str (depth-str depth) "successful unification: " (fo head) " and " (fo comp-specification) " for " (unify/get-in parent '(:comment-plaintext)))))
                     (lazy-seq
                      (cons result
                            (head-by-comps parent head comp-spec (rest comps) depth head-is-finished?))))))))))
    (log/debug (str "no more comps to try for parent: "
                    (unify/get-in parent '(:comment-plaintext))))))

(defn espressioni []
  (lexfn/choose-lexeme {:cat :espressioni}))

(defn random-infinitivo []
  (lexfn/choose-lexeme
   (unify/unify {:cat :verb
           :infl :infinitive}
          config/random-infinitivo)))

(defn random-futuro-semplice [& constraints]
  ;; 1. choose a random verb in the passato-prossimo form.
  (let [verb-future (lexfn/choose-lexeme
                     (unify/unify
                      (if constraints (first constraints) {})
                      {:infl :futuro-semplice}
                      config/futuro-semplice))]
    verb-future))

(defn plain [expr]
  {:plain expr})

(defn moreover-head [parent child]
  (do
    (log/debug (str "moreover-head (candidate) parent: " (fo parent)))
    (log/debug (str "moreover-head (candidate) parent sem: " (unify/get-in parent '(:synsem :sem) :no-semantics)))
    (log/debug (str "moreover-head (candidate) head:" (fo child)))
    (let [;parent (unify/copy parent)
                                        ;child (unify/copy child)
          result (lexfn/unify parent
                              {:head child}
                              {:head {:synsem {:sem (lexfn/sem-impl (unify/get-in child '(:synsem :sem)))}}})]
      (if (not (unify/fail? result))
        (let [debug (log/debug (str "moreover-head " (get-in parent '(:comment)) " (SUCCESS) result sem: " (unify/get-in result '(:synsem :sem))))
              debug (log/debug (str "moreover-head (SUCCESS) parent (2x) sem: " (unify/get-in parent '(:synsem :sem))))]
          (merge {:head-filled true}
                 result))
        (let [debug (log/debug (str "moreover-head " (fo child) "/" (get-in parent '(:comment)) "," (fo child) "/" (get-in child '(:comment))))
              fail-path (unify/fail-path result)
              debug (log/debug (str " fail-path: " fail-path))
              debug (log/debug (str " path to head-value-at-fail:" (rest fail-path)))
              debug (log/debug (str " head: " child))
              debug (log/debug (str " head-value-at-fail: " (unify/get-in child (rest fail-path))))
              debug (log/debug (str " parent-value-at-fail: " (unify/get-in parent fail-path)))]
          :fail)))))

(defn moreover-head-diagnostics [parent child]
  (do
    (log/info (str "moreover-head-diagnostics (candidate) parent: " (fo parent)))
    (log/info (str "moreover-head-diagnostics (candidate) parent sem: " (unify/get-in parent '(:synsem :sem) :no-semantics)))
    (log/info (str "moreover-head-diagnostics (candidate) head:" (fo child)))
    (let [;parent (unify/copy parent)
                                        ;child (unify/copy child)
          result (lexfn/unify parent
                              {:head child}
                              {:head {:synsem {:sem (lexfn/sem-impl (unify/get-in child '(:synsem :sem)))}}})]
      (if (not (unify/fail? result))
        (let [debug (log/debug (str "moreover-head-diagnostics " (get-in parent '(:comment)) " (SUCCESS) result sem: " (unify/get-in result '(:synsem :sem))))
              debug (log/debug (str "moreover-head-diagnostics (SUCCESS) parent (2x) sem: " (unify/get-in parent '(:synsem :sem))))]
          (merge {:head-filled true}
                 result))
        (let [debug (log/debug (str "moreover-head-diagnostics " (fo child) "/" (get-in parent '(:comment)) "," (fo child) "/" (get-in child '(:comment))))
              fail-path (unify/fail-path result)
              debug (log/info (str " fail-path: " fail-path))
              debug (log/info (str " path to head-value-at-fail:" (rest fail-path)))
              debug (log/info (str " head: " child))
              debug (log/info (str " head-value-at-fail: " (unify/get-in child (rest fail-path) :top)))
              debug (log/info (str " parent-value-at-fail: " (unify/get-in parent fail-path)))]
          result)))))  ;; note that we return result rather than :fail, for diagnostics. Note that unify/fail? = true for result if we got here.

(defn moreover-comp [parent child]
  (log/debug (str "moreover-comp parent: " (fo parent)))
  (log/debug (str "moreover-comp comp:" (fo child)))
  (let [result
        (lexfn/unify parent
                     {:comp child}
                     {:comp {:synsem {:sem (lexfn/sem-impl (unify/get-in child '(:synsem :sem)))}}})]
    (if (not (unify/fail? result))
      (let [debug (log/debug (str "moreover-comp " (get-in parent '(:comment)) " (SUCCESS) result sem: " (unify/get-in result '(:synsem :sem))))
            debug (log/debug (str "moreover-comp (SUCCESS) parent (2x) sem: " (unify/get-in parent '(:synsem :sem))))]
        (merge {:comp-filled true}
               result))
      (do
        (log/debug "moreover-comp: fail at: " (unify/fail-path result))
        (log/debug "moreover-comp: complement synsem: " (unify/get-in child '(:synsem)))
        (log/debug "moreover-comp:  parent value: " (unify/get-in parent (unify/fail-path result)))
        :fail))))

(defn over3 [parent child]
  (log/debug (str "string? child: " (string? child)))
  (log/debug (str "seq? child: " (string? child)))
  (cond
   (string? child) (map (fn [each-child]
                          (over3 parent each-child))
                        (lex/it1 child))

   (seq? child) (map (fn [each-child]
                       (over3 parent each-child))
                     child)

   (= (unify/get-in parent '(:head-filled)) true) ;; won't work in general: only works if complement is first (e.g. cc10)
   (moreover-comp parent child)

   :else
   (moreover-head parent child)))

(defn gen13 [depth phrases lexicon]
  (if (>= depth 0) ;; screen out negative numbers to prevent infinite recursion.
    (if (> depth 0)
      (concat
       (gen13 (- depth 1) phrases
              lexicon (gen13 (- depth 1) phrases lexicon))
;       (gen13 (- depth 1) phrases
;              (gen13 (- depth 1) phrases lexicon))
;       (gen13 (- depth 1) phrases
;              (gen13 (- depth 1) phrases)
;              (gen13 (- depth 1) phrases)))
       )
      ;; depth == 0: no more recursion.
      (do
          (remove (fn [phr] (unify/fail? phr))
                  (flatten
                   (map (fn [phrase]
                          (if (head-filled-in? phrase)
                            (map (fn [lexeme]
                                   (moreover-comp phrase lexeme))
                                 lexicon)
                            ;; else: head is not filled in: fill it in.
                            (map (fn [lexeme]
                                   (moreover-head phrase lexeme))
                                 lexicon)))
                        phrases)))))))

(defn cleanup [phrases]
  (remove (fn [phr] (or (= (unify/get-in phr '(:english :a)) :top)
                        (= (unify/get-in phr '(:english :b)) :top)
                        (= (unify/get-in phr '(:italian :a)) :top)
                        (= (unify/get-in phr '(:italian :b)) :top)))
          phrases))

(defn double-apply [depth phrases lexicon]
  (cleanup (gen13 0
                  (gen13 0 phrases lexicon)
                  lexicon)))

(defn triple-apply [depth phrases lexicon]
  (cleanup (gen13 0
                  (gen13 1 phrases lexicon)
                  lexicon)))

(defn gen14-inner [phrase-with-head complements complement-filter-fn post-unify-fn recursion-level [ & filtered-complements]]
  (let [debug (log/debug (str "gen14-inner begin: recursion level: " recursion-level))
        debug (log/debug (str "gen14-inner phrase-with-head: " (fo phrase-with-head)))
        recursion-level (+ 1 recursion-level)
        debug-inner (log/debug (str "gen14-inner: type of complements: " (type complements)))
        debug-inner (log/debug (str "gen14-inner: complement-filter-fn: " complement-filter-fn))
        check-filter-fn (if (nil? complement-filter-fn)
                          (let [error-message (str "complement-filter-fn is nil.")]
                            (log/debug error-message)
                            (throw (Exception. error-message))))
        debug-inner (log/debug (str "gen14-inner: fn? complements: " (fn? complements)))
        debug-inner (log/debug (str "gen14-inner: seq? complements: " (seq? complements)))
        debug-inner (log/debug (str "gen14-inner: map? complements: " (map? complements)))
        debug-inner (if (= (type complements)
                           clojure.lang.PersistentVector)
                      (log/debug (str "gen14-inner: vector? complements: " (= (type complements) clojure.lang.PersistentVector) " with size: " (.size complements))))
        maps-as-complements-not-allowed
        (if (map? complements)
          ;; TODO: support map by simply re-calling with a list with one element: the map.
          (let [error-message (str "complements should be either a sequence or a function: maps not supported at this time.")]
            (log/debug error-message)
            (throw (Exception. error-message))))
        complements (cond (fn? complements)
                          (do (log/debug (str "gen14-inner: treating complements as a fn."))
                              (apply complements (list (apply complement-filter-fn (list phrase-with-head)))))
                          (seq? complements)
                          (if filtered-complements
                            filtered-complements
                            ;; filter the complements according to the complement-filter-fn.
                            (filter (fn [complement]
                                      (log/debug (str "FILTERING COMPLEMENT: " complement))
                                      (apply
                                       (apply complement-filter-fn (list phrase-with-head))
                                       (list complement)))
                                    complements))
                          (= (type complements)
                             clojure.lang.PersistentVector)
                          (do (log/debug (str "gen14-inner: filtering vector."))
                              (if filtered-complements
                                filtered-complements
                                (filter (fn [complement]
                                          true)
                                        (list (first complements)))))
                          :else
                          (let [error-message (str "neither fn?, seq? nor vector: " (type complements))]
                            (do (log/error error-message)
                                (throw (Exception. error-message)))))
        empty-complements
        (if (fn? complements)
          (empty? (apply complements nil))
          (empty? complements))
        complement
        (cond (fn? complements)
              (first (apply complements nil))
              (seq? complements)
              (first complements)
              (= (type complements) clojure.lang.PersistentVector)
              (first complements))
        debug-inner (log/debug "gen14-inner: before doing (rest complements)..")
        rest-complements
        (if (fn? complements)
          (lazy-seq (rest (apply complements nil)))
          (lazy-seq (rest complements)))
        debug-inner (log/debug "gen14-inner: after doing (rest complements)..")]
    (log/debug (str "gen14-inner: comp-emptiness: " empty-complements))
    (log/debug (str "(fo phrase-with-head): " (fo phrase-with-head)))
    (log/debug (str "complement(comment): " (unify/get-in complement '(:comment))))
    (log/debug (str "complement: " (fo complement)))
    (if (not empty-complements)
      (let [comp complement]
        (let [result
              (moreover-comp
               phrase-with-head
               comp)]
          (if (not (unify/fail? result))
            (do
              (log/debug (str "gen14-inner: unifies: recursion level: " recursion-level))
              (log/debug (str "gen14-inner: unifies head: " (fo phrase-with-head)))
              (log/debug (str "gen14-inner: unifies comp: " (fo comp)))
              ;; test: in italian, is complement first?
              (if (= \c (nth (get-in phrase-with-head '(:comment)) 0))
                ;; yes, italian strings complement is first.
                (log/debug (str "gen14-inner:"
                               (get-in phrase-with-head '(:comment)) " => "
                               (fo comp)
                               " + "
                               (fo (unify/get-in phrase-with-head '(:head))) " => TRUE"))
                ;; italian head first.
                (log/debug (str "gen14-inner:"
                               (get-in phrase-with-head '(:comment)) " => "
                               (fo (unify/get-in phrase-with-head '(:head)))
                               " + "
                               (fo comp) " => TRUE")))
              (let [with-impl (if post-unify-fn (post-unify-fn result) result)]
                  (if (unify/fail? with-impl)
                    (do (log/warn (str "result: " (fo result) " was not fail, but its (post-unify-fn) returned fail."))
                        (gen14-inner phrase-with-head rest-complements complement-filter-fn post-unify-fn recursion-level nil))
                    (lazy-seq
                     (cons
                      with-impl
                      (gen14-inner phrase-with-head rest-complements complement-filter-fn post-unify-fn recursion-level rest-complements))))))
            (do
              (log/debug (str "gen14-inner: fail: " result))
              (if (= \c (nth (get-in phrase-with-head '(:comment)) 0))
                ;; comp first ('c' is first character of comment):
                (log/debug (str "gen14-inner :"
                                (get-in phrase-with-head '(:comment)) " => "
                                (fo comp)
                                " + "
                                (fo (unify/get-in phrase-with-head '(:head))) " => false"))
                ;; head first ('c' is not first character of comment):
                (log/debug (str "gen14-inner :"
                               (get-in phrase-with-head '(:comment)) " => "
                               (fo (unify/get-in phrase-with-head '(:head)))
                               " + "
                               (fo comp) " => FAIL.")))

              (gen14-inner phrase-with-head rest-complements complement-filter-fn post-unify-fn recursion-level rest-complements))))))))

(defn gen14 [phrase heads complements post-unify-fn recursion-level]
  (if (or (fn? heads) (not (empty? heads)))
    (do
      (log/debug (str "gen14: starting now: recursion-level: " recursion-level))
      (log/debug (str "gen14: type of heads: " (type heads)))
      (log/debug (str "gen14: phrase: " (unify/get-in phrase '(:comment))))
      (log/debug (str "gen14: fo(first phrase): " (fo phrase)))
      (log/debug (str "gen14: type of comps: " (type complements)))
      (log/debug (str "gen14: emptyness of comps: " (and (not (fn? complements)) (empty? complements))))
      (let [recursion-level (+ 1 recursion-level)
            heads (cond (fn? heads)
                        (do (log/debug "gen14: treating head's value (fn) as a lazy seq and doing (take 1 (apply nil)) on it to get first of the heads.")
                            (apply heads nil))
                        :else
                        heads)
            head (first heads)
            rest-heads (rest heads)]
        (let [logging (log/debug (str "gen14: head candidate: " (fo head)))
              logging (log/debug (str "gen14: phrase: " (unify/get-in phrase '(:comment))))
              phrase-with-head (moreover-head phrase head)
              is-fail? (unify/fail? phrase-with-head)
              debug (log/debug (str "gen14: fail? phrase-with-head:"
                                   is-fail?))
              ]
          (if (not is-fail?)
            (do
              (log/debug (str "gen14: head: " (fo (dissoc head :serialized))
                             (if (unify/get-in head '(:comment))
                               (str "(" (unify/get-in head '(:comment))) ")")
                             " added successfully to " (unify/get-in phrase '(:comment)) "."))
              (log/debug (str "gen14: phrase: " (unify/get-in phrase '(:comment)) "=> head: " (fo head)
                             (if (unify/get-in head '(:comment))
                               (str "(" (unify/get-in head '(:comment)) ")")
                               "")))
              (lazy-cat
               (do
                 (log/debug (str "gen14: about to call gen14-inner with phrase-with-head: " (fo phrase-with-head) " and complements type=: " (type complements)))
                 (if (= (type complements) clojure.lang.PersistentVector)
                   (log/debug (str "gen14: complements is a vector with size: " (.size complements))))
                 (let [filter-function (unify/get-in phrase '(:comp-filter-fn))]
                   (gen14-inner phrase-with-head
                                complements
                                filter-function
                                post-unify-fn 0 nil)))
               (gen14 phrase
                      rest-heads
                      complements
                      post-unify-fn
                      recursion-level)))
            (do
              (log/info (str "gen14: FAIL WITH HEAD: " (fo head)))
              (moreover-head-diagnostics phrase head)
              (gen14 phrase
                     rest-heads
                     complements
                     post-unify-fn
                     recursion-level))))))))

;; see example usage in grammar.clj.
(defmacro rewrite-as [name value]
  (if (ns-resolve *ns* (symbol (str name)))
    `(def ~name (cons ~value ~name))
    `(def ~name (list ~value))))

;; thanks to Boris V. Schmid for lazy-shuffle:
;; https://groups.google.com/forum/#!topic/clojure/riyVxj1Qbbs
(defn lazy-shuffle [coll]
  (let [size (count coll)]
    (if (> size 0)
      (let [rand-pos (rand-int size)
            [prior remainder]
            (split-at rand-pos coll)
            elem (nth coll rand-pos)]
        (log/debug (str "lazy-shuff: " (fo elem)))
        (lazy-seq
         (cons elem
               (lazy-shuffle (concat prior (rest remainder)))))))))

(defn gen15 [phrase heads comps]
  (do
    (log/info (str "gen15 start: " (get-in phrase '(:comment)) "," (type heads)))
    (gen14 phrase heads comps nil 0)))

(defn gen17 [phrase heads comps post-unify-fn]
  (log/debug (str "gen17: phrase:" (:comment phrase)))
  (log/debug (str "gen17: seq? heads:" (seq? heads)))
  (log/debug (str "gen17: fn? heads:" (fn? heads)))
  (cond (seq? heads)
        (let [head (first heads)]
          (if head
            (lazy-cat
             (do
               (log/info (str "will filter comps using phrase's filter function: " (:comp-filter-fn phrase)))
               (gen14 phrase (list head) comps post-unify-fn 0)))
             (gen17 phrase (rest heads) comps post-unify-fn)))

        true
        (gen14 phrase heads comps post-unify-fn 0)))

(defn log-candidate-form [candidate & [label]]
  (cond (and (map? candidate)
             (:schema candidate)
             (:head candidate)
             (:comp candidate))
        (if (= \c (nth (str (:schema candidate)) 0))
          (str (if label (:label candidate) " -> ")
               "C:" (:comp candidate) " "
               "H:" (:head candidate))
          (str (if label (:label candidate) " -> ")
               "H:" (:head candidate) " "
               "C:" (:comp candidate)))
        (map? candidate)
        (str (if label (str label)))
        true
        (str (if label (str label)))))

(defn gen-all [alternatives label & [filter-against filter-fn]]
  (if (first alternatives)
    (let [candidate (first alternatives)
          label (if label label (if (map? label) (:label candidate)))]
      (if (and (nil? (:schema candidate))
               (map? candidate))
        (log/debug (str "gen-all: " label ": " (fo candidate))))
      (if (and (map? candidate)
               (nil? (:schema candidate)));; don't log lexical candidates: too many of them.
        (log/debug (str "gen-all: " (log-candidate-form candidate label))))
      (log/debug (str "gen-all:  type of candidate "
                     (if (symbol? candidate) (str "'" candidate)) ": " (type candidate)))
      (if filter-fn (log/debug (str "gen-all: filter-fn: " filter-fn)))

      (if (and (or filter-fn filter-against)
               (map? candidate)
               (:schema candidate)) ;; a rule, not a lexeme.
        (log/debug (str "gen-all: FILTERING ON CANDIDATE RULE:"
                        (fo candidate))))

      (if (and (map? candidate)
               (:schema candidate))
        (log/info (str "gen-all: candidate: " (:schema candidate) " is a rule.")))

      (if (and (nil? filter-fn)
               (nil? filter-against)
               (map? candidate)
               (nil? (:schema candidate))) ;; a lexeme, not a rule.
        (log/debug (str "gen-all: NO FILTERING ON CANDIDATE LEXEME: "
                       (fo candidate))))

      (if (and (nil? filter-fn)
               (nil? filter-against)
               (map? candidate)
               (:schema candidate)) ;; a rule, not a lexeme.
        (log/warn (str "gen-all: NO FILTERING ON CANDIDATE RULE: "
                       candidate)))

      (if (and (map? candidate) (:post-unify-fn candidate))
        (log/debug (str "gen-all: post-unify filter exists : " (:post-unify-fn candidate))))
      (let [filter-fn (if filter-fn
                        filter-fn
                        (if filter-against
                          ;; create a function using the filter-against map that we were given:
                          (fn [x]
                            (let [debug (log/debug (str "filtering: " x))
                                  debug (log/debug (str "against: " filter-against))
                                  result (lexfn/unify x filter-against)
                                  debug (log/debug (str "result: " result))]
                              (not (unify/fail? result))))

                          ;; no filter was desired by the caller: just use the pass-through filter.
                          ;; TODO: just return nil and don't filter below.
                          (do (log/debug (str "using pass-thru filter for " (log-candidate-form candidate)))
                              (fn [complement-lexeme] 
                                (log/info (str label " : " (fo complement-lexeme) " is passed through."))
                                true))))
            ]
        (lazy-cat
         (let [lazy-returned-sequence
               (cond (symbol? candidate)
                     (do
                       (log/debug "candidate is a symbol: " candidate)
                       (log/debug "candidate's eval type is: " (type (eval candidate)))
                       (if (seq? (eval candidate))
                         (do
                           (if (list? (eval candidate))
                             (log/debug "candidate:" candidate " is a list: " (eval candidate))
                             (log/debug "candidate:" candidate " is a seq but not a list (a lazyseq)"))
                           (log/debug (str label " -> " candidate " -> "))
                           (gen-all
                            (lazy-shuffle
                             (filter filter-fn (eval candidate)))
                            (str label " -> " candidate)))))

                     (and (map? candidate)
                          (not (nil? (:schema candidate))))
                     (let [debug (log/debug (str "gen-all: " label " -> " (:label candidate)))
                           schema (:schema candidate)
                           head (:head candidate)
                           comp (:comp candidate)
                           debug (log/debug (str "candidate rewrite rule: " candidate))]

                       ;; schema is a tree with 3 nodes: a parent and two children: a head child, and a comp child.
                       ;; all possible schemas are defined above, after the "BEGIN SCHEMA DEFINITIONS" comment.
                       ;; in a particular order (i.e. either head is first or complement is first).
                       ;; head is either 1) or 2):
                       ;; 1) a rule consisting of a schema, a head rule, and a comp rule.
                       ;; 2) a sequence of lexemes.

                       ;; comp is either 1) or 2):
                       ;; 1) a rule consisting of a schema, a head rule, and a comp rule.
                       ;; 2) a sequence of lexemes.


                       (if (nil? (:label candidate))
                         (log/info (str "gen-all: expanding: [" label " -> H: " head "; C: " comp "]"))
                         (log/info (str "gen-all: expanding: [" (:label candidate) " -> H: " head "; C: " comp "]")))
                       ;; (eval schema) is a 3-node tree (parent and two children) as described
                       ;; above: schema is a symbol (e.g. 'cc10 whose value is the tree, thus
                       ;; allowing us to access that value with (eval schema).
                       (gen17 (eval schema)
                              ;; head (1) (see below for complements)
                              (fn []
                                (gen-all (lazy-shuffle (eval head))
                                         (if false ;; show or don't show schema (e.g. cc10)
                                           (str label ":" schema " -> {H:" head "}")
                                           (str label " -> {H:" head "}"))
                                         nil
                                         filter-fn))

                              ;; complement: filter-by will filter candidate complements according to each head
                              ;; generated in immediately above, in (1).
                              (fn [filter-by]
                                (do
                                  (gen-all (if (symbol? comp)
                                             (lazy-shuffle (eval comp)) (lazy-shuffle comp))
                                           (if false ;; show or don't show schema (e.g. cc10)
                                             (str label " : " schema " -> {C:" comp "}")
                                             (str label " -> {C: " comp "}"))
                                           (if true nil filter-against)
                                           filter-by)))
                              (:post-unify-fn candidate)))

                     (map? candidate)
                     (do
                       (log/debug (str "candidate is just a plain map:" (fo candidate)))
                       (list candidate))

                     true (throw (Exception. (str "don't know what to do with this; type=" (type candidate)))))]
           lazy-returned-sequence)
         (gen-all (rest alternatives) label filter-against filter-fn))))))

(defmacro gen-ch21 [head comp]
  `(do ~(log/info "gen-ch21 macro compile-time.")
       (gen15 ch21
              ~head
              ~comp)))

(defmacro gen-hh21 [head comp]
  `(do ~(log/info "gen-hh21 macro compile-time.")
       (gen15 hh21
              ~head
              ~comp)))

(defmacro gen-cc10 [head comp]
  `(do ~(log/info "gen-cc10 macro compile-time.")
       (gen15 cc10
              ~head
              ~comp)))

