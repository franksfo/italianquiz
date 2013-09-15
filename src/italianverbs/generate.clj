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
    (log/debug (str "moreover-head (candidate) parent sem: " (unify/get-in parent '(:synsem :sem) :wtf)))
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
          :fail))))

(defn moreover-comp [parent child]
  (do
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
          :fail))))

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

(defn gen14-inner [phrase-with-head complements complement-filter-fn sent-impl recursion-level]
  (let [debug (log/debug (str "gen14-inner begin: recursion level: " recursion-level))
        debug (log/debug (str "gen14-inner phrase-with-head: " (fo phrase-with-head)))
        recursion-level (+ 1 recursion-level)
        debug-inner (log/debug (str "gen14-inner: type of complements: " (type complements)))
        debug-inner (log/debug (str "gen14-inner: complement-filter-fn: " complement-filter-fn))
        debug-inner (log/debug (str "gen14-inner: fn? complements: " (fn? complements)))
        debug-inner (log/debug (str "gen14-inner: seq? complements: " (seq? complements)))
        complements (cond (fn? complements)
                          (do (log/debug (str "gen14-inner: treating complements as a fn."))
                              (apply complements (list (apply complement-filter-fn (list phrase-with-head)))))
                          (seq? complements)
                          ;; filter the complements according to the complement-filter-fn
                          (lazy-seq
                          (do
                            (log/debug (str "gen14-inner: applying complement-filter-fn with (after application) type: " (type (apply complement-filter-fn (list phrase-with-head)))))
                            (filter (fn [complement]
                                      (apply
                                       (apply complement-filter-fn (list phrase-with-head))
                                       (list complement)))
                                    complements))
                          :else
                          (do (log/debug "neither fn? or seq?: just returning complements.")
                              complements)))
        empty-complements
        (if (fn? complements)
          (empty? (apply complements nil))
          (empty? complements))
        complement
        (if (fn? complements)
          (first (apply complements nil))
          (first complements))
        debug-inner (log/debug "gen14-inner: before doing (rest complements)..")
        rest-complements
        (if (fn? complements)
          (lazy-seq (rest (apply complements nil)))
          (lazy-seq (rest complements)))
        debug-inner (log/debug "gen14-inner: after doing (rest complements)..")]
    (log/debug (str "gen14-inner: comp-emptiness: " empty-complements))
    (if (not empty-complements)
      (let [comp complement]
        (let [result (sent-impl (moreover-comp
                                 phrase-with-head
                                 comp))]
          (if (not (unify/fail? result))
            (do
              (log/debug (str "gen14-inner: unifies: recursion level: " recursion-level))
              (log/debug (str "gen14-inner: unifies head: " (fo phrase-with-head)))
              (log/debug (str "gen14-inner: unifies comp: " (fo comp)))
              (if (= \c (nth (get-in phrase-with-head '(:comment)) 0))
                ;; italian comp first.
                (log/info (str "gen14-inner:"
                               (get-in phrase-with-head '(:comment)) " => "
                               (fo comp)
                               " + "
                               (fo (unify/get-in phrase-with-head '(:head))) " => TRUE"))
                ;; italian head first.
                (log/info (str "gen14-inner:"
                               (get-in phrase-with-head '(:comment)) " => "
                               (fo (unify/get-in phrase-with-head '(:head)))
                               " + "
                               (fo comp) " => TRUE")))
              (lazy-seq
               (cons result
                     (gen14-inner phrase-with-head rest-complements complement-filter-fn sent-impl recursion-level))))
            (do
              (log/debug "gen14-inner: fail.")
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
                               (fo comp) " => false")))

              (gen14-inner phrase-with-head rest-complements complement-filter-fn sent-impl recursion-level))))))))

(defn gen14 [phrases heads complements sent-impl recursion-level]
  (log/debug (str "gen14: starting now: recursion-level: " recursion-level))
  (log/debug (str "gen14: type of heads: " (type heads)))
  (log/debug (str "gen14: first phrase: " (unify/get-in (first phrases) '(:comment))))
  (log/debug (str "gen14: fo(first phrase): " (fo (first phrases))))
  (log/debug (str "gen14: type of comps: " (type complements)))

  (let [recursion-level (+ 1 recursion-level)
        heads (cond (fn? heads)
                    (do (log/debug "gen14: treating head's value (fn) as a lazy seq and doing (take 1 (apply nil)) on it to get first of the heads.")
                        (apply heads nil))
                    :else
                    heads)
        head (first heads)
        log-debug (log/debug (if (empty? phrases) (str "phrases is empty: done with gen14 at recursion level: " recursion-level)
                                (str "phrases is not empty: there's at least one phrase to process.")))
        rest-heads (rest heads)]
    (if (and (not (empty? phrases))
             (not (nil? head)))
      (let [debug (log/debug (str "gen14: phrases is non-empty, and head exists, so we will do Ps x Hs x Cs."))
            phrase (first phrases)]
        (lazy-cat
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
                  (let [filter-function (unify/get-in phrase '(:comp-filter-fn))]
                    (gen14-inner phrase-with-head
                                 complements
                                 filter-function
                                 sent-impl 0)))
                (gen14 (list phrase)
                       rest-heads
                       complements
                       sent-impl
                       recursion-level)))
             (do
               (log/debug (str "gen14: FAIL: continuing with rest of heads."))
               (gen14 (list phrase)
                      rest-heads
                      complements
                      sent-impl
                      recursion-level))))
         (gen14 (rest phrases) heads complements sent-impl recursion-level))))))
