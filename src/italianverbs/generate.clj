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
    (log/debug (str "moreover-head-diagnostics (candidate) parent: " (fo parent)))
    (log/debug (str "moreover-head-diagnostics (candidate) parent sem: " (unify/get-in parent '(:synsem :sem) :no-semantics)))
    (log/debug (str "moreover-head-diagnostics (candidate) head:" (fo child)))
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
              debug (log/debug (str " fail-path: " fail-path))
              debug (log/debug (str " path to head-value-at-fail:" (rest fail-path)))
              debug (log/debug (str " head: " child))
              debug (log/debug (str " head-value-at-fail: " (unify/get-in child (rest fail-path) :top)))
              debug (log/debug (str " parent-value-at-fail: " (unify/get-in parent fail-path)))]
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
        (let [result
              (merge {:comp-filled true}
                     result)]
          (log/info (str "moreover-comp (SUCCESS) merged result: " (fo result)))
          result))
      (do
        (log/info "moreover-comp: fail at: " (unify/fail-path result))
        (if (unify/get-in child '(:head))
          (throw (Exception. (str "failed to add complement: " (fo child) "  to: phrase: " (fo parent)
                                  ". Failed path was: " (unify/fail-path result)
                                  ". Value of parent at path is: "
                                  (unify/get-in parent (unify/fail-path result))))))
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
                      (log/debug (str "gen14-inner: vector? complements: "
                                      (= (type complements) clojure.lang.PersistentVector)
                                      " with size: " (.size complements))))
        maps-as-complements-not-allowed
        (if (map? complements)
          ;; TODO: support map by simply re-calling with a list with one element: the map.
          (let [error-message (str "complements should be either a sequence or a function: maps not supported at this time.")]
            (log/debug error-message)
            (throw (Exception. error-message))))
        complements (cond (fn? complements)

                          (do (log/debug (str "gen14-inner: treating complements as a fn."))

                              ;; probably don't need lazy-seq here, so simply leaving this here, commented out, in case I'm wrong:
                              ;; (lazy-seq (apply complements (list (apply complement-filter-fn (list phrase-with-head)))))
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
                        (do (log/debug "gen14: treating head's value a fn and doing (take 1 (apply nil)) on it to get first of the heads.")
                            (apply heads nil))
                        :else
                        heads)
            debug (log/debug "HEADS: " (fo heads))
            head (first heads)
            rest-heads (rest heads)]
        (let [check (if (nil? head) (log/warn "head candidate is null - heads was a function, which, when called, returned an empty set of candidates."))
              logging (log/debug (str "gen14: head candidate: " (fo head)))
              logging (log/debug (str "gen14: phrase: " (unify/get-in phrase '(:comment))))
              phrase-with-head (moreover-head phrase head)
              is-fail? (unify/fail? phrase-with-head)
              debug (log/debug (str "gen14: fail? phrase-with-head:"
                                   is-fail?))
              ]
          (if (nil? head)
            nil
            (if (not is-fail?)
              (do
                (log/debug (str "gen14: head: " (fo (dissoc head :serialized))
                                (if (unify/get-in head '(:comment))
                                  (str "(" (unify/get-in head '(:comment))) ")")
                                " added successfully to " (unify/get-in phrase '(:comment)) "."))
                (log/debug (str "gen14: phrase: " (unify/get-in phrase '(:comment)) " => head: " (fo head)
                                (if (unify/get-in head '(:comment))
                                  (str "(" (unify/get-in head '(:comment)) ")")
                                  "")))
                (lazy-cat
                 (do
                   (log/debug (str "gen14: about to call gen14-inner with phrase-with-head: " (fo phrase-with-head) " and complements type= " (type complements)))
                   (if (= (type complements) clojure.lang.PersistentVector)
                     (log/debug (str "gen14: complements is a vector with size: " (.size complements))))
                   (let [complement-filter-function (unify/get-in phrase '(:comp-filter-fn))]
                     (log/info (str "gen14: looking for complements of: " (fo phrase-with-head)))
                     (gen14-inner phrase-with-head
                                  complements
                                  complement-filter-function
                                  post-unify-fn 0 nil)))
                 (do
                   (log/info (str "gen14: done with head: " (fo head) "; doing rest of heads."))
                   (gen14 phrase
                          rest-heads
                          complements
                          post-unify-fn
                          recursion-level))))
              (do
                (log/info (str "gen14: FAIL WITH HEAD: " (fo head)))
                (moreover-head-diagnostics phrase head)
                (gen14 phrase
                       rest-heads
                       complements
                       post-unify-fn
                       recursion-level)))))))))

;; see example usage in grammar.clj.
(defmacro rewrite-as [name value]
  (if (ns-resolve *ns* (symbol (str name)))
    `(def ~name (cons ~value ~name))
    `(def ~name (list ~value))))

;; thanks to Boris V. Schmid for lazy-shuffle:
;; https://groups.google.com/forum/#!topic/clojure/riyVxj1Qbbs
(defn lazy-shuffle [coll]
;  (shuffle coll))
  (let [size (count coll)]
    (if (> size 0)
      (let [rand-pos (rand-int size)
            [prior remainder]
            (split-at rand-pos coll)
            elem (nth coll rand-pos)]
        (log/debug (str "lazy-shuff: element chosen:" elem))
        (lazy-seq
         (cons elem
               (lazy-shuffle (concat prior (rest remainder)))))))))

(defn gen15 [phrase heads comps]
  (do
    (log/debug (str "gen15 start: " (get-in phrase '(:comment)) "," (type heads)))
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
               (log/debug (str "will filter comps using phrase's filter function: " (:comp-filter-fn phrase)))
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
          ;; complement is first (in italian)
          (str (if label (:label candidate) " -> ")
               "C:" (:comp candidate) " "
               "H:" (:head candidate))
          ;; head is first (in italian)
          (str (if label (:label candidate) " -> ")
               "H:" (:head candidate) " "
               "C:" (:comp candidate)))
        (map? candidate)
        (str (if label (str label)))
        true
        (str (if label (str label)))))

(defn gen-all [alternatives label & [filter-against filter-fn]]
  (do
    (log/debug (str "ALTS: " alternatives))
    (log/debug (str "FIRST ALTS: " (first alternatives)))
    (if (not (first alternatives))
      (log/debug "exhausted all possibilities for: " label))

    (if (first alternatives)
    (let [debug (log/debug (str "OK THERE IS ONE ALT AT LEAST."))
          candidate (first alternatives)
          label (if label label (if (map? label) (:label candidate)))]
      (log/debug (str "TYPE OF CANDIDATE: " (type candidate)))
      (if (and (nil? (:schema candidate))
               (map? candidate))
        (log/info (str "gen-all: " label ": " (fo candidate))))
      (if (and (map? candidate)
               (nil? (:schema candidate)));; don't log lexical candidates: too many of them.
        (log/debug (str "gen-all: " (log-candidate-form candidate label))))
      (log/debug (str "gen-all:  type of candidate "
                     (if (symbol? candidate) (str "'" candidate)) ": " (type candidate)))
      (if filter-fn (log/debug (str "gen-all: filter-fn: " filter-fn)))

      (log/debug (str "TYPE OF CANDIDATE2: " (type candidate)))

      (if (and (or filter-fn filter-against)
               (map? candidate)
               (:schema candidate)) ;; a rule, not a lexeme.
        (log/debug (str "gen-all: FILTERING ON CANDIDATE RULE:"
                        (fo candidate))))

      (if (and (map? candidate)
               (:schema candidate))
        (log/debug (str "gen-all: candidate: " candidate " is a rule.")))

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

      (if (and (map? candidate)
               (:post-unify-fn candidate))
        (log/debug (str "gen-all: post-unify filter exists : " (:post-unify-fn candidate))))

      (let [filter-fn (if filter-fn
                        filter-fn ;; use the given filter-fn to filter complement lexemes.
                        (if filter-against
                          ;; create a filter function given the passed filter-against map to filter complement lexemes..
                          (fn [complement-lexeme]
                            (let [debug (log/debug (str "filtering: " complement-lexeme))
                                  debug (log/debug (str "against: " filter-against))
                                  result (lexfn/unify complement-lexeme filter-against)
                                  debug (log/debug (str "result: " result))]
                              (not (unify/fail? result))))

                          ;; no filter was desired by the caller: just use the pass-through filter.
                          ;; TODO: just return nil and don't filter below.
                          (do (log/debug (str "using pass-thru filter for " (log-candidate-form candidate)))
                              (fn [complement-lexeme]
                                (log/debug (str label " : " (fo complement-lexeme) " is passed through."))
                                true))))
            ]
        (log/debug (str "GOT HERE: CANDIDATE: " candidate))
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
                             (eval candidate))
                            (str label " -> " candidate)
                            nil
                            filter-fn))

                         (throw (Exception. (str "candidate: " candidate " did not evaluate to a sequence - please define it as such.")))))

                     (and (map? candidate)
                          (not (nil? (:schema candidate))))
                     (let [debug (log/debug (str "gen-all: " label ": expanding:" candidate))
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
                         (log/debug (str "gen-all: expanding: [" label " -> H: " head "; C: " comp "]"))
                         (log/debug (str "gen-all: expanding: [" (:label candidate) " -> H: " head "; C: " comp "]")))
                       ;; (eval schema) is a 3-node tree (parent and two children) as described
                       ;; above: schema is a symbol (e.g. 'cc10 whose value is the tree, thus
                       ;; allowing us to access that value with (eval schema).
                       (gen17 (eval schema)
                              ;; head (1) (see below for complements)
                              (fn []
                                (log/debug "realizing head arg for gen-17 using symbol: " head)
                                ;(log/debug "type of symbol's eval is:" (type (eval head)))
                                ;(log/debug "count of symbol's eval is:" (count (eval head)))
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
                                           nil
                                           filter-by)))

                              (:post-unify-fn candidate)))

                     (map? candidate)
                     (do
                       (log/debug (str label ": leaf: " (fo candidate) " : filtering it with: " filter-fn))
                       (log/debug (str label ": leaf: " (fo candidate)))
                       (if (not (nil? filter-against)) (log/debug (str label ": candidate is just a plain map:" (fo candidate) " : filtering it against: " filter-against)))
                       (let [filtered (filter filter-fn (list candidate))]
                         (log/debug (str "after filtering: emptiness of singleton list: " (empty? filtered)))
                         filtered))

                     true (throw (Exception. (str "don't know what to do with this; type=" (type candidate)))))]
           lazy-returned-sequence)
         (gen-all (rest alternatives) label filter-against filter-fn)))))))

(defmacro gen-ch21 [head comp]
  `(do ~(log/debug "gen-ch21 macro compile-time.")
       (gen15 ch21
              ~head
              ~comp)))

(defmacro gen-hh21 [head comp]
  `(do ~(log/debug "gen-hh21 macro compile-time.")
       (gen15 hh21
              ~head
              ~comp)))

(defmacro gen-cc10 [head comp]
  `(do ~(log/debug "gen-cc10 macro compile-time.")
       (gen15 cc10
              ~head
              ~comp)))

