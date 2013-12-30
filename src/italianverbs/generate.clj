(ns italianverbs.generate
  (:refer-clojure :exclude [get-in merge resolve find])
  (:use [clojure.stacktrace]
        [italianverbs.morphology :only (fo)])
  (:require
   [clojure.tools.logging :as log]
   [italianverbs.lev :as lev]
   [italianverbs.unify :refer :all]
   [italianverbs.unify :as unify]
   [italianverbs.config :as config]
   [italianverbs.html :as html]
   [italianverbs.over :refer (moreover-head moreover-comp)]
   [italianverbs.search :as search]
   [clojure.string :as string]))

(defn printfs [fs & filename]
  "print a feature structure to a file. filename will be something easy to derive from the fs."
  (let [filename (if filename (first filename) "foo.html")]  ;; TODO: some conventional default if deriving from fs is too hard.
    (spit filename (html/static-page (html/tablize fs) filename))))

(defn plain [expr]
  "simply map expr in a map with one key :plain, whose value is expr.
   workbook/workbookq will format this accordingly."
  {:plain expr})

(defn gen14-inner [phrase-with-head complements complement-filter-fn post-unify-fn recursion-level
                   lexfn-sem-impl [ & filtered-complements]]
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
        complements (if filtered-complements filtered-complements
                        (cond (fn? complements)
                              (do (log/debug (str "gen14-inner: treating complements as a fn."))
                                  ;; probably don't need lazy-seq here, so simply leaving this here, commented out, in case I'm wrong:
                                  ;; (lazy-seq (apply complements (list (apply complement-filter-fn (list phrase-with-head)))))
                                  (apply complements (list phrase-with-head)))

                              (seq? complements)
                              (filter (fn [complement]
                                        (log/debug (str "FILTERING COMPLEMENT: " (fo complement)))
                                        (apply
                                         (fn [x] (apply complement-filter-fn (list phrase-with-head)))
                                         (list complement)))
                                      filtered-complements)
                              :else
                              (let [error-message (str "neither fn? nor seq?: " (type complements))]
                                (do (log/error error-message)
                                    (throw (Exception. error-message))))))
        empty-complements
        (empty? complements)
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
              (moreover-comp phrase-with-head comp lexfn-sem-impl)]
          (if (not (unify/fail? result))
            (do
              (log/debug (str "gen14-inner: unifies: recursion level: " recursion-level))
              (log/debug (str "gen14-inner: unifies head: " (fo phrase-with-head)))
              (log/debug (str "gen14-inner: unifies comp: " (fo comp)))
              ;; test: in italian, is complement first?
              (if (= \c (nth (unify/get-in phrase-with-head '(:comment)) 0))
                ;; yes, italian strings complement is first.
                (log/debug (str "gen14-inner:"
                               (unify/get-in phrase-with-head '(:comment)) " => "
                               (fo comp)
                               " + "
                               (fo (unify/get-in phrase-with-head '(:head))) " => TRUE"))
                ;; italian head first.
                (log/debug (str "gen14-inner:"
                               (unify/get-in phrase-with-head '(:comment)) " => "
                               (fo (unify/get-in phrase-with-head '(:head)))
                               " + "
                               (fo comp) " => TRUE")))
              (let [with-impl (if (and false post-unify-fn) (post-unify-fn result) result)]
                  (if (unify/fail? with-impl)

                    ;; adding comp to this phrase failed: continue with the rest of the complements.
                    (gen14-inner phrase-with-head rest-complements complement-filter-fn post-unify-fn
                                 recursion-level lexfn-sem-impl nil)


                    (lazy-seq
                     (cons
                      with-impl
                      (gen14-inner phrase-with-head rest-complements complement-filter-fn post-unify-fn
                                   recursion-level lexfn-sem-impl rest-complements))))))
            (do
              (log/debug (str "gen14-inner: fail: " result))
              (if (= \c (nth (unify/get-in phrase-with-head '(:comment)) 0))
                ;; comp first ('c' is first character of comment):
                (log/debug (str "gen14-inner :"
                                (unify/get-in phrase-with-head '(:comment)) " => "
                                (fo comp)
                                " + "
                                (fo (unify/get-in phrase-with-head '(:head))) " => FAIL"))
                ;; head first ('c' is not first character of comment):
                (log/debug (str "gen14-inner :"
                               (unify/get-in phrase-with-head '(:comment)) " => "
                               (fo (unify/get-in phrase-with-head '(:head)))
                               " + "
                               (fo comp) " => FAIL.")))

              (lazy-seq (gen14-inner phrase-with-head rest-complements complement-filter-fn post-unify-fn recursion-level
                                      lexfn-sem-impl rest-complements)))))))))

(defn gen14 [phrase heads complements filter-against post-unify-fn recursion-level lexfn-sem-impl]
  (log/debug (str "gen14: phrase: " (:comment phrase) "; heads type: " (type heads)))
;  (log/debug (str "gen14: filter-against: " filter-against))
  (log/debug (str "gen14: fn? heads:" (fn? heads)))
  (log/debug (str "gen14: not empty? heads: " (and (not (fn? heads)) (not (empty? heads)))))
  (if (or (fn? heads) (not (empty? heads)))
    (do
      (log/debug (str "gen14: starting now: recursion-level: " recursion-level))
      (log/debug (str "gen14: type of heads: " (type heads)))
      (log/debug (str "gen14: filter-against: " filter-against))
      (log/debug (str "gen14: phrase: " (unify/get-in phrase '(:comment))))
      (log/debug (str "gen14: fo(first phrase): " (fo phrase)))
      (log/debug (str "gen14: type of comps: " (type complements)))
      (log/debug (str "gen14: emptyness of comps: " (and (not (fn? complements)) (empty? complements))))
      (let [recursion-level (+ 1 recursion-level)
            debug (log/debug (str "gen14: phrase-unify first arg: " (fo phrase)))
            debug (log/trace (str "gen14: phrase-unify second arg: " filter-against))
            debug (log/trace (str "gen14: phrase-unify third arg: "
                                  {:synsem {:sem (lexfn-sem-impl
                                                  (unifyc
                                                   (unify/get-in phrase '(:synsem :sem) :top)
                                                   (unify/get-in filter-against '(:synsem :sem) :top)))}}))

            phrase (unifyc phrase
                           (unifyc
                            filter-against
                            {:synsem {:sem (lexfn-sem-impl
                                            (unifyc
                                             (unify/get-in phrase '(:synsem :sem) :top)
                                             (unify/get-in filter-against '(:synsem :sem) :top)))}}))
            debug (log/debug (str "gen14: unified phrase fail?: " (fail? phrase)))
            debug (log/debug "gen14: TYPE OF HEADS (before eval): " (type heads))
            heads (cond (fn? heads)
                        (let [debug (log/debug "gen14: heads is a fn.")
                              filter-against
                              (unify/get-in phrase
                                            '(:head) :top)]
                          (log/debug (str "gen14: phrase:" (fo phrase)))
                          (log/debug (str "gen14: treating heads as a function and applying against filter:"  filter-against))
                          (apply heads (list filter-against)))
                        :else
                        heads)
            debug (log/debug "gen14: TYPE OF HEADS (after eval): " (type heads))
            head (first heads)]
        (let [check (if (nil? head) (log/warn "gen14: head candidate is null - heads was a function, which, when called, returned an empty set of candidates."))
              logging (log/debug (str "gen14: head candidate: " (fo head)))
              logging (log/debug (str "gen14: phrase: " (unify/get-in phrase '(:comment))))
              phrase-with-head (if (not (nil? head))
                                 (moreover-head phrase head lexfn-sem-impl)
                                 :fail)
              debug (log/debug (str "gen14: phrase-with-head: " (fo phrase-with-head)))
              is-fail? (unify/fail? phrase-with-head)
              debug (log/debug (str "gen14: head is-nil? " (nil? head)))
              debug (log/debug (str "gen14: phrase-with-head is-fail? " is-fail?))
              debug (log/debug (str "comp-filter-fn: " (unify/get-in phrase '(:comp-filter-fn))))
              debug (let [complement-filter-function (unify/get-in phrase '(:comp-filter-fn))
                          applied-complement-filter-fn (apply
                                                        complement-filter-function
                                                        (list phrase-with-head))]
                      (log/debug (str "applied-complement-filter-fn type: " (type applied-complement-filter-fn))))
              debug (log/debug (str "SURVIVED do apply..."))

             ]
          (if (nil? head)
            nil
            (if (not is-fail?)
              (do
                (lazy-cat
                 (let [complement-filter-function (unify/get-in phrase '(:comp-filter-fn))

                       ;;
                       ;; you cannot log/ anything here: even if disabled at runtime with (if false)
                       ;; as below:
                       ;;   debug (if false (log/debug "hello"))

                       ;; TODO: improve the following comment: not sure what it means anymore.
                       ;; enhance with (get-in phrase-with-head's value for the possible complements:
                       ;; TODO: this seems wrong: we are applying
                       ;; a function to some arg (list phrase-with-head),
                       ;; and then applying the result (complement-filter-function)
                       ;; to the same arg (list phrase-with-head).
                       applied-complement-filter-fn (apply
                                                     complement-filter-function
                                                     (list phrase-with-head))]
                   (gen14-inner phrase-with-head
                                complements
                                (apply applied-complement-filter-fn (list phrase-with-head))
                                post-unify-fn 0 lexfn-sem-impl nil))
                 (gen14 phrase
                        (rest heads)
                        complements
                        filter-against
                        post-unify-fn
                        recursion-level
                        lexfn-sem-impl)))
              (do
                (log/debug (str "gen14: head unification failed: trying rest of heads."))
                (gen14 phrase
                       (rest heads)
                       complements
                       filter-against
                       post-unify-fn
                       recursion-level
                       lexfn-sem-impl)))))))
    (do
      (log/debug (str "gen14: done.")))))

;; see example usage in rules.clj.
(defmacro rewrite-as [name value]
  (if (ns-resolve *ns* (symbol (str name)))
    `(def ~name (cons ~value ~name))
    `(def ~name (list ~value))))

(defn log-candidate-form [candidate & [label]]
  (cond (and (map? candidate)
             (:schema candidate)
             (:head candidate)
             (:comp candidate))
        (if (= \c (nth (str (:schema candidate)) 0))
          ;; complement is first (in italian)
          (str (if label (:label candidate) " -> ")
               "C:" (:comp candidate) "(" (fo (:head candidate)) ") "
               "H:" (:head candidate) "(" (fo (:comp candidate)))
          ;; head is first (in italian)
          (str (if label (:label candidate) " -> ")
               "H:" (:head candidate) "(" (fo (:head candidate)) ") "
               "C:" (:comp candidate) "(" (fo (:comp candidate)) ")"))
        (map? candidate)
        (str (if label (str label)))
        true
        (str (if label (str label)))))

(declare generate)

(defn generate-from-candidate [candidate label filter-against lexfn-sem-impl]
  (log/debug "generate-from-candidate: candidate with type: " (type candidate))
  (log/debug "generate-from-candidate: map? candidate: " (map? candidate))
  (log/debug "generate-from-candidate: (nil? :schema) candidate: " (nil? (:schema candidate)))
  (log/debug "generate-from-candidate: (nil? filter-against)" (nil? filter-against))
  (cond (and (symbol? candidate)
             (seq? (eval candidate)))
        (generate
         (lazy-shuffle
          (eval candidate))
         (str label " -> " candidate)
         filter-against lexfn-sem-impl)

        (and (map? candidate)
             (not (nil? (:schema candidate))))
        (let [schema (:schema candidate)
              head (:head candidate)
              comp (:comp candidate)]
          (log/debug (str "generate: candidate is a schema: " schema))
          ;; schema is a tree with 3 nodes: a parent and two children: a head child, and a comp child.
          ;; all possible schemas are defined above, after the "BEGIN SCHEMA DEFINITIONS" comment.
          ;; in a particular order (i.e. either head is first or complement is first).
          ;; head is either 1) or 2):
          ;; 1) a rule consisting of a schema, a head rule, and a comp rule.
          ;; 2) a sequence of lexemes.

          ;; comp is either 1) or 2):
          ;; 1) a rule consisting of a schema, a head rule, and a comp rule.
          ;; 2) a sequence of lexemes.


          ;; do some type checks
          (if (not (symbol? schema))
            (throw (Exception. (str "schema is not a symbol: " schema))))

          (log/debug (str "going to call gen14 from g-f-c: filter-against: " filter-against))
          (log/debug (str "going to call gen14 from g-f-c: schema: " schema))
          (log/debug (str "going to call gen14 from g-f-c: head: " head))

          ;; (eval schema) is a 3-node tree (parent and two children) as described
          ;; above: schema is a symbol (e.g. 'cc10 whose value is the tree, thus
          ;; allowing us to access that value with (eval schema).
          (gen14 (unify/copy (eval schema))
                 ;; head (1) (see below for complements)
                 (fn [inner-filter-against]
                   (let [intermediate (unifyc filter-against
                                              {:head inner-filter-against})
                         shuffled-heads (lazy-shuffle (eval head))]
                     (generate shuffled-heads
                              (if false ;; show or don't show schema (e.g. cc10)
                                (str label ":" schema " -> {H:" head "}")
                                (str label " -> {H:" head "}"))
                              (unify/get-in intermediate
                                            '(:head) :top)
                              lexfn-sem-impl)))

                 ;; complement: filter-by will filter candidate complements according to each head
                 ;; generated in immediately above, in (1).
                 (fn [parent-with-head]
                   (generate (if (symbol? comp)
                              (lazy-shuffle (eval comp)) (lazy-shuffle comp))
                            (if false ;; show or don't show schema (e.g. cc10)
                              (str label " : " schema " -> {C:" comp "}")
                              (str label " -> {C: " comp "}"))
                            (unify/get-in
                             (unifyc filter-against
                                     parent-with-head) '(:comp) :top)
                            lexfn-sem-impl))
                 filter-against
                 (:post-unify-fn candidate)
                 0
                 lexfn-sem-impl))
        (and (map? candidate)
             (not (nil? filter-against)))
        (let [result (unifyc filter-against candidate)
              debug (log/debug (str "result of filter-against and candidate unified successfully? " (not (fail? result))))]
          (if (not (unify/fail? result))
            (do (log/info (str "generate: " (log-candidate-form candidate label) " -> " (fo candidate)))
                (list result))
            (do (log/debug (str "generate: " (log-candidate-form candidate label) " -> " (fo candidate) ": failed."))
                nil)))
        (and (map? candidate)
             (nil? filter-against))
        (let [result candidate]
          (if (not (unify/fail? result))
            (do (log/info (str "generate: " (log-candidate-form candidate label) " -> " (fo candidate)))
                (list result))
            (do (log/debug (str "generate: " (log-candidate-form candidate label) " -> " (fo candidate) ": failed."))
                nil)))
        true (throw (Exception. (str "don't know what to do with this; type=" (type candidate))))))

(defn generate [alternatives label filter-against lexfn-sem-impl]
  (if (and (not (empty? alternatives))
           (first alternatives));; TODO: (first alternatives) is redundant?
    (let [candidate (first alternatives)
          constraints-feature
          (cond (and (symbol? candidate)
                     (map? (eval candidate)))
                (:constraints (eval candidate))
                (map? candidate)
                (:constraints candidate)
                true
                :top)

          ;; TODO: rather than (first), treat as lazy-sequence: make constraints a first-order param
          ;; of (generate), and (take 1) on this param to get the constraints-choose-one.
          constraints-choose-one
          (if (set? constraints-feature)
            (first (shuffle (expand-disj constraints-feature)))
            :top)

          filter-against (do
                           (log/debug (str "using constraints-choose-one: " constraints-choose-one))
                           (unifyc filter-against constraints-choose-one))

          label (if label label (if (map? label) (:label candidate)))
          debug (log/trace (str "filter-against U constraints: " filter-against))
          debug (if (fail? filter-against)
                  (log/debug (str "WILL IGNORE THIS FAILURE: " filter-against)))
          ]
      (lazy-cat
       (if (not (fail? filter-against))
         (generate-from-candidate candidate label filter-against lexfn-sem-impl))
       (if (not (empty? (rest alternatives)))
         (generate (rest alternatives) label filter-against lexfn-sem-impl))))))

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
