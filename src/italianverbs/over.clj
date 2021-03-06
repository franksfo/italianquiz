(ns italianverbs.over
  (:refer-clojure :exclude [get get-in merge resolve find parents])
  (:require
   [clojure.core :as core] ;; This allows us to use core's get-in by doing "(core/get-in ..)"

   [clojure.set :refer :all]
   [clojure.string :as string]

   [clojure.tools.logging :as log]

   [italianverbs.lexiconfn :refer :all]
   [italianverbs.morphology :refer [finalize fo fo-ps]]
   [italianverbs.unify :refer :all :exclude [unify]]))

;; TODO: need better debugging throughout this file to diagnose generation failures.

;; tree-building functions: useful for developing grammars.

(defn into-list-of-maps [arg]
  (cond

   (seq? arg)
   arg

   (set? arg)
   (seq arg)

   (map? arg)
   (list arg)

   (nil? arg)
   (list :top)

   (keyword? arg)
   (list arg)

   true (throw (Exception. (str "into-map: don't know what to do with a " (type arg) ".")))))

(declare overh)
(declare overc)

(defn over-each-parent-head [parents head]
  (if (not (empty? parents))
    (let [each-parent (first parents)]
      (log/debug (str "over-each-parent-head: each-parent type:" (type (first parents))))
      (log/debug (str "over-each-parent-head: head type:" (type head)))
      (lazy-cat
       (overh each-parent head)
       (over-each-parent-head (rest parents) head)))
    (do
      (log/debug (str "over-each-parent-head: done. returning nil"))
      nil)))

(defn over-each-parent-comp [parents comp]
  (log/trace (str "over-each-parent-comp: parents type: " (type parents)))
  (log/trace (str "over-each-parent-comp: comp type: " (type comp)))
  (if (not (empty? parents))
    (let [each-parent (first parents)]
      (log/trace (str "over-each-parent-comp: each-parent type:" (type (first parents))))
      (log/trace (str "over-each-parent-comp: comp type:" (type comp)))
      (lazy-cat
       (overc each-parent comp)
       (over-each-parent-comp (rest parents) comp)))
    (do
      (log/trace (str "over-each-parent-comp: done. returning nil"))
      nil)))

(defn over-each-head-child [parent children]
  (log/trace (str "over-each-head-child: parent type: " (type parent)))
  (log/trace (str "over-each-head-child: head children type: " (type children)))
  (if (not (empty? children))
    (let [each-child (first children)]
      (log/trace (str "over-each-head-child: each-parent?: " (label-of (first children))))
      (lazy-cat
       (overh parent each-child)
       (over-each-head-child parent (rest children))))
    (do
      (log/trace (str "over-each-head-child: done. returning nil."))
      nil)))

(defn over-each-comp-child [parent children]
  (log/trace (str "over-each-comp-child: parent type: " (type parent)))
  (log/trace (str "over-each-comp-child: comp children type: " (type children)))
  (if (not (empty? children))
    (let [each-child (first children)]
      (log/trace (str "over-each-comp-child: each-parent?: " (label-of (first children))))
      (lazy-cat
       (overc parent each-child)
       (over-each-comp-child parent (rest children))))
    (do
      (log/trace (str "over-each-comp-child: done. returning nil."))
      nil)))

(defn moreover-head [parent child lexfn-sem-impl]
  (do
    (log/debug (str "moreover-head (candidate) parent: " (fo parent)))
    (log/debug (str "moreover-head (candidate) parent sem: " (get-in parent '(:synsem :sem) :no-semantics)))
    (log/debug (str "moreover-head (candidate) head child sem:" (get-in child '(:synsem :sem) :top)))
    (log/debug (str "moreover-head (candidate) head:" (fo child)))
    (let [result
          (unifyc parent
                  (unifyc {:head child}
                          {:head {:synsem {:sem (lexfn-sem-impl (get-in child '(:synsem :sem) :top))}}}))]
      (if (not (fail? result))
        (let [debug (log/debug (str "moreover-head " (get-in parent '(:comment)) " (SUCCESS) result sem: " (get-in result '(:synsem :sem))))
              debug (log/debug (str "moreover-head (SUCCESS) parent (2x) sem: " (get-in parent '(:synsem :sem))))]
          (merge {:head-filled true}
                 result))

        ;; attempt to put head under parent failed: provide diagnostics through log/debug messages.
        ;; TODO: make (fail-path) call part of each log/debug message to avoid computing it if log/debug is not enabled.
        (let [debug (log/debug (str "unifyc arg1(head): " (strip-refs child)))
              debug (log/debug (str "unifyc arg2(head): " (strip-refs {:synsem {:sem (lexfn-sem-impl (get-in child '(:synsem :sem) :top))}})))
              debug (log/debug (str "moreover-head failed: child:" (fo child) "/parent comment=" (get-in parent '(:comment)) "/child phrase:" (get-in child '(:comment))))
              debug (log/debug (str "(strip-refs result):" (strip-refs result)))
              debug (log/debug (str "(strip-refs result italiano):" (get-in (strip-refs result) [:italiano])))
              fail-path (fail-path result)
              debug (log/debug (str " fail-path: " fail-path))
              debug (log/debug (str " path to head-value-at-fail:" (rest fail-path)))
              debug (log/debug (str " head: " (fo child)))
              debug (log/debug (str " head-value-at-fail: " (strip-refs (get-in child (rest fail-path)))))
              debug (log/debug (str " parent-value-at-fail: " (strip-refs (get-in parent fail-path))))]
          (do
            (log/debug (str "moreover-head: fail-path: " fail-path))
            :fail))))))

;; Might be useful to set the following variable to true,
;; if doing grammar development and it would be unexpected
;; to have a failing result from calling (moreover-comp)
;; with certain arguments.
(def ^:dynamic *throw-exception-if-failed-to-add-complement* false)

(defn moreover-comp [parent child lexfn-sem-impl]
  (log/debug (str "moreover-comp parent: " (fo parent)))
  (log/debug (str "moreover-comp comp:" (fo child)))
  (log/debug (str "moreover-comp type parent: " (type parent)))
  (log/debug (str "moreover-comp type comp:" (type child)))

  (let [result
        (unifyc parent
                (unifyc {:comp child}
                        {:comp {:synsem {:sem (lexfn-sem-impl (get-in child '(:synsem :sem) :top))}}}))]

    (if (not (fail? result))
      (let [debug (log/debug (str "moreover-comp (SUCCESS) parent (2x) sem: " (get-in parent '(:synsem :sem))))]
        (let [result
              (merge {:comp-filled true}
                     result)]
          (log/debug (str "moreover-comp (SUCCESS) merged result:(fo) " (fo result))))
        result)
      (do
        (log/debug "moreover-comp: fail at: " (fail-path result))
        (if (and
             *throw-exception-if-failed-to-add-complement*
             (get-in child '(:head)))
          (throw (Exception. (str "failed to add complement: " (fo child) "  to: phrase: " (fo parent)
                                  ". Failed path was: " (fail-path result)
                                  ". Value of parent at path is: "
                                  (get-in parent (fail-path result))
                                  "; Synsem of child is: "
                                  (get-in child '(:synsem) :top)))))
        (log/debug "moreover-comp: complement synsem: " (get-in child '(:synsem) :top))
        (log/debug "moreover-comp:  parent value: " (get-in parent (fail-path result)))
        :fail))))

(defn overh [parent head]
  "add given head as the head child of the phrase: parent."
  (log/debug (str "overh parent type: " (type parent)))
  (log/debug (str "overh head  type: " (type head)))

  (log/debug (str "set? parent:" (set? parent)))
  (log/debug (str "seq? parent:" (seq? parent)))
  (log/debug (str "seq? head:" (seq? head)))
  (log/debug (str "vector? head:" (vector? head)))

  (if (map? parent)
    (if (get-in parent '(:aliases))
      (log/debug (str "overh: parent:" (get-in parent '(:aliases))))
      (if (get-in parent '(:comment))
        (log/debug (str "overh: parent:" (get-in parent '(:comment)))))))
  (if (map? head)
    (if (get-in parent '(:aliases))
      (log/debug (str "overh: parent:" (get-in parent '(:aliases))))
      (if (get-in head '(:comment))
        (log/debug (str "overh: head: " (get-in head '(:comment))))
        (log/debug (str "overh: head: " (fo head))))))

  (cond

   (nil? head)
   nil

   (or
    (seq? parent)
    (set? parent)
    (vector? parent))
   (let [parents (lazy-seq parent)]
     (filter (fn [result]
               (not (fail? result)))
             (over-each-parent-head parents head)))

   (or (set? head)
       (vector? head))
   (do (log/debug "head is a set: converting to a seq.")
       (overh parent (lazy-seq head)))

   (seq? head)
   (let [head-children head]
     (log/debug (str "head is a seq - actual type is " (type head)))
     (filter (fn [result]
               (not (fail? result)))
             (over-each-head-child parent head-children)))

   true
   ;; TODO: 'true' here assumes that both parent and head are maps: make this assumption explicit,
   ;; and save 'true' for errors.
   (let [result (moreover-head parent head sem-impl)
         is-fail? (fail? result)
         label (if (:rule parent) (:rule parent) (:comment parent))]
     (log/debug (str "overh result keys: " (if (map? result) (keys result) "(not a map)")))
     (log/debug (str "overh italian value: " (if (map? result) (get-in result '(:italiano)) "(not a map)")))
     (log/debug (str "overh italian :a value: " (if (map? result) (get-in result '(:italiano :a)) "(not a map)")))
     (log/debug (str "overh italian :b value: " (if (map? result) (get-in result '(:italiano :b)) "(not a map)")))
     (if is-fail?
       (log/debug (str "overh: parent=" label "; head=[" (fo head) "]=> :fail")))

     (if (not is-fail?)
       (log/debug (str "overh: parent=" label "; head=[" (fo head) "]=> " (if is-fail?
                                                                                       ":fail"
                                                                                       (fo result)))))
     (if (not is-fail?)
       (list result)))))

;; Haskell-looking signature:
;; (parent:map) X (child:{set,seq,fs}) => list:map
;; TODO: verify that the above commentn about the signature
;; is still true.
(defn overc [parent comp]
  "add given child as the comp child of the phrase: parent."
  (log/debug (str "overc parent : " (fo-ps parent)))
  (log/debug (str "overc comp : " (fo comp)))

  (log/debug (str "set? parent:" (set? parent)))
  (log/debug (str "seq? parent:" (seq? parent)))
  (log/debug (str "seq? comp:" (seq? comp)))

  (if (map? parent)
    (if (get-in parent '(:comment))
      (log/debug (str "parent:" (get-in parent '(:comment)))))
    (log/debug (str "parent:" (fo parent))))
  (if (map? comp)
    (log/debug (str "comp: " (fo comp))))

  (log/debug (str "type of parent: " (type parent)))
  (log/debug (str "type of comp  : " (type comp)))
  (log/debug (str "nil? comp  : " (nil? comp)))

  (cond
   (nil? comp) nil

   (or
    (seq? parent)
    (set? parent)
    (vector? parent))
   (let [parents (lazy-seq parent)]
     (filter (fn [result]
               (not (fail? result)))
             (over-each-parent-comp parents comp)))

   (future? comp)
   (overc parent (deref comp))

   (or (set? comp)
       (vector? comp))
   (do (log/debug "comp is a set: converting to a seq.")
       (overc parent (lazy-seq comp)))

   (seq? comp)
   (let [comp-children comp]
     (log/debug (str "comp is a seq - actual type is " (type comp)))
     (filter (fn [result]
               (not (fail? result)))
             (over-each-comp-child parent comp-children)))

   true
   (let [result (moreover-comp parent comp sem-impl)
         is-fail? (fail? result)]
     (if is-fail?
       (log/debug (str "overc: parent=" (label-of parent)
                       "; head=[" (fo (get-in parent '(:head)))
                       "]; comp=[" (fo comp) "]=> :fail")))

     (if (not is-fail?)
       (log/debug (str "overc: parent=" (label-of parent)
                      "; head=[" (fo (get-in parent '(:head)))
                      "]; comp=[" (fo comp) "]=> " (fo result))))

     (if (not is-fail?)
       (list result)))))

(defn overhc [parent head comp]
  (overc (overh parent head) comp))

;; TODO: distinguish between when:
;; 1) called with only a child1 (no child2),
;; 2) called with both a child1 and a child2, but child2's supplied value is nil:
;;    should be treated the same as empty list.
(defn over [parents child1 & [child2]]
  (cond (vector? child1)
        (over parents (seq child1) child2)
        (vector? child2)
        (over parents child1 (seq child2))
        true
  (if (nil? child2) (over parents child1 :top)
      (if (map? parents)
        (over (list parents) child1 child2)
        (if (not (empty? parents))
          (let [parent (first parents)]
            (log/debug (str "over: parent: " (first (:aliases parent))))
            (concat
             (cond (and (map? parent)
                        (not (nil? (:serialized parent))))
                   ;; In this case, supposed 'parent' is really a lexical item: for now, definition of 'lexical item' is,
                   ;; it has a non-nil value for :serialized - just return nil, nothing else to do.

                   (throw (Exception. (str "Don't know what to do with this parent: " parent)))

                   ;; if parent is a symbol, evaluate it; should evaluate to a list of expansions (which might also be symbols, etc).
                   (symbol? parent)
                   (over (eval parent) child1 child2)

                   ;; if parent is map, do introspection: figure out the schema from the :schema-symbol attribute,
                   ;; and figure out head-comp ordering from :first attribute.
                   (and (map? parent)
                        (not (nil? (:schema-symbol parent))))
                   (filter (fn [each]
                             (not (fail? each)))
                           (overhc parent
                                   (if (= (:first parent) :head)
                                     child1 child2)
                                   (if (= (:first parent) :head)
                                     child2 child1)))
                   true
                   (throw (Exception. (str "Don't know what to do with parent: " parent))))

             (over (rest parents) child1 child2))))))))
