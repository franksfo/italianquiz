(ns italianverbs.over
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [clojure.core :exclude [get-in]]
   [clojure.core :as core] ;; This allows us to use core's get-in by doing "(core/get-in ..)"

   [clojure.set :refer :all]
   [clojure.string :as string]

   [clojure.tools.logging :as log]

   [italianverbs.generate :refer :all]
   [italianverbs.lexicon :refer :all]
   [italianverbs.lexiconfn :refer :all]
   [italianverbs.morphology :refer [finalize fo]]
   [italianverbs.unify :refer :all :exclude [unify]]))

;; tree-building functions: useful for developing grammars.

(defn into-list-of-maps [arg]
  (cond

   (seq? arg)
   arg

   (set? arg)
   (seq arg)

   (map? arg)
   (list arg)

   (string? arg)
   (seq (it arg))

   (nil? arg)
   (list :top)

   (keyword? arg)
   (list arg)

   true (throw (Exception. (str "into-map: don't know what to do with a " (type arg) ".")))))

(defn over-each-parent [parents child1 child2]
  (if (not (empty? parents))
    (let [parent (first parents)]
      (log/debug (str "parent: " parent))
      (lazy-cat
       (cond (and (map? parent)
                  (not (nil? (:serialized parent))))
             ;; In this case, supposed 'parent' is really a lexical item: for now, definition of 'lexical item' is,
             ;; it has a non-nil value for :serialized - just return nil, nothing else to do.
             (throw (Exception. (str "Don't know what to do with this parent: " parent)))

             (and (map? parent)
                  (not (nil? (:schema parent))))
                  ;; figure out whether head is child1 or child2:
             (let [head
                   (cond
                    (= \c (nth (str (:schema parent)) 0))
                    child2

                    (= \h (nth (str (:schema parent)) 0))
                    child1

                    true
                    (throw (Exception. (str "Don't know what the head-vs-complement ordering is for parent: " parent))))
                   comp
                   (if (= head child1)
                     child2 child1)]
               (generate (list parent)
                         "parent" {:head head
                                   :comp comp} sem-impl))

             ;; if parent is a symbol, evaluate it; should evaluate to a list of expansions (which might also be symbols, etc).
             (symbol? parent)
             (over-each-parent (eval parent) child1 child2)

             ;; if parent is map, do introspection: figure out the schema from the :schema-symbol attribute,
             ;; and figure out head-comp ordering from :first attribute.
             (and (map? parent)
                  (not (nil? (:schema-symbol parent))))
             (generate (list (eval (:schema-symbol parent)))
                       "parent"
                       {:head (if (= (:first parent) :head)
                                child1 child2)
                        :comp (if (= (:first parent) :head)
                                child2 child1)}
                       sem-impl)


             true
             (throw (Exception. (str "Don't know what to do with parent: " parent))))

       (over-each-parent (rest parents) child1 child2)))))

(defn over-each-child2 [parents child1s child2]
  (if (not (empty? child1s))
    (lazy-cat
     (over-each-parent parents (first child1s) child2)
     (over-each-child2 parents (rest child1s) child2))))

(defn over-each-child1 [parents child1s child2s]
  (if (not (empty? child2s))
    (lazy-cat
     (over-each-child2 parents child1s (first child2s))
     (over-each-child1 parents child1s (rest child2s)))))

(defn over-gen [parents child1 & [child2]]
  (let [child1 (into-list-of-maps child1)
        child2 (into-list-of-maps child2)]
    (over-each-child1 (if (seq? parents)
                        (shuffle parents)
                        (list parents))
                      child1 child2)))

(defn mylazycat [one two]
  (lazy-cat one two))

(declare overh)
(declare overc)

(defn over-each-parent [parents child]
  (log/debug (str "over-each-parent: parents type: " (type parents)))
  (log/debug (str "over-each-parent: child type: " (type child)))
  (if (not (empty? parents))
    (let [each-parent (first parents)]
      (log/debug (str "over-each-parent: each-parent type:" (type (first parents))))
      (log/debug (str "over-each-parent: child type:" (type child)))
      (lazy-cat
       (overh each-parent child)
       (over-each-parent (rest parents) child)))
    (do
      (log/debug (str "over-each-parent: done. returning nil"))
      nil)))

(defn over-each-head-child [parent children]
  (log/debug (str "over-each-head-child: parent type: " (type parent)))
  (log/debug (str "over-each-head-child: head children type: " (type children)))
  (if (not (empty? children))
    (let [each-child (first children)]
      (log/debug (str "over-each-head-child: each-parent?: " (:comment (first children))))
      (lazy-cat
       (overh parent each-child)
       (over-each-head-child parent (rest children))))
    (do
      (log/debug (str "over-each-head-child: done. returning nil."))
      nil)))

(defn over-each-comp-child [parent children]
  (log/debug (str "over-each-comp-child: parent type: " (type parent)))
  (log/debug (str "over-each-comp-child: comp children type: " (type children)))
  (if (not (empty? children))
    (let [each-child (first children)]
      (log/debug (str "over-each-comp-child: each-parent?: " (:comment (first children))))
      (lazy-cat
       (overc parent each-child)
       (over-each-comp-child parent (rest children))))
    (do
      (log/debug (str "over-each-comp-child: done. returning nil."))
      nil)))

(defn overh [parent head]
  "add given head as the head child of the phrase: parent."
  (log/debug (str "overh parent type: " (type parent)))
  (log/debug (str "overh head  type: " (type head)))

  (log/debug (str "set? parent:" (set? parent)))
  (log/debug (str "seq? parent:" (seq? parent)))
  (log/debug (str "seq? head:" (seq? head)))

  (if (map? parent)
    (if (get-in parent '(:comment))
      (log/debug (str "parent:" (get-in parent '(:comment)))))
    (log/debug (str "parent:" (fo parent))))
  (if (map? head)
    (log/debug (str "head: " (fo head))))

  (cond

   (or
    (seq? parent)
    (set? parent))
   (let [parents (lazy-seq parent)]
     (filter (fn [result]
               (not (fail? result)))
             (over-each-parent parents head)))

   (string? head)
   (overh parent (it head))

   (set? head)
   (do (log/debug "head is a set: converting to a seq.")
       (overh parent (lazy-seq head)))

   (seq? head)
   (let [head-children head]
     (log/debug (str "head is a seq - actual type is " (type head)))
     (filter (fn [result]
               (not (fail? result)))
             (over-each-head-child parent head-children)))

   true
   (do
     (log/debug (str "overh: parent and head are both maps: put head under parent. Parent=" (:comment parent) "; head=" (fo head)))
     (list
      (moreover-head parent head sem-impl)))))

;; Haskell-looking signature:
;; (parent:map) X (child:{set,seq,fs}) => list:map
;; TODO: verify that the above commentn about the signature
;; is still true.
(defn overc [parent comp]
  "add given child as the comp child of the phrase: parent."
  (log/debug (str "overc parent type: " (type parent)))
  (log/debug (str "overc comp  type: " (type comp)))

  (log/debug (str "set? parent:" (set? parent)))
  (log/debug (str "seq? parent:" (seq? parent)))
  (log/debug (str "seq? comp:" (seq? comp)))

  (if (map? parent)
    (if (get-in parent '(:comment))
      (log/debug (str "parent:" (get-in parent '(:comment)))))
    (log/debug (str "parent:" (fo parent))))
  (if (map? comp)
    (log/debug (str "comp: " (fo comp))))

  (cond

   (or
    (seq? parent)
    (set? parent))
   (let [parents (lazy-seq parent)]
     (filter (fn [result]
               (not (fail? result)))
             (over-each-parent parents comp)))

   (string? comp)
   (overc parent (it comp))

   (set? comp)
   (do (log/debug "comp is a set: converting to a seq.")
       (overc parent (lazy-seq comp)))

   (seq? comp)
   (let [comp-children comp]
     (log/debug (str "comp is a seq - actual type is " (type comp)))
     (filter (fn [result]
               (not (fail? result)))
             (over-each-comp-child parent comp-children)))

   true
   (do
     (log/debug (str "overh: parent and comp are both maps: put comp under parent. Parent=" (:comment parent) "; comp=" (fo comp)))
     (list
      (moreover-comp parent comp sem-impl)))))

(defn overhc [parent head comp]
  "TODO: reimplement as (overc (overh head) comp)"
  (log/debug (str "overhc parent: " parent))
  (log/debug (str "overhc head: " head))
  (log/debug (str "overhc comp: " comp))
  (reduce #'concat
          (map (fn [each-with-head]
                 (overc each-with-head comp))
               (overh parent head))))

(defn over [parents child1 & [child2]]
  (cond (vector? child1)
        (over parents (set child1) child2)
        (vector? child2)
        (over parents child1 (set child2))
        true
  (if (nil? child2) (over parents child1 :top)
      (if (map? parents)
        (over (list parents) child1 child2)
        (if (not (empty? parents))
          (let [parent (first parents)]
            (log/debug (str "over: parent: " parent))
            (concat
             (cond (and (map? parent)
                        (not (nil? (:serialized parent))))
                   ;; In this case, supposed 'parent' is really a lexical item: for now, definition of 'lexical item' is,
                   ;; it has a non-nil value for :serialized - just return nil, nothing else to do.

                   (throw (Exception. (str "Don't know what to do with this parent: " parent)))

                   (and (map? parent)
                        (not (nil? (:schema parent))))
                   ;; figure out whether head is child1 or child2:
                   (let [head
                         (cond
                          (= \c (nth (str (:schema parent)) 0))
                          child2

                          (= \h (nth (str (:schema parent)) 0))
                          child1

                          true
                          (throw (Exception. (str "Don't know what the head-vs-complement ordering is for parent: " parent))))
                         comp
                         (if (= head child1)
                           child2 child1)]
                     (filter (fn [each] (not (fail? each)))
                             (overhc parent head comp)))

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


