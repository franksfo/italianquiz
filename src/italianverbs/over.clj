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

;; TODO: move all of these (over-X) functions to their own namespace.
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

(defn over-each-parent [parents child]
  (if (not (empty? parents))
    (let [each-parent (first parents)]
      (lazy-seq
       (cons
        (overh each-parent child)
        (over-each-parent (rest parents) child))))))

(defn over-each-child [parent children]
  (if (not (empty? children))
    (let [each-child (first children)]
      (lazy-seq
       (cons
        (overh parent each-child)
        (over-each-child parent (rest children)))))))

(defn overh [parent child]
  (log/debug (str "overh parent type: " (type parent)))
  (log/debug (str "overh child  type: " (type child)))

  (log/debug (str "set? parent:" (set? parent)))
  (log/debug (str "seq? child:" (seq? parent)))

  (if (map? parent)
    (if (get-in parent '(:comment))
      (log/debug (str "parent:" (get-in parent '(:comment)))))
    (log/debug (str "parent:" (fo parent))))
  (if (map? child)
    (log/debug (str "child: " (fo child))))

  (cond

   (or (set? parent)
       (seq? parent))
   (let [parents parent]
     (reduce mylazycat
             (over-each-parent parents child)))

   (string? child)
   (overh parent (it child))

   (set? child)
   (overh parent (seq child))

   (seq? child)
   (let [children child]
     (log/debug (str "child is a seq - actual type is " (type child)))
     (filter (fn [result]
               (not (fail? result)))
             (reduce mylazycat
                     (over-each-child parent children))))

   true
   (list
    (moreover-head parent child sem-impl))))

;; Haskell-looking signature:
;; (parent:map) X (child:{set,seq,fs}) => list:map
(defn overc [parent child]
  (log/debug (str "overc parent: " (fo parent)))
  (log/debug (str "overc child: " (fo child)))
  (log/debug (str "overc parent type: " (type parent)))
  (log/debug (str "overc child type: " (type child)))
  (cond
   (or (set? parent)
       (seq? parent))
   (apply concat
          (map (fn [each-parent]
                 (overc each-parent child))
               parent))

   (string? child)
   (overc parent (it child))

   (set? child)
   (overc parent (seq child))

   (seq? child)
   (let [children child]
     (filter (fn [result]
               (not (fail? result)))
             (reduce #'concat
                     (map (fn [each-child]
                            (overc parent each-child))
                          children))))

   (and
    (keyword? child)
    (= child :top))
   (list parent)

   (keyword? child)
   (list :fail)

   true
   (list (moreover-comp parent child sem-impl))))

(defn overhc [parent head comp]
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


