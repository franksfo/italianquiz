(ns italianverbs.unify
  (:refer-clojure :exclude [get-in merge resolve])
  (:require
   [italianverbs.unify :refer :all]
   [clojure.set :refer :all]
   [clojure.string :as string]
   [clojure.tools.logging :as log]))

(defn get-head [sign]
  (if (get sign :head)
    (get-head (get sign :head))
    sign))

(defn resolve [arg]
  "if arg is not a ref, return arg. if is a ref, return (resolve @arg)"
  (if (= (type arg) clojure.lang.Ref)
    (resolve @arg)
    arg))

;; TODO: need tests: many tests use (get-in), but need more dedicated tests for it alone.
(defn get-in [in-map path & [not-found]]
  "same as clojure.core (get-in), but it resolves references if need be."
  (if (seq? in-map)
    (map (fn [each]
           (get-in each path not-found))
         in-map)
    (let [result
          (if (first path)
            (let [result (get in-map (first path) not-found)]
              (if (= result not-found) not-found
                  (get-in (resolve result) (rest path) not-found)))
            in-map)]
      (if (= (type result) clojure.lang.Ref)
        @result
        result))))

;; following is deprecated in favor of just (get-in) (above).
(defn get-in-r [map keys]
  (get-in map keys))

(defn get-r [map key]
  "same as clojure.core (get), but it resolves references if need be."
  (let [result (get map key)]
    (if (= (type result) clojure.lang.Ref)
      @result
      result)))

(defn get-root-head [sign]
  (cond
   (get sign :head)
   (get-root-head (get sign :head))
   true
   sign))

;; TODO: use multi-methods.
;; TODO: keep list of already-seen references to avoid
;; cost of traversing substructures more than once.
(defn fail? [fs]
  "(fail? fs) <=> true if at least one of fs's path's value is :fail."
  (if (= :fail fs) true
      (if (seq? fs) false
          (if (= fs :fail) true
              (do
                (defn failr? [fs keys]
;                  (log/error (str "FAILR: fs: " fs))
;                  (log/error (str "FAILR: keys: " keys))
                  (if (first keys)
                    (if (fail? (get-in fs (list (first keys))))
                      true
                      (failr? fs (rest keys)))
                    false))
                (cond
                 (= fs :fail) true
                 (map? fs)
                 (failr? fs (keys fs))
                 (= (type fs) clojure.lang.Ref)
                 (fail? @fs)
                 :else false))))))

(defn nonfail [maps]
  (filter (fn [each-map]
            (not (fail? each-map)))
          maps))

(defn fail-path [fs & [ fs-keys ] ]
  "find the first failing path in a fs."
  (if (map? fs)
    (let [fs-keys (if fs-keys fs-keys (keys fs))]
      (if (> (.size fs-keys) 0)
        (if (fail? (get-in fs (list (first fs-keys))))
          (cons (first fs-keys) (fail-path (get-in fs (list (first fs-keys)))))
          (fail-path fs (rest fs-keys)))))))

(defn any? [fn members]
  (if (not (empty? members))
    (or (fn (first members))
        (any? fn (rest members)))

    ;; members is empty.
    false))

(defn ref? [val]
  (= (type val) clojure.lang.Ref))

(defn has-set? [fs]
  (cond (map? fs)
        (or (any? set? (vals fs))
            (any? has-set? (vals fs)))
        (ref? fs)
        (has-set? @fs)
        true
        false))

(declare expand-disj) ;; needed by unify.
(declare copy)
(declare unifyc)

;; TODO: many code paths below only look at val1 and val2, and ignore rest of args beyond that.
;; either consider all args, or change signature of (unify) to take only val1 val2.
;; see also lexiconfn/unify (probably will change signature, but make lexiconfn/unify handle
;; have signature [& args] and pass to unify/unify with appropriate translation.
;;
;; TODO: support lazy sequences and vectors
;;
(defn unify [& args]
  (if (empty? (rest args)) (first args))
  (let [val1 (first args)
        val2 (second args)]
    (log/debug (str "unify val1: " val1))
    (log/debug (str "      val2: " val2))
    (cond
     (set? val1)
     (set (filter (fn [each]
                    (not (fail? each)))
                  (reduce union
                          (map (fn [each]
                                 (let [result (unifyc each val2)]
                                   (cond (set? result)
                                         result
                                         (seq? result)
                                         (set result)
                                         true
                                         (set (list result)))))
                               val1))))

     (set? val2)
     (set (filter (fn [each]
                    (not (fail? each)))
                  (reduce union
                          (map (fn [each]
                                 (let [result (unifyc each val1)]
                                   (cond (set? result)
                                         result
                                         (seq? result)
                                         (set result)
                                         true
                                         (set (list result)))))
                               val2))))

     (has-set? val1)
     (unify (expand-disj val1) val2)

     (has-set? val2)
     (unify val1 (expand-disj val2))

     (and (= val1 '())
          (= val2 :top))
     val1

     (and (= val1 '())
          (= val2 '()))
     val1


     (and (= val1 '()))
     :fail

     (and (= val1 nil)
          (= val2 :top))
     val1

     (= val1 nil)
     :fail

     (and (set? val1)
          (set? val2))
     (intersection val1 val2)

     (nil? args) nil

     (= (.count args) 1)
     (first args)

     (= :fail (first args))
     :fail

     (= :fail (second args))
     :fail

     (seq? val1)
     (map (fn [each]
            (unify each val2))
          val1)


     ;; This is the canonical unification case: unifying two DAGs
     ;; (maps with possible references within them).
     ;;
     ;; merge-with: http://clojuredocs.org/clojure_core/clojure.core/merge-with
     ;;
     ;; "Returns a map that consists of the rest of the maps conj-ed onto
     ;; the first. If a key occurs in more than one map, the mapping(s)
     ;; from the latter (left-to-right) will be combined with the mapping in
     ;; the result by calling (f val-in-result val-in-latter)."
     (and (map? val1)
          (map? val2))
     (let [tmp-result
           (reduce #(merge-with unify %1 %2) args)]
       (if (not (nil? (some #{:fail} (vals tmp-result))))
         :fail
         (do ;(println (str "no fail in: " vals))
           tmp-result)))
     (and
      (= (type val1) clojure.lang.Ref)
      (not (= (type val2) clojure.lang.Ref)))
     (do (dosync
          (alter val1
                 (fn [x] (unify @val1 val2))))
         ;; alternative to the above (not tested yet):  (fn [x] (unify (copy @val1) val2))))
         ;; TODO: why is this false-disabled? (document and test) or remove
         (if (and false (fail? @val1)) :fail
         val1))
     (and
      (= (type val2) clojure.lang.Ref)
      (not (= (type val1) clojure.lang.Ref)))
     (do (dosync
          (alter val2
                 (fn [x] (unify val1 @val2))))
         ;; alternative to the above (not tested yet): (fn [x] (unify val1 (fs/copy @val2)))))
         ;; TODO: why is this false-disabled? (document and test) or remove.
         (if (and false (fail? @val2)) :fail
         val2))

     (and
      (= (type val1) clojure.lang.Ref)
      (= (type val2) clojure.lang.Ref))
     (do
       (if (or (= val1 val2) ;; same reference.
               (= val1 @val2)) ;; val1 <- val2
         val1
         (if (= @val1 val2) ;; val1 -> val2
           val2
           (do
             (log/debug (str "unifying two refs: " val1 " and " val2))
             (dosync
              (alter val1
                     (fn [x] (unify @val1 @val2))))
             (dosync
              (alter val2
                     (fn [x] val1))) ;; note that now val2 is a ref to a ref.
             (log/debug (str "returning ref: " val1))
             (if (and false (fail? @val1)) :fail
             val1)))))

     ;; convoluted way of expressing: "if val1 has the form: {:not X}, then .."
     (not (= :notfound (:not val1 :notfound)))
     (if (= val2 :top)
;       :top ;; special case: (unify :top {:not X}) => :top
       val1
       ;; else
       (let [debug1 (log/debug (str "VAL1: " val1))
             debug2 (log/debug (str "VAL2: " val2))
             result (unify (:not val1) val2)]
         (if (= result :fail)
           val2
           :fail)))

     ;; convoluted way of expressing: "if val2 has the form: {:not X}, then .."
     (not (= :notfound (:not val2 :notfound)))
     (if (= val1 :top)
;       val1 ;; special case mentioned above in comments preceding this function.
       val2
       (let [debug1 (log/debug (str "VAL1: " val1))
             debug2 (log/debug (str "VAL2: " val2))
             result (unify val1 (:not val2))]
         (if (= result :fail)
           val1
           :fail)))

     (or (= val1 :fail)
         (= val2 :fail))
     :fail

     (= val1 :top) val2
     (= val2 :top) val1

     ;; these two rules are unfortunately necessary because of mongo/clojure storage of keywords as strings.
     (= val1 "top") val2
     (= val2 "top") val1

     ;; TODO: verify that these keyword/string exceptions are necessary - otherwise remove them.
     ;; :foo,"foo" => :foo
     (and (= (type val1) clojure.lang.Keyword)
          (= (type val2) java.lang.String)
          (= (string/replace-first (str val1) ":" "") val2))
     val1

     ;; TODO: verify that these keyword/string exceptions are necessary - otherwise remove them.
     ;; "foo",:foo => :foo
     (and (= (type val2) clojure.lang.Keyword)
          (= (type val1) java.lang.String)
          (= (string/replace-first (str val2) ":" "") val1))
     val2

     (= val1 val2) val1

     ;; The follow two 2 rules allow values of :english and :italian that
     ;; are strings to over-ride values that are maps (in which
     ;; case they are specs of how to compute a string: agreement
     ;; information such as gender and number.
     (and
      (map? val1)
      (string? val2))
     (do
       (log/debug "unifying a map and a string: ignoring the former and returning the latter: 'val2'")
       val2)

     (and
      (string? val1)
      (map? val2))
     (do
       (log/debug (str "unifying a string and a map: ignoring the latter and returning the former: '" val1 "'"))
       val1)

     :else ;; fail.
     (do
       (log/debug (str "(" val1 ", " val2 ") => :fail"))
;       (println (str "(" val1 ", " val2 ") => :fail"))
       :fail))))

;; unify vs. merge:
;;
;; case 1:
;; (fs/unify {:a 42} {:a 42 :b 43})
;; => {:a 42 :b 43}
;;  and:
;; (fs/match {:a 42} {:a 42 :b 43})
;; => {:b 43, :a 42} ; ok: val2 specializes val1.
;;
;; case 2:
;; (fs/unify {:a 42 :b 43} {:a 42})
;; => {:a 42 :b 43}
;;  but:
;; (fs/match {:a 42 :b 43} {:a 42})
;; => :fail          ; fail: val2 does not specialize val1.

;; special cases:
;; (fs/match {:not X} :top)
;; => :top

(defn match [val1 val2]
  "match: like unify, but requires that every path in val1 must be in val2: in other words, val2 matches, or is a specialization, of val1."
  (let [args (list val1 val2)]
;    (println (str "match(" val1 "," val2 ")"))
    (cond

     (= (.count args) 1)
     (first args)

     (= :fail (first args))
     :fail

     (= :fail (second args))
     :fail

     ;; if keys(val1) is not a subset of keys(val2), then fail.
     ;; same set is ok, since a set is a subset of itself.
     (and (map? val1)
          (map? val2)
          (not (subset? (set (keys val1)) (set (keys val2)))))
     (do
;       (println (str "SUBSET FAIL: " (keys val1) " is not a subset of:" (keys val2)))
     :fail)

     (and (map? val1)
          (map? val2))
     (let [tmp-result
           (reduce #(merge-with match %1 %2) args)]
       (if (not (nil? (some #{:fail} (vals tmp-result))))
         (do
;           (println (str "found fail amongst: " (vals tmp-result)))
            :fail)
         (do ;(println (str "no fail in: " vals))
             tmp-result)))
     (and
      (= (type val1) clojure.lang.Ref)
      (not (= (type val2) clojure.lang.Ref)))
     (do (dosync
          (alter val1
                 (fn [x] (match @val1 val2))))
         (if (and false (fail? @val1)) :fail
             val1))
     (and
      (= (type val2) clojure.lang.Ref)
      (not (= (type val1) clojure.lang.Ref)))
     (do (dosync
          (alter val2
                 (fn [x] (match val1 @val2))))
         (if (and false (fail? @val2)) :fail
             val2))
     (and
      (= (type val1) clojure.lang.Ref)
      (= (type val2) clojure.lang.Ref))
     (do
       (if (or (= val1 val2) ;; same reference.
               (= val1 @val2)) ;; val1 <- val2
         val1
         (if (= @val1 val2) ;; val1 -> val2
           val2
           (do
             (log/debug (str "unifying two refs: " val1 " and " val2))
             (dosync
              (alter val1
                     (fn [x] (match @val1 @val2))))
             (dosync
              (alter val2
                     (fn [x] val1))) ;; note that now val2 is a ref to a ref.
             (log/debug (str "returning ref: " val1))
             (if (and false (fail? @val1)) :fail
                 val1)))))

     ;; convoluted way of expressing: "if val1 has the form: {:not X}, then .."
     (not (= :notfound (:not val1 :notfound)))
     (if (= val2 :top)
       :top ;; special case mentioned above in comments preceding this function.
       ;; else, (if val2 is not :top)..
       (let [result (match (:not val1) val2)]
         (if (= result :fail)
           val2
           :fail)))

     ;; convoluted way of expressing: "if val2 has the form: {:not X}, then .."
     (not (= :notfound (:not val2 :notfound)))
     (if (= val1 :top) val1 ;; another special case: (match :top {:not X}) => :top
         (let [result (match val1 (:not val2))]
           (if (= result :fail)
             val1
             :fail)))

     (or (= val1 :fail)
         (= val2 :fail))
     :fail
     (= val1 :top) val2
     (= val2 :top) val1

     ;; these two rules are unfortunately necessary because of mongo/clojure storage of keywords as strings.
     (= val1 "top") val2
     (= val2 "top") val1

     ;; :foo,"foo" => :foo
     (and (= (type val1) clojure.lang.Keyword)
          (= (type val2) java.lang.String)
          (= (string/replace-first (str val1) ":" "") val2))
     val1

     ;; "foo",:foo => :foo
     (and (= (type val2) clojure.lang.Keyword)
          (= (type val1) java.lang.String)
          (= (string/replace-first (str val2) ":" "") val1))
     val2

     (= val1 val2) val1

     :else :fail)))

(defn merge [& args]
  "warning: {} is the identity value, not nil; that is: (merge X {}) => X, but (merge X nil) => nil, (not X)."
  (if (empty? (rest args)) (first args))
  (let [val1 (first args)
        val2 (second args)]
    (cond

     (set? val1)
     (set (filter (fn [each]
                    (not (fail? each)))
                  (reduce union
                          (map (fn [each]
                                 (let [result (merge (copy each) (copy val2))]
                                   (cond (set? result)
                                         result
                                         (seq? result)
                                         (set result)
                                         true
                                         (set (list result)))))
                               val1))))

     (set? val2)
     (set (filter (fn [each]
                    (not (fail? each)))
                  (reduce union
                          (map (fn [each]
                                 (let [result (merge (copy each) (copy val1))]
                                   (cond (set? result)
                                         result
                                         (seq? result)
                                         (set result)
                                         true
                                         (set (list result)))))
                               val2))))

     (has-set? val1)
     (merge (expand-disj val1) val2)

     (has-set? val2)
     (merge val1 (expand-disj val2))

     (= (.count args) 1)
     (first args)

     (and (map? val1)
          (map? val2))
     (reduce #(merge-with merge %1 %2) args)

     (and
      (= (type val1) clojure.lang.Ref)
      (not (= (type val2) clojure.lang.Ref)))
     (do (dosync
          (alter val1
                 (fn [x] (merge @val1 val2))))
         val1)

     (and
      (= (type val2) clojure.lang.Ref)
      (not (= (type val1) clojure.lang.Ref)))
     (do (dosync
          (alter val2
                 (fn [x] (merge val1 @val2))))
         val2)

     (and
      (= (type val1) clojure.lang.Ref)
      (= (type val2) clojure.lang.Ref))
     (do (dosync
          (alter val1
                 (fn [x] (merge @val1 @val2))))
         val1)

     (and (= val2 :top)
          (not (= :notfound (:not val1 :notfound))))
     val1

     (not (= :notfound (:not val1 :notfound)))
     (let [result (unify (:not val1) val2)]
       (if (= result :fail)
         val2
         :fail))

     (and (= val1 :top)
          (not (= :notfound (:not val2 :notfound))))
     val2

     (not (= :notfound (:not val2 :notfound)))
     (let [result (unify val1 (:not val2))]
        (if (= result :fail)
          val1
          :fail))

     (or (= val1 :fail)
         (= val2 :fail))
     :fail

     (= val1 :top) val2
     (= val2 :top) val1
     (= val1 nil) val2

     ;; note difference in behavior between nil and :nil!:
     ;; (nil is ignored, while :nil! is not).
     ;; (merge 42 nil) => 42
     ;; (merge 42 :nil!) => :nil!
     (= val2 nil) val1
     (= val2 :nil!) val2
     (= val2 "nil!") val2 ;; needed because of translation error from mongod to clojure.

     (= val1 val2) val1

     :else ;override with remainder of arguments, like core/merge.
     (apply merge (rest args)))))

(defn merge-debug [& args]
  (log/debug (str "mergeD  v1:" (first args)))
  (log/debug (str "mergeD: v2:" (second args)))
  (let [retval (apply merge args)]
    (log/debug (str "retvalD: " retval))
    retval))

(def ^:dynamic *exclude-keys* #{:_id})

(defn deref-map [input]
  input)

;; TODO: remove *exclude-keys*,(pathify-r) and (pathify) in favor of fs's versions.
(def ^:dynamic *exclude-keys* (set #{:_id :ref :refmap}))

(defn pathify-r [fs & [prefix]]
"Transform a map into a map of paths/value pairs,
 where paths are lists of keywords, and values are atomic values.
 e.g.:
 {:foo {:bar 42, :baz 99}} =>  { { (:foo :bar) 42}, {(:foo :baz) 99} }
The idea is to map the key :foo to the (recursive) result of pathify on :foo's value."
  (mapcat (fn [kv]
            (let [key (first kv)
                  val (second kv)]
;              (println (str "K:" key))
              (if (not (contains? *exclude-keys* key))
                (if (or (= (type val) clojure.lang.PersistentArrayMap)
                        (= (type val) clojure.lang.PersistentHashMap))
                  (do
;                    (println (str "PAM"))
                    (pathify-r val (concat prefix (list key))))
                  (if (and (= (type val) clojure.lang.Ref)
                           (let [val @val]
                             (or (= (type val) clojure.lang.PersistentArrayMap)
                                 (= (type val) clojure.lang.PersistentHashMap))))
                    (pathify-r @val (concat prefix (list key)))
                  (do
;                    (println (str "not PAM" (type val)))
                    (list {(concat prefix (list key))
                           (if (= (type val) clojure.lang.Ref) @val ;; simply resolve references rather than trying to search for graph isomorphism.
                               val)})))))))
          fs))

(defn pathify [fs]
  (pathify-r fs))


(def uniq-using-recur
  (fn [sorted-vals]
    (loop [sv sorted-vals result nil]
      (let [first-val (first sv)]
        (if (nil? (first sv))
          result
          (let [second-val (second sv)]
            (if (= first-val second-val)
              (recur (rest sv)
                     result)
              (recur (rest sv)
                     (cons first-val result)))))))))

(defn uniq [sorted-vals]
  (reverse (uniq-using-recur sorted-vals)))

;; by comparison (causes stack overflow on large lists):
(defn uniq-recursive [sorted-vals]
  (let [first-val (first sorted-vals)]
    (if first-val
      (let [second-val (second sorted-vals)]
        (if second-val
          (if (= first-val second-val)
            (uniq-recursive (rest sorted-vals))
            (cons first-val
                  (uniq-recursive (rest sorted-vals))))
          (list first-val))))))

(defn paths-to-value [map value path]
  (if (= map value) (list path)
      (if (= (type map) clojure.lang.Ref)
        (paths-to-value @map value path)
        (if (or (= (type map) clojure.lang.PersistentArrayMap)
                (= (type map) clojure.lang.PersistentHashMap))
          (mapcat (fn [key]
                    (paths-to-value (get map key) value (concat path (list key))))
                  (keys map))))))

(defn all-refs [input]
  (if input
    (do
      (if false ;; debug instrumentation
        (do (println "")
            (println (str "input: " input))
            (if (= (type input) clojure.lang.Ref)
              (println (str "@input: " @input)))
            (println "")))
      (if (= (type input) clojure.lang.Ref)
        (cons
         (if (= (type @input) clojure.lang.Ref)
           ;; dereference double-references (references to another reference) :
           (do
;             (println (str "double ref(i): " input " -> " @input " -> " @@input))
           @input)
           ;; a simple reference: reference to a non-reference (e.g. a map, boolean, etc):
           input)
         (all-refs @input))
        (if (or (= (type input) clojure.lang.PersistentArrayMap)
                (= (type input) clojure.lang.PersistentHashMap))
          ;; TODO: fix bug here: vals resolves @'s
          (concat
           (mapcat (fn [key]
                     (let [val (get input key)]
                       (if (= (type input) clojure.lang.Ref)
                         (if (= (type @val) clojure.lang.Ref)
                           (list @val)
                           (list val)))))
                   input)
           (all-refs
            (map (fn [val]
                   ;; dereference double-references (references to another reference) :
                   (if (and (= (type val) clojure.lang.Ref)
                            (= (type @val) clojure.lang.Ref))
                     (do
;                       (println (str "double ref: " val " -> " @val " -> " @@val))
                       @val)
                     ;; a simple reference: reference to a non-reference (e.g. a map, boolean, etc):
                     val))
                 (vals input))))
          (if (and (seq? input)
                   (> (.size input) 0))
            (concat
             (all-refs (first input))
             (all-refs (rest input)))))))))

(defn skeletize [input-val]
  (if (or (= (type input-val) clojure.lang.PersistentArrayMap)
          (= (type input-val) clojure.lang.PersistentHashMap))
    (zipmap (keys (dissoc input-val :serialized))
            (map (fn [val]
                   (if (= (type val) clojure.lang.Ref)
                     :top
                     (if (or (= (type val) clojure.lang.PersistentArrayMap)
                             (= (type val) clojure.lang.PersistentHashMap))
                       (skeletize val)
                       val)))
                 (vals (dissoc input-val :serialized))))
    input-val))

;; TODO s/map/input-map/
;; TODO: merge or distinguish from all-refs (above)
(defn get-refs [input-map]
  (uniq (sort (all-refs input-map))))

;; TODO s/map/input-map/
(defn skels [input-map refs]
  "create map from reference to their skeletons."
  (let [
        refs (get-refs input-map)
        ]
    (zipmap
     refs
     (map (fn [ref]
            (skeletize @ref))
          refs))))

(defn ref-skel-map [input-map]
  "associate each reference in _input-map_ with:
   1. its skeleton
   2. all paths to point to it."
  (let [refs (get-refs input-map)
        ;; skels returns a map from a reference to its skeleton.
        skels (skels input-map refs)]
    (zipmap
     ;; associate each ref with its skeleton.
     (map (fn [ref]
            {:ref ref
             :skel (get skels ref)})
          refs)

     ;; list of all paths that point to each ref in _input-map_.
     (map (fn [eachref]
            (paths-to-value input-map eachref nil))
          refs))))

;; only used for testing: move to test.fs.
(defn ser-db [input-map]
  (let [refs (get-refs input-map)
        skels (skels input-map refs)]
    (ref-skel-map input-map)))

;; (((:a :c) (:b :c) (:d))
;;  ((:a) (:b))
;;  nil)
;;     =>
;; {((:a :c) (:b :c) (:d)) => 2
;;  ((:a)    (:b))         => 1
;;  nil                    => 0
;; }
(defn max-lengths [serialization]
  ;; check type (TODO: use multimethods instead)
  (if (= (first (first serialization)) ())
    (throw (Exception. (str "Serialization was badly formed. This is known to happen when a key's value is a sequence: for now, only maps and atoms are supported as values of keys.")))
    (let [keys (keys serialization)]
      (zipmap
       keys
       (map (fn [paths]
              (if (nil? paths) 0
                  (apply max (map (fn [path] (if (nil? path) 0 (.size path))) paths))))
            keys)))))

(defn sort-by-max-lengths [serialization]
  (let [max-lengths (max-lengths serialization)]
    (sort (fn [x y] (< (second x) (second y)))
          max-lengths)))

(defn sort-shortest-path-ascending-r [serialization path-length-pairs]
  (if (first path-length-pairs)
    (let [path-length-pair (first path-length-pairs)
          paths (first path-length-pair)
          max-length (second path-length-pair)]
      (cons
       (list paths
             (get serialization paths))
       (sort-shortest-path-ascending-r serialization (rest path-length-pairs))))))

(defn ser-intermed [input-map]
  (cond (has-set? input-map)
        (ser-intermed (expand-disj input-map))
        (set? input-map)
        (set (map (fn [each]
                    (ser-intermed each))
                  input-map))
        true
        (let [top-level (skeletize input-map)
              rsk (ref-skel-map input-map)
              sk (map (fn [ref-skel]
                        (:skel ref-skel))
                      (keys rsk))]
          (merge
           {nil top-level}
           (zipmap
            (vals rsk)
            sk)))))

(defn create-shared-values [serialized]
  (map (fn [paths-vals]
         (let [val (second paths-vals)]
           ;; TODO: why/why not do copy val rather than just val(?)
           (ref val)))
       serialized))

(defn create-path-in [path value]
  "create a path starting at map through all keys in map:
   (create-path-in '(a b c d e) value) => {:a {:b {:c {:d {:e value}}}}})"
  (if (first path)
    (if (rest path)
      (let [assigned (create-path-in (rest path) value)]
        {(keyword (first path)) assigned})
      {(first path) value})
    value))

;; Serialization format is a sequence:
;; (
;;  paths1 => map1 <= 'base'
;;  paths2 => map2
;;  ..
;; )
;; 'base' is the outermost map 'skeleton' (
;; a 'skeleton' is a map with the dummy placeholder
;; value :top).
;;
;; Note that (deserialize) should be able to cope with
;; both lists and arrays (i.e. just assume a sequence).
(defn deserialize [serialized]
  (cond (set? serialized)
        (set (map (fn [each]
                    (deserialize each))
                  serialized))
        true (let [base (second (first serialized))]
               (apply merge-debug
                      (let [all
                            (cons base
                                  (flatten
                                   (map (fn [paths-val]
                                          (let [paths (first paths-val)
                                                val (ref (second paths-val))]
                                            (map (fn [path]
                                                   (create-path-in path val))
                                                 paths)))
                                        (rest serialized))))]
                        all)))))

(defn serialize [input-map]
  (cond
   (set? input-map)
   (set (map (fn [each]
               (serialize each))
             input-map))
   (has-set? input-map)
   (serialize (expand-disj input-map))

   true
   (let [memoized (get input-map :serialized :none)]
     (if (not (= memoized :none))
       (let [debug (log/debug "using cached serialization!")]
         memoized)
       (let [ser (ser-intermed input-map)]
        ;; ser is a intermediate (but fully-serialized) representation
         ;; of the input map, as a map from pathsets to reference-free maps
         ;; (maps which have no references within them).

         ;; In place of the references in the original map, the reference-free
         ;; maps have simply a dummy value (the value :top) stored where the
         ;; the reference is in the input-map.
         ;;
         ;; ser:
         ;;
         ;;   pathset    |  value
         ;; -------------+---------
         ;;   pathset1   => value1
         ;;   pathset2   => value2
         ;;      ..         ..
         ;;   nil        => outermost_map
         ;;
         ;; Each pathset is a set of paths to a shared value, the value
         ;; shared by all paths in that pathset.
         ;;
         ;; The last row shown is for the outermost_map that represents
         ;; the entire input, which is why its pathset is nil.
         ;;
         ;; However, ser is not sorted by path length: it needs to be
         ;; sorted so that, when deserialization is done, assignment
         ;; will occur in the correct order: shortest path first.

         ;; Thefore, we now sort _ser_ in a shortest-path-first order, so that
         ;; during de-serialization, all assignments will happen in this
         ;; same correct order.

         (sort-shortest-path-ascending-r ser (sort-by-max-lengths ser)))))))

(defn optimized-ser [input-map]
  "generate a better serialized form that removes intermediate refs (refs to other refs)"
  (let [serialized (serialize input-map)]
    serialized))

(defn copy [input]
  (log/debug (str "copy: " input))
  (cond (seq? input)
        (map (fn [each]
               (copy each))
             input)
        (has-set? input)
        (set
         (map (fn [each]
                (copy each))
              (expand-disj input)))
        true
        (deserialize (serialize input))))

(defn trunc [serialized]
  "create a new serialized map with all paths removed that are non-immediate."
  (map (fn [paths-skel-pair]
         (let [paths (first paths-skel-pair)
               skel (second paths-skel-pair)]
           (let [filtered-paths
                 (filter (fn [path]
                           (let [take-2 (take 2 path)]
                             (and (not (= take-2
                                          '(:head :head)))
                                  (not (= take-2
                                          '(:comp :comp)))
                                  (not (= take-2
                                          '(:head :comp)))
                                  (not (= take-2
                                          '(:comp :2)))
                                  (not (= take-2
                                          '(:head :1)))
                                  (not (= take-2
                                          '(:head :2)))
                                  (not (= take-2
                                          '(:1 :1)))
                                  (not (= take-2
                                          '(:1 :comp)))
                                  (not (= take-2
                                          '(:1 :head)))
                                  (not (= take-2
                                          '(:1 :2)))
                                  (not (= take-2
                                          '(:2 :2)))
                                  (not (= take-2
                                          '(:2 :1)))
                                  (not (= take-2
                                          '(:2 :head)))
                                  (not (= take-2
                                          '(:2 :comp))))))
                         paths)]
           (list filtered-paths skel))))
       serialized))

(defn copy-trunc [map]
  (log/debug (str "copy: " map))
  (deserialize (trunc (serialize map))))

(defn unifyc [& args]
  (log/debug (str "unifyc: " args))
  "like fs/unify, but fs/copy each argument before unifying."
  (apply unify
         (map (fn [arg]
                (copy arg))
              args)))

(defn has-path [path paths]
  (if (first paths)
    (if (= (first paths) path)
      true
      (has-path path (rest paths)))))

(defn path-to-ref-index [serialized path n]
  "given serialized form of a map, find the index for _path_. Start with 0."
  (if (first serialized)
    (let [paths (butlast (first serialized))
          has-path (has-path path (first paths))]
      (if (not (nil? has-path))
        n
        (path-to-ref-index (rest serialized) path (+ n 1))))))

(defn compare-bytewise [a b index]
  "compare two byte by casting each byte to short."
  (if (> (alength a) index)
    (if (> (alength b) index)
      (if (= (nth a index)
             (nth b index))
        (compare-bytewise a b (+ 1 index))
        (< (nth a index)
           (nth b index)))
      true)
    false))

(defn sorted-paths-1 [paths]
  (sort (fn [x y]
          (if (< (.size x) (.size y))
            true
            (compare-bytewise (.getBytes (str x)) (.getBytes (str y)) 0)))
        paths))

(defn sorted-paths [serialized path n index]
  (let [lookup (nth serialized index)
        allpaths (seq (first (butlast lookup)))]
    (sorted-paths-1 allpaths)))

(defn is-first-path [serialized path n index]
  (if (nil? index)
    (throw (Exception. (str "Index was null in serialized feature structure: " serialized)))
    (let [lookup (nth serialized index)
          firstpath (seq (first (sorted-paths serialized path n index)))]
      (if (or true (= (seq path) firstpath))
        (do ;(println (str "path: " (seq path) " is first of " (sorted-paths serialized path n index)))
            true)
        (do ;(println (str "path: " (seq path) " is NOT first of " (sorted-paths serialized path n index)))
            false)))))


(defn first-path [serialized path n index]
  (let [lookup (nth serialized index)
        firstpath (seq (first (sorted-paths serialized path n index)))]
    firstpath))

(defn ref= [map path1 path2]
  "return true iff path1 and path2 point to the same object."
  ;; TODO: add error checking.
  (let [butlast-val1 (get-in map (butlast path1))
        butlast-val2 (get-in map (butlast path2))]
    (= (get butlast-val1 (last path1))
       (get butlast-val2 (last path2)))))

(defn strip-refs [map-with-refs]
  "return a map like map-with-refs, but without refs - (e.g. {:foo (ref 42)} => {:foo 42}) - used for printing maps in plain (i.e. non html) format"
  (cond
   (= map-with-refs {})
   {}
   (map? map-with-refs)
   (let [map-keys (sort (keys map-with-refs))]
     (let [first-key (first (keys map-with-refs))
           val (get map-with-refs first-key)]
       (conj
        {first-key (strip-refs val)}
        (strip-refs (dissoc map-with-refs first-key)))))
   (= (type map-with-refs) clojure.lang.Ref)
   (strip-refs (deref map-with-refs))
   :else
   map-with-refs))

(defn refset2map [fs]
  "Turn every ref to a set into a map with two keys: :ref and :val."
  (cond

   (set? fs)
   (set (map (fn [each]
               (refset2map each))
             fs))

   (and (ref? fs)
        (set? @fs))
   (set (map (fn [each]
               {:val (refset2map each)
                :ref fs})
             @fs))

   (ref? fs)
   {:val (refset2map @fs)
    :ref fs}

   (and (map? fs)
        (not (empty? fs)))
   (let [key (first (first fs))
         val (key fs)]
     (conj
      {key (refset2map val)}
      (refset2map (dissoc fs key))))

   true
   fs))

(defn get-all-ref-tuples [fs & [path]]
  "returns list of ref:val:path tuples."
  (let [path (if path path nil)]
    (cond
     (and (map? fs)
          (not (empty? fs))
          (:ref fs))
     (list {:ref (:ref fs)
            :val (:val fs)
            :path path})
     (and (map? fs)
          (not (empty? fs)))
     (let [key (first (first fs))
           val (key fs)]
       (concat
        (get-all-ref-tuples val (concat path (list key)))
        (get-all-ref-tuples (dissoc fs key) path)))
     true nil)))

(defn get-all-refs-for [fs]
  "get the set of refs in a normalized fs"
  (apply union
         (map (fn [tuple]
                (set (list (:ref tuple))))
              (get-all-ref-tuples fs))))

(defn cartesian [set1 set2]
  "for x in set1, y in set2, conj each x and each y"
  (cond (empty? set1) set2
        (empty? set2) set1
        true
        (apply union
               (map (fn [each-map-in-set-1]
                      (set (map (fn [each-map-in-set-2]
                                  (conj each-map-in-set-1 each-map-in-set-2))
                                set2)))
                    set1))))
(defn get-unified-value-for [fs ref]
  "get all values to be unified for the given ref in the given normalized fs."
  (reduce unify
          (apply concat
                 (map (fn [tuple]
                        (let [tuple-ref (:ref tuple)]
                          (if (= tuple-ref ref)
                            (list (:val tuple)))))
                      (get-all-ref-tuples fs)))))

(defn copy-with-ref-substitute [fs old-ref new-ref]
  "create new fs, but with new-ref substituted for every occurance of {:ref ref,:val X}"
  (cond
   (and (map? fs)
        (not (empty? fs))
        (not (nil? (:ref fs)))
        (= (:ref fs) old-ref))
   new-ref

   (and (map? fs)
        (not (empty? fs)))
   (let [key (first (first fs))
         val (key fs)]
     (conj {key (copy-with-ref-substitute val old-ref new-ref)}
           (copy-with-ref-substitute (dissoc fs key) old-ref new-ref)))
   true
   fs))

(defn copy-with-assignments [fs assignments]
  (if (not (empty? assignments))
    (let [assignment (first assignments)
          old-ref (:ref assignment)
          new-ref (ref (:val assignment))]
      (copy-with-assignments
       (copy-with-ref-substitute fs old-ref new-ref)
       (rest assignments)))
    fs))

(defn step2 [fs]
  "step2.."
  (cond

   (and (set? fs)
        (not (empty? fs)))
   (union
    (step2 (first fs))
    (step2 (set (rest fs))))

   (and (map? fs)
        (not (empty? fs)))
   (let [key (first (first fs))
         val (step2 (key fs))]
     (cond

      (and (set? val)
           (empty? val))
      val

      (set? val)
      (cartesian
       (union
        #{{key (first val)}}
        (step2 {key (set (rest val))}))
       (step2 (dissoc fs key)))

      true
      (cartesian
       (set (list {key val}))
       (step2 (dissoc fs key)))))

   (and (seq? fs)
        (empty? fs))
   fs

   (and (set? fs)
        (empty? fs))
   fs

   true
   (set (list fs))))

(defn expand-disj [input]
  (let [step2-set (step2 (refset2map input))
        refs-per-fs
        (zipmap (seq step2-set)
                (map (fn [each-member]
                       (get-all-refs-for each-member))
                     step2-set))

        unified-values (map (fn [each-fs]
                              {:fs each-fs
                               :assignments
                               (map (fn [each-ref-in-fs]
                                      {:ref each-ref-in-fs
                                    :val (get-unified-value-for each-fs each-ref-in-fs)})
                                    (get refs-per-fs each-fs))})
                            step2-set)]
    (set (filter (fn [each]
                   (not (fail? each)))
                 (map (fn [each-tuple]
                        (let [fs (:fs each-tuple)
                              assignments (:assignments each-tuple)]
                          (copy-with-assignments fs assignments)))
                      unified-values)))))

(defn remove-path-from [fs & [paths]]
  "dissoc a path from a map; e.g.: (remove-path-from {:a {:b 42 :c 43}} '(:a :b)) => {:a {:c 43}}."
  (let [debug (log/debug (str "remove path from fs: " fs " with paths: " paths))]
    (cond (empty? paths)
          fs

          (ref? fs)
          (remove-path-from @fs paths)

          (keyword? fs)
          fs

          (empty? fs)
          :top

          true
          (let [path (first paths)]
            (remove-path-from
             (cond (keyword fs)
                   fs
                   (and (map? fs)
                        (not (empty? fs))
                        (not (empty? path)))
                   (let [feature (first path)]
                     (cond (ref? fs)
                           (remove-path-from @fs (list path))
                           (map? fs)
                           (cond
                            (empty? (rest path))
                            (dissoc fs feature)
                            (not (= :notfound (get-in fs (list feature) :notfound)))
                            (conj
                             {feature (remove-path-from (get-in fs (list feature)) (list (rest path)))}
                             (dissoc fs feature)))))
                   true (throw (Exception. (str "don't know what to do with this input argument (fs): " fs))))
             (rest paths))))))

