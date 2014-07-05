(ns italianverbs.forest
  (:refer-clojure :exclude [get-in deref merge resolve find future parents rand-int])
  (:require
   [clojure.core :as core]
   ;; have to exclude partition-by because of some namespace clash, not sure how else to fix
   [clojure.core.async :as async :exclude [partition-by]]
   [clojure.set :refer :all]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [italianverbs.cache :refer (build-lex-sch-cache get-comp-phrases-of get-head-phrases-of get-lex
                                                   get-parent-phrases-for-spec
                                                   overc overh overh-with-cache overc-with-cache)]
   [italianverbs.lexicon :refer (it)]
   [italianverbs.morphology :refer (fo fo-ps)]
   [italianverbs.over :as over]
   [italianverbs.unify :as unify]
   [italianverbs.unify :refer (dissoc-paths get-in fail? lazy-shuffle remove-top-values-log show-spec unifyc)]))

(def concurrent false)

(def random-order true)
(defn rand-int [range & [constant]]
  (if random-order
    (core/rand-int range)
    (if constant constant 0)))

(declare lightning-bolt)

(defn add-comp-phrase-to-headed-phrase [parents phrases & [cache supplied-comp-spec]]
  (if (and (not (empty? parents))
           (first parents))
    (let [debug
          (do
            (log/debug (str "PARENTS: " (fo-ps parents)))
            (log/debug (str "TYPE PARENTS: " (type parents)))
            (log/debug (str "EMPTY? PARENTS: " (empty? parents)))
            (log/debug (str "starting add-comp-phrase-to-headed-phrase."))
            (log/debug (str "    with parent: " (fo-ps (first parents))))
            (log/trace (str "    with phrases: " (fo-ps phrases)))
            (log/trace (str "add-comp-phrase-to-headed-phrase: emptyness of parents: " (empty? parents))))

          debug (if false (throw (Exception. "GOT HERE: INSIDE MAIN PART OF add-comp-phrase-to-headed-phrase.")))
          debug (log/trace (str "add-comp-phrase-to-headed-phrase is non-empty."))
          parent (first parents)

          comp-spec
          (dissoc-paths
           (unifyc
            (get-in parent '(:comp))
            supplied-comp-spec)
           ;; TODO: do we need to dissoc these paths from the comp spec?
           '((:english :initial)
             (:italian :initial)))

          debug (do (log/debug (str "add-comp-phrase-to-headed-phrase: parent: " (fo-ps parent)))
                    (log/debug (str "   with comp-spec: " (show-spec comp-spec))))

          comp-phrases-for-parent (if (not (= false (get-in comp-spec '(:phrasal))))
                                    (filter (fn [phrase]
                                              (not (fail? phrase)))
                                            (map (fn [phrase]
                                                   (unifyc phrase comp-spec))
                                                 (get-comp-phrases-of parent cache)))
                                    (do
                                      (log/trace (str "the phrase '" (fo-ps parent) "' specifies a non-phrasal complement."))
                                      '()))

          comps 
          (lightning-bolt phrases cache comp-spec)]
      (lazy-cat
       (overc parent comps)
       (add-comp-phrase-to-headed-phrase (rest parents) phrases cache supplied-comp-spec)))))

(def can-log-if-in-sandbox-mode false)

(defn phrasal-headed-phrases [phrases grammar depth cache path]
  "return a lazy seq of phrases (maps) whose heads are themselves phrases."
  (if (not (empty? phrases))
    (let [debug (log/debug (str "phrasal-headed-phrases with phrases: " (fo-ps phrases)))
          parent (first phrases) ;; realizes possibly?
          debug (log/trace (str "phrasal-headed-phrases grammar size: " (.size grammar)))
          headed-phrases-of-parent (get-head-phrases-of parent cache)]
      (if (nil? headed-phrases-of-parent)
        (phrasal-headed-phrases (rest phrases) grammar depth cache path)
        (let [headed-phrases-of-parent (if (nil? headed-phrases-of-parent)
                                         (list)
                                         headed-phrases-of-parent)
              head-spec (dissoc-paths (get-in parent '(:head))
                                      '((:english :initial)
                                        (:italian :initial)))
              debug (log/trace (str "phrasal-headed-phrases: parent's head: " (show-spec head-spec)))]
          (lazy-cat
           (let [debug (log/debug (str "about to call lightning-bolt from phrasal-headed-phrase."))
                 debug (log/debug (str " with head-spec: " (show-spec head-spec)))
                 bolts (lightning-bolt grammar cache head-spec (+ 1 depth))]
             (overh phrases bolts))
           (phrasal-headed-phrases (rest phrases) grammar depth cache path)))))))

(defn parents-given-spec [cache spec phrases]
  "subset of phrases possible where the phrase's head is the given head."
  (if (fail? spec)
    nil
    (do
      (if (nil? phrases)
        (log/trace (str "no parents for spec: " (show-spec spec))))
      (log/debug (str "parents-given-spec: spec:" (show-spec spec)))
      (log/trace (str "parents-given-spec: phrases:" (fo-ps phrases)))
      (let [cached-parents (get-parent-phrases-for-spec cache spec)]
        (log/debug (str "parents-given-spec cached: " (if (not (nil? cached-parents))
                                                      (.size cached-parents))))
        (if (not (nil? cached-parents))
          cached-parents
          (do
            (log/warn (str "no cached entry for spec: " (show-spec spec)))
            ;; TODO: add this missing entry to the cache somehow:
            ;; problem is that cache is immutable as far as we are using it:
            ;; need to make it an atom or some similar concurrent data structure and
            ;; mutate it as such.
            (filter #(not (fail? %))
                    (map (fn [each-phrase]
                           (unifyc each-phrase spec))
                         ;; TODO: possibly: remove-paths such as (subcat) from head: would make it easier to call with lexemes:
                         ;; e.g. "generate a sentence whose head is the word 'mangiare'" (i.e. user passes the lexical entry as
                         ;; head param of (lightning-bolt)".
                         phrases))))))))

(defn lazy-cats [lists & [ show-first ]]
  (if (not (empty? lists))
    (if (not (empty? (first lists)))
      (do
        (if show-first
          (log/info (str "lazy-cats: first: " (fo-ps (first (first lists))))))
        (lazy-cat (first lists)
                  (lazy-cats (rest lists))))
      (lazy-cats (rest lists)))))

(defn log-path [path log-fn & [ depth]]
  (let [depth (if depth depth 0)
        print-blank-line false]
    (if (> (.size path) 0)
      (let [h-or-c (:h-or-c (first path))
            depth (:depth (first path))
            grammar (:grammar (first path))
            lexicon-size (:lexicon-size (first path))
            spec (:spec (first path))
            phrases (fo-ps (:parents (first path)))]
        (log-fn (str "LB@[" depth "]: " h-or-c ":" spec))
        (log/trace (str "   grammar: " (fo-ps grammar) "; lexicon size: " lexicon-size))
        (log-fn (str "   applicable rules: " (fo-ps phrases)))
        (log-path (rest path) log-fn (+ depth 1)))
      (if print-blank-line (log-fn (str ""))))))

(def maxdepth 1)

(defn phrasal-spec [spec cache]
  "add additional constraints so that this spec can constrain heads and comps."
  (unifyc spec (if (:phrase-constraints cache)
                 (do
                   (log/trace (str "phrasal-spec: " (show-spec (:phrase-constraints cache))))
                   (:phrase-constraints cache))
                  :top)))

(defn hl [cache grammar & [spec depth]]
  (let [depth (if depth depth 0)
        spec (phrasal-spec (if spec spec :top) cache)
        head-spec (get-in spec [:head :synsem])
        grammar (lazy-shuffle grammar)]

    ;; try every possible lexeme as a candidate head for each phrase:
    ;; use (:comp (cache ..)) as the subset of the lexicon to try.
    (mapcat #(lazy-seq (overh % (filter (fn [lexeme]
                                          (not (fail? (unifyc (get-in lexeme [:synsem]) head-spec))))
                                        (lazy-shuffle (:head (cache (:rule %)))))))
            (parents-given-spec cache spec grammar))))

(defn lazy-mapcat-bailout-early [the-fn the-args]
   (let [arg (first the-args)]
     (if arg
       (let [result (the-fn arg)]
         (if (empty? result)
           (do
             (log/warn (str "lazy-cat: bailing out early because first result was nil."))
             nil)
           (lazy-cat
            result
            (lazy-mapcat-bailout-early the-fn (rest the-args))))))))

(defn lazy-mapcat-bailout-after [name the-fn the-args tries & [enough-tries length-of-args]]
  (let [arg (first the-args)
        enough-tries (if (not (nil? enough-tries)) enough-tries tries)
        ;; assuming that args is not a lazy seq: if so, this (.size call) will realize it.
        length-of-args (if (not (nil? length-of-args)) length-of-args (.size the-args))]
    (if (= tries 0)
      (log/warn (str "lazy-mapcat-bailout-after(" name "): bailing out now because we've tried: " enough-tries " without success with total possible args: " length-of-args))
      ;; else: keep trying more possibilities: apply the-fn to the first arg in the-args,
      ;; and concat that with a recursive function call with (rest the-args).
      (if arg
        (let [result (the-fn arg)]
          (lazy-cat
           result
           (lazy-mapcat-bailout-after name the-fn (rest the-args) (- tries 1) enough-tries length-of-args)))))))

(defn lazy-mapcat [the-fn the-args]
   (let [arg (first the-args)]
     (if arg
       (let [result (the-fn arg)]
         (lazy-cat
          result
          (lazy-mapcat the-fn (rest the-args)))))))

(declare hlcl)
(declare hlcp)
(declare hpcp)

(defn hp [cache grammar & [spec depth chain]]
  "return a lazy sequence of every possible phrasal head as the head of every rule in rule-set _grammar_."
  (let [depth (if depth depth 0)
        spec (phrasal-spec (if spec spec :top) cache)
        chain (if chain 
                (str chain "->"
                     (str "hp@" depth " [" (show-spec spec) "]"))
                (str "hp@" depth " [" (show-spec spec) "]"))
        grammar (lazy-shuffle grammar)
        with-hlcl (lazy-mapcat-bailout-after (str chain "->H:hlcl")
                   #(do
                      (log/debug
                       (str chain "(" (get-in % [:rule])  ")->H:hlcl["
                            (show-spec (get-in % [:head]))))
                      (overh %
                             (hlcl cache
                                   grammar
                                   (get-in % [:head])
                                   (+ 1 depth)
                                   chain)))
                   (filter (fn [rule]
                             (not (fail? rule)))
                           (map (fn [rule]
                                  (unifyc rule spec))
                                grammar))
                   2)

        debug (log/debug (str chain ":finished with hlcl as head:" (type with-hlcl)))

        with-hlcp (lazy-mapcat-bailout-after (str chain "->H:hlcp")
                   #(do
                      (log/debug
                       (str chain "(" (get-in % [:rule])  ")->H:hlcp["
                            (show-spec (get-in % [:head]))))
                      (overh %
                             (hlcp cache
                                   grammar
                                   (get-in % [:head])
                                   (+ 1 depth)
                                   chain)))
                   (filter (fn [rule]
                             (not (fail? rule)))
                           (map (fn [rule]
                                  (unifyc rule spec))
                                grammar))
                   2)

        debug (log/debug (str chain ":finished with hlcp as head:" (type with-hlcp)))

        with-hpcp (lazy-mapcat-bailout-after (str chain "->H:hpcp")
                   #(do
                      (log/debug
                       (str chain "(" (get-in % [:rule])  ")->H:hpcp["
                            (show-spec (get-in % [:head]))))
                      (overh %
                             (hpcp cache
                                   grammar
                                   (get-in % [:head])
                                   (+ 1 depth))))
                   (filter (fn [rule]
                             (not (fail? rule)))
                           (map (fn [rule]
                                  (unifyc rule spec))
                                grammar))
                   2)

        debug (log/debug (str chain ":finished with hpcp as head:" (type with-hpcp)))

        ordering (rand-int 6)
        ]
    (cond
     true
     with-hlcl
     (= ordering 0)
     (lazy-cat with-hlcp with-hlcl with-hpcp)
     (= ordering 1)
     (lazy-cat with-hlcp with-hpcp with-hlcl)
     (= ordering 2)
     (lazy-cat with-hlcl with-hlcp with-hpcp)
     (= ordering 3)
     (lazy-cat with-hlcl with-hpcp with-hlcp)
     (= ordering 4)
     (lazy-cat with-hpcp with-hlcl with-hlcp)
     (= ordering 5)
     (lazy-cat with-hpcp with-hlcp with-hlcl))))

(defn cp [phrases-with-heads cache grammar]
  "phrases-with-heads is a seq (usually lazy)"
  (lazy-mapcat
   #(lazy-seq (overc % (hlcl cache grammar (get-in % [:comp]))))
   phrases-with-heads))

(defn hlcl [cache grammar & [spec depth chain]]
  "generate all the phrases where the head is a lexeme and the complement is a lexeme"
  (let [depth (if depth depth 0)
        ;; adding {:head {:phrasal false}} because head of hlcp is lexical, not phrasal.
        spec (unifyc {:head {:phrasal false}} (phrasal-spec (if spec spec :top) cache))

        chain (if chain 
                (str chain "->"
                     (str "hlcl@" depth " [" (show-spec spec) "]"))
                (str "hlcl@" depth " [" (show-spec spec) "]"))]
    
    (log/debug "hlcl:: " chain)

    ;; parents-with-heads is the lazy sequence of all possible lexical heads attached to all possible grammar rules.
    (let [parents-with-heads
          (hl cache grammar spec)]
      (lazy-mapcat
       (fn [parent-with-head]
         (let [pred-of-arg (get-in parent-with-head [:comp :synsem])]
           (log/trace (str "pred-of-arg: " pred-of-arg))
           (log/debug (str "looking for lexical complements for headed phrase: " (fo-ps parent-with-head)))
           (overc parent-with-head (lazy-shuffle
                                    (filter (fn [complement]
                                              (let [not-fail
                                                    (not (fail? (unifyc (get-in complement [:synsem])
                                                                        pred-of-arg)))]
                                                not-fail))
                                            (:comp (cache (:rule parent-with-head))))))))
       parents-with-heads))))

(defn hlcp [cache grammar & [spec depth chain]]
  "generate all the phrases where the head is a lexeme and the complement is a phrase."
  (let [depth (if depth depth 0)
        ;; adding {:head {:phrasal false}} because head of hlcp is lexical, not phrasal.
        spec (unifyc {:head {:phrasal false}} (phrasal-spec (if spec spec :top) cache))
        head-spec (get-in spec [:head])
        chain (if chain 
                (str chain "->"
                     (str "hlcp@" depth " [" (show-spec spec) "]"))
                (str "hlcp@" depth " [" (show-spec spec) "]"))]

    (log/debug (str "hlcp:: " chain))

    ;; parents-with-heads is the lazy sequence of all possible heads attached to all possible grammar rules.
    (let [parents-with-heads
          (hl cache grammar spec)]

      (lazy-mapcat-bailout-after
       (str "hlcp@" depth)
       (fn [parent-with-head]
         (lazy-seq (overc parent-with-head (hlcl cache
                                                 grammar
                                                 {:synsem (get-in parent-with-head [:comp :synsem] :top)}
                                                 (+ 1 depth)))))
       parents-with-heads
       1))))

(defn cl [cache grammar]
  (lazy-mapcat
   #(lazy-seq (overc % (lazy-shuffle (:comp (cache (:rule %))))))
   grammar))

(defn hpcl [cache grammar & [spec depth]]
  "generate all the phrases where the head is a phrase and the complement is a lexeme."
  (let [depth (if depth depth 0)
        spec (phrasal-spec (if spec spec :top) cache)
        head-spec (get-in spec [:head])]
    (log/debug (str "hpcl@" depth " with spec: " (show-spec spec)))
    (let [hp (lazy-seq (hp cache grammar head-spec (+ 1 depth)))]
      (log/debug (str "HP IS DONE. NOW DOING CL."))
      (cl cache hp))))

(defn hpcp [cache grammar & [spec depth]]
  "generate all the phrases where the head is a phrase and the complement is a phrase."
  (let [depth (if depth depth 0)
        spec (phrasal-spec (if spec spec :top) cache)
        head-spec (get-in spec [:head])]
    (if (fail? spec)
      nil
      (do
        (log/debug (str "hpcp[" (show-spec spec) "]@" depth ": [H:" (show-spec head-spec) "]"))
        (let [hp
              (hp cache grammar head-spec (+ 0 depth))
              ordering (rand-int 6)
              with-hlcl (lazy-mapcat-bailout-after
                         (str "hpcp@" depth "->hlcl")
                         (fn [the-hp]
                           (overc
                            the-hp
                            (hlcl cache grammar
                                  {:synsem (get-in the-hp [:comp :synsem] :top)}
                                  (+ 0 depth))))
                         hp
                         1)
              with-hlcp (lazy-mapcat-bailout-after
                         (str "hlcp@" depth "->hlcp")
                         (fn [the-hp]
                           (log/info (str "the-hp's [:comp :synsem]: " (show-spec (get-in the-hp [:comp :synsem] :top))))
                           (let [the-hlcp (hlcp cache grammar
                                                {:synsem (get-in the-hp [:comp :synsem] :top)}
                                                (+ 0 depth))]
                             (if (empty? the-hlcp)
                               (do (log/warn (str "Can't do a damn thing with this: hlcp with spec: {:synsem "
                                                  (show-spec (get-in the-hp [:comp :synsem] :top))))
                                   nil)
                               (overc
                                the-hp
                                the-hlcp))))
                         hp
                         1)

              with-hpcl (lazy-mapcat-bailout-after
                         (str "hpcl@" depth "->hpcl")
                         (fn [the-hp]
                           (overc
                            the-hp
                            (hpcl cache grammar
                                  {:synsem (get-in the-hp [:comp :synsem] :top)}
                                  (+ 0 depth))))
                         hp
                         1)
              ]

          (cond
           (or true (= ordering 0))
           (lazy-cat with-hlcl with-hlcp with-hpcl)
           
           (= ordering 1)
           (lazy-cat with-hlcl with-hpcl with-hlcp)
           
           (= ordering 2)
           (lazy-cat with-hlcp with-hlcl with-hpcl)
           
           (= ordering 3)
           (lazy-cat with-hlcp with-hpcl with-hlcl)
           
           (= ordering 4)
           (lazy-cat with-hpcl with-hlcl with-hlcp)
           
           (or true (= ordering 5))
           (lazy-cat with-hpcl with-hlcp with-hlcl)))))))

(defn hppcp [cache grammar & [spec depth]]
  (let [depth (if depth depth 0)
        spec (phrasal-spec (if spec spec :top) cache)
        head-spec (get-in spec [:head])]
    (log/debug (str "hpcp with spec: " (show-spec spec)))
    (mapcat

     #(lazy-seq

       (overc % ;; parent: a phrase from HEAD-PHRASE:
              ;; complement: a hlcl.
              (do
                (log/debug (str "generating a comp with: " (get-in % [:comp :synsem] :top) " given parent: "
                                (:rule %)))
                (hlcl cache grammar {:synsem (get-in % [:comp :synsem] :top)} (+ 1 depth)))))

     ;; HEAD-PHRASE:
     (mapcat
      (let [head-spec (unifyc head-spec {:head {:synsem {:subcat {:1 :top
                                                                  :2 '()}}}})]
        #(lazy-seq (overh

                    ;; parent
                    (parents-given-spec cache head-spec (lazy-shuffle grammar))

                    ;; head: a hpcl.
                    %))

        (log/debug (str "generating a head phrase with: " (get-in head-spec [:head] :top)))
;        (let [head-spec (unifyc head-spec {:head {:synsem {:subcat :top}}})]
        (let [head-spec (unifyc head-spec {:head :top})]
          (hpcl cache
                ;; grammar for this hlcl: the phrase's head must *not* be an intransitive verb.
                (parents-given-spec cache {:synsem (get-in head-spec [:head :synsem] :top)}
                                    (lazy-shuffle grammar))
                (get-in head-spec [:head]) (+ 1 depth))))))))

(defn hxcx [cache grammar & [spec depth]]
  (let [depth (if depth depth 0)
        spec (phrasal-spec (if spec spec :top) cache)
        head-spec (get-in spec [:head] :top)]
    (let [fns [hlcl]]
      (mapcat
       #(% cache grammar spec depth)
       (shuffle fns)))))

(defn lightning-bolt [grammar cache & [ spec depth]]
  "lightning-bolt lite"
  (let [depth (if depth depth 0)
        spec (if spec spec :top)
        parents (map #(unifyc % spec)
                     grammar)]
    (cond 
     (> depth maxdepth)
     nil

     true
     (let [parents-given-spec (parents-given-spec spec (lazy-shuffle grammar))

           lexical-headed-phrases
           (map #(overh % (lazy-shuffle (:head (cache (:rule %)))))
                parents-given-spec)

           debug (log/debug (str "PARENTS FOR THIS SPEC: " (fo-ps parents-given-spec)))

           phrasal-headed-phrases
           (if (not (= false (get-in spec [:head :phrasal])))
             (phrasal-headed-phrases parents-given-spec
                                     grammar depth cache nil))

           debug (log/debug (str "getting 1-level trees.."))

           hlcl
           (if (first lexical-headed-phrases)
             (mapcat #(overc % (lazy-shuffle (:comp (cache (:rule (first parents-given-spec))))))
                     lexical-headed-phrases))

           debug (log/debug (str "done getting 1-level trees - type: " (type hlcl)))

           hlcp
           (if (not (= false (get-in spec [:comp :phrasal])))
               (add-comp-phrase-to-headed-phrase lexical-headed-phrases
                                                 grammar cache
                                                 :top))

;           hpcp
;           (if (and (not (= false (get-in spec [:comp :phrasal])))
;                    (not (= false (get-in spec [:head :phrasal]))))
;             (mapcat #(add-comp-phrase-to-headed-phrase %
;                                                        grammar cache
;                                                        :top)
;                     phrasal-headed-phrases))


           debug (log/debug (str "FIRST PHRASAL-HEADED-PHRASES: "
                                 (fo-ps (first phrasal-headed-phrases))))

           hpcl
           (mapcat #(overc % (lazy-shuffle (:comp (cache (:rule %)))))
                   phrasal-headed-phrases)
           
           ]
;       hlcl))))
;       hlcl))))
       (flatten (lazy-shuffle (list hpcl hlcl)))))))
;       (lazy-shuffle (lazy-cats one-level-trees with-phrasal-complement hpcl))))))

;; aliases that might be easier to use in a repl:
(defn lb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

(defn lightningb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

