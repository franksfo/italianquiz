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

(declare lazy-mapcat)

(defn lazy-mapcat-shuffle [fn args & [depth name foo]]
  (do
    (log/debug (str "lms@" depth ":" name))
    (log/debug (str "lms@" depth ":" name " : type of args: " (type args)))
    (log/debug (str "lms@" depth ":" name " : type of first arg: " (type (first args))))
    (let [result
          (lazy-mapcat fn (shuffle args))]
      (log/debug (str "lms@" depth ":" name " : returning type: " (if result (type result))))
      result)))


;; (take 10 (repeatedly #(fo-ps (take 1 (forest/gen1 (shuffle grammar) (shuffle lexicon) {:synsem {:cat :verb :subcat '()}})))))

;; TODO: add usage of rule-to-lexicon cache (rather than using lexicon directly)
(defn gen1 [grammar lexicon spec & [ depth ]]
  (log/debug (str "gen1@" depth))
  (let [depth (if depth depth 0)
        parents (filter #(not (fail? (unifyc spec %)))
                        (map (fn [rule]
                               (unifyc spec rule))
                             grammar))

        ;; simple-case: hlcX
        simple
        (lazy-mapcat-shuffle
         (fn [parent]
           (do
             (log/debug (str "gen1@" depth ": overh(lex) with parent: " (fo-ps parent)))
             (overh parent lexicon)))
         parents
         depth
         "overh(lex)" 42)


        parents-with-head
        (lazy-mapcat-shuffle
         (fn [generates-parent-with-head]
           (generates-parent-with-head))

          (list
           
           (fn []
             (lazy-mapcat-shuffle
              (fn [parent]
                (do
                  (log/debug (str "gen1@" depth ": overh(lex) with parent: " (fo-ps parent)))
                  (overh parent lexicon)))
              parents
              depth
              "overh(lex)" 42))
           
           (fn []
             (if (< depth 1)

                (lazy-mapcat-shuffle
                 (fn [parent]
                   (do
                     (log/debug (str "gen1@" depth ": overh(gen1) with parent: " (fo-ps parent)))
                     (overh parent
                            (gen1 grammar lexicon
                                  (get-in parent [:head])
                                  (+ 1 depth)))))
                 parents
                 depth
                 "overh(gen1)" 42)
                (do
                  (log/debug (str "gen1@" depth ": terminating."))
                  nil))))
          depth
          "overh(lex;gen1)" 42)]

    (log/debug (str "type of parents-with-head t=" (type parents-with-head)))
    parents-with-head))

(defn path-to-map [path val]
  (let [feat (first path)]
    (if feat
      {feat (path-to-map (rest path) val)}
      val)))
  

(defn butlast-unless-singleton [the-seq]
  (if (and (not (nil? the-seq))
           (or (vector? the-seq) (seq? the-seq))
           (= (.size the-seq) 1))
    the-seq
    (butlast the-seq)))

(declare add-complement)

;; slow:
;; (time (fo-ps (take 1 (forest/add-complements-to-bolts (forest/gen1 grammar lexicon {:synsem {:cat :verb :subcat '()}}) [:comp] :top lexicon))))
;; fast now:
;; (time (fo-ps (take 1 (forest/add-complements-to-bolts (forest/gen1 grammar lexicon {:synsem {:cat :verb :subcat '()}}) [:comp] :top lexicon))))

;; fast:
;; (time (fo-ps (take 1 (forest/add-complements-to-bolts (take 1 (forest/gen1 grammar lexicon {:synsem {:cat :verb :subcat '()}})) [:comp] :top lexicon))))

;; reasonably fast:
;; (take 5 (repeatedly #(fo-ps (take 1 (forest/add-complements-to-bolts (forest/gen1 grammar (shuffle lexicon) {:synsem {:cat :verb}}) [:comp] :top (shuffle lexicon))))))

;; reasonably fast:
;; (fo-ps (take 10 (repeatedly #(take 1 (forest/add-complements-to-bolts (forest/gen1 (shuffle grammar) (shuffle lexicon) {:synsem {:cat :verb :subcat '()}}) [:comp] :top (shuffle lexicon))))))

(declare do-the-cooler-thing)

(defn do-the-cool-thing [grammar lexicon]
  (time (fo (take 1 (do-the-cooler-thing grammar lexicon 
                                         {:synsem {:cat :verb :sem {:pred :vedere}
                                                   :aux false
                                                   :subcat '()}})))))

(declare add-complements-to-bolts)

(defn do-the-cooler-thing [grammar lexicon spec]
  (let [cool (add-complements-to-bolts 
              (gen1 (shuffle grammar) 
                            (shuffle lexicon)
                            spec)
              [:comp] :top (shuffle lexicon))]
;    cool))
    (add-complements-to-bolts 
     cool
     [:head :comp] :top (shuffle lexicon))))

(defn add-complements-to-bolts [bolts path spec lexicon]
  (if (not (empty? bolts))
    (let [bolt (first bolts)]
      (lazy-cat
       (add-complement bolt path spec lexicon)
       (add-complements-to-bolts (rest bolts) path spec lexicon)))))

;; (forest/add-complement lb [:comp] :top lexicon)
;; (fo-ps (forest/add-complement (first (take 1 (forest/gen1 (shuffle grammar) (shuffle lexicon) {:synsem {:cat :verb :subcat '()}}))) [:comp] :top lexicon))
(defn add-complement [bolt path spec lexicon]
  (let [path-to-parent (butlast path)
        spec (unifyc spec (get-in bolt path))]
    (log/debug (str "add-complement to: " (fo-ps bolt) " with spec " (show-spec spec) " at path: " path))
    (filter (fn [result]
              (not (fail? result)))
            (map (fn [lexeme]
                   (log/debug (str "unifyc: " (fo-ps bolt) " with lexeme: " (fo lexeme)))
                   (let [result
                         (unifyc bolt
                                 (path-to-map path
                                              lexeme))]
                     (log/debug (str "unifyc: " (fo-ps bolt) " with lexeme: " (fo lexeme) " => " (if (not (fail? result))
                                                                                                   (fo-ps result)
                                                                                                   ":fail")))
                     result))
                 lexicon))))

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
      (log/trace (str "parents-given-spec: spec:" (show-spec spec)))
      (log/trace (str "parents-given-spec: phrases:" (fo-ps phrases)))
      (let [cached-parents (get-parent-phrases-for-spec cache spec)]
        (log/trace (str "parents-given-spec cached: " (if (not (nil? cached-parents))
                                                      (.size cached-parents))))
        (if (not (nil? cached-parents))
          cached-parents
          (do
            ;; used to be WARN; turned down to TRACE because it happens so frequently that it's distracting.
            (log/trace (str "no cached entry for spec: " (show-spec spec)))
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
                   (log/trace (str "phrasal-spec: " (:phrase-constraints cache)))
                   (log/trace (str "  will apply to: " spec))
                   (log/trace (str "phrasal-spec: " (show-spec (:phrase-constraints cache))))
                   (log/trace (str "  will apply to: " (show-spec spec)))
                   (:phrase-constraints cache))
                  :top)))

(defn subcat-constraints [subcat-spec]
  (if (= subcat-spec '())
;    {:1 :top}
    :top
    (let [val-1 (get-in subcat-spec [:1])
          val-2 (get-in subcat-spec [:2])
          val-3 (get-in subcat-spec [:3])]
      (conj
       (if (and (not (nil? val-1)) (or (keyword? val-1) (map? val-1) (not (empty? val-1))))
         {:1 val-1}
         {})
       (if (and (not (nil? val-2)) (or (keyword? val-2) (map? val-2) (not (empty? val-2))))
         {:2 val-2}
         {})
       (if (and (not (nil? val-3)) (or (keyword? val-3) (map? val-3) (not (empty? val-3))))
         {:3 val-3}
         {})))))

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

(defn lazy-mapcat-bailout-after [name the-fn the-args tries & [max-tries length-of-args]]
  (let [arg (first the-args)
        max-tries (if (not (nil? max-tries)) max-tries tries)
        ;; assuming that args is not a lazy seq: if so, this (.size call) will realize it.
        length-of-args (if (not (nil? length-of-args)) length-of-args (if (nil? the-args) 0 (.size the-args)))
;        length-of-args (str "(n/a)")
        ]
    (if (and (number? tries) (number? max-tries)) (log/debug (str name ": trying #" (- max-tries tries) "/" max-tries)))
    (if (and (number? tries) (<= tries 0))
      (log/warn (str name ": bailing out@" tries " now: tried: " max-tries " / possible: " length-of-args))
      ;; else: keep trying more possibilities: apply the-fn to the first arg in the-args,
      ;; and concat that with a recursive function call with (rest the-args).
      (if arg
        (let [debug (log/debug (str "function: " name ": with rule=" (fo-ps arg)))
              debug (log/trace (str (:rule arg) " max tries: " max-tries))
              result (the-fn arg)]
          (lazy-cat
           result
           (lazy-mapcat-bailout-after name the-fn 
                                      (rest the-args) 
                                      (if (number? tries) (- tries 1)
                                          tries) ;; not a number: a symbol like :dont-bail-out
                                      max-tries length-of-args)))))))

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
(declare hpcl)
(declare random-lazy-cat)

(defn hl [cache grammar & [spec depth chain]]
  (log/debug (str chain " -> hl@" depth))
  (let [depth (if depth depth 0)
        debug (log/debug (str chain " -> hl@" depth "sp(pre)=" (show-spec spec)))
        spec (phrasal-spec (if spec spec :top) cache)
        head-spec (get-in spec [:head :synsem])
        grammar (lazy-shuffle grammar)]

    (log/debug (str chain " -> hl@" depth "sp=" (show-spec spec)))
    (log/debug (str chain " -> hl@" depth "hsp=" (show-spec head-spec)))

    ;; try every possible lexeme as a candidate head for each phrase:
    ;; use (:comp (cache ..)) as the subset of the lexicon to try.
    (lazy-mapcat-bailout-after
     "hl"
     #(do
        (log/debug (str "hl: trying rule: " (:rule %)))
        (overh % (filter (fn [lexeme]
                           (not (fail? (unifyc (get-in lexeme [:synsem]) head-spec))))
                         (lazy-shuffle (:head (cache (:rule %)))))))
     (parents-given-spec cache spec grammar)
     :dont-bailout)))

(defn cl [cache grammar & [spec depth chain]]
  (lazy-mapcat-bailout-after
   "cl"
   #(do
      (log/debug (str "cl: trying: " (fo-ps %)))
      (log/trace (str "cl: with: " (fo (:comp (cache (:rule %))))))
      (overc % (lazy-shuffle (:comp (cache (:rule %))))))
   grammar
   :dont-bailout))

(defn hp [cache grammar & [spec depth chain]]
  "return a lazy sequence of every possible phrasal head as the head of every rule in rule-set _grammar_."
  (log/debug (str chain " -> hp@" depth ": spec: " (show-spec spec)))
  (let [depth (if depth depth 0)
        spec (phrasal-spec (if spec spec :top) cache)
        spec-info ""
        chain (if chain chain "")
        chain (str chain " -> hp@" depth)
        grammar (filter (fn [rule]
                          (not (fail? rule)))
                        (map (fn [rule]
                               (unifyc rule spec))
                             grammar))
    
        with-hlcl (lazy-mapcat-bailout-after (str chain " -> overh(rule,hlcl)")
                   #(do
                      (log/debug (str chain " -> overh(" (fo-ps %) ")"))
;                      (log/debug (str chain " -> calling hlcl with spec: " (show-spec (get-in % [:head]))))
                      (log/debug (str chain " -> calling hlcl with rule: " (fo-ps %)))
                      (let [hlcl (hlcl cache
                                       grammar
                                       %
;                                       (get-in % [:head])
                                       (+ 1 depth)
                                       (str chain " -> overh(rule,hlcl)"))]
;                        (log/debug (str chain " (overh(rule,hlcl) => " (fo-ps hlcl)))
                        (log/debug (str chain " done with hlcl: overh-ing."))
                        (overh % hlcl)))
                   (shuffle grammar)
                   :dont-bailout)

;        with-hlcp (lazy-mapcat-bailout-after (str chain " -> overh(rule,hlcp)")
;                   #(do
;                      (overh %
;                             (hlcp cache
;                                   grammar
;                                   (get-in % [:head])
;                                   (+ 1 depth)
;                                   (str chain " -> overh(rule,hlcp)"))))
;                   (shuffle grammar)
;                   (if (< depth 3)
;                     :dont-bailout
;                     0))

;        with-hpcl (lazy-mapcat-bailout-after (str chain " -> overh(rule,hpcl)")
;                   #(do
;                      (overh %
;                             (hpcl cache
;                                   grammar
;                                   (get-in % [:head])
;                                   (+ 1 depth)
;                                   (str chain " -> overh(rule,hpcl)"))))
;                   (shuffle grammar)
;                   (if (< depth 4)
;                     :dont-bailout
;                     0))


;        with-hpcp (lazy-mapcat-bailout-after (str chain " -> overh(rule,hpcp)")
;                   #(do
;                      (overh %
;                             (hpcp cache
;                                   grammar
;                                   (get-in % [:head])
;                                   (+ 1 depth)
;                                   (str chain " -> overh(rule,hpcp)"))))
;                   (shuffle grammar)
;                   (if (< depth 4)
;                     :dont-bailout
;                     0))

;        debug (log/trace (str chain ":finished with hpcp as head:" (type with-hpcp)))
        ]
    (random-lazy-cat (shuffle (list (fn [] with-hlcl)
                                    ;(fn [] with-hlcp)
                                    ;(fn [] with-hpcl)
                                    ;(fn [] with-hpcp)
                                    )))))
 
(defn cp [parents-with-heads cache grammar & [depth chain]]
  (let [with-hlcl-as-complement (lazy-mapcat-bailout-after (str chain " -> C:hlcl")
                                                           #(do
                                                              (log/debug
                                                               (str chain " -> C:hlcl with parent: " (fo-ps %)))
                                                              (overc %
                                                                     (hlcl cache grammar
                                                                           {:synsem (get-in % [:comp :synsem] :top)}
                                                                           (+ 1 depth)
                                                                           (str chain " -> C:hlcl"))))
                                                           parents-with-heads
                                                           1)
        with-hpcl-as-complement (lazy-mapcat-bailout-after (str chain " -> C:hpcl")
                                                           #(do
                                                              (log/debug
                                                               (str chain " -> C:hpcl"))
                                                              (overc %
                                                                     (hpcl cache grammar
                                                                           {:synsem (get-in % [:comp :synsem] :top)}
                                                                           (+ 1 depth)
                                                                           (str chain " -> C:hpcl"))))
                                                           parents-with-heads
                                                           1)
        with-hlcp-as-complement (lazy-mapcat-bailout-after (str chain " -> C:hlcp")
                                                           #(do
                                                              (log/debug
                                                               (str chain " -> C:hlcp"))
                                                              (overc %
                                                                     (hpcl cache grammar
                                                                           {:synsem (get-in % [:comp :synsem] :top)}
                                                                           (+ 1 depth)
                                                                           (str chain " -> C:hlcp"))))
                                                           parents-with-heads
                                                           1)
        with-hpcp-as-complement (lazy-mapcat-bailout-after (str chain " -> C:hpcp")
                                                           #(do
                                                              (log/debug
                                                               (str chain " -> C:hpcp"))
                                                              (overc %
                                                                     (hpcl cache grammar
                                                                           {:synsem (get-in % [:comp :synsem] :top)}
                                                                           (+ 1 depth)
                                                                           (str chain " -> C:hpcp"))))
                                                           parents-with-heads
                                                           1)]
    (random-lazy-cat (shuffle (list (fn [] with-hlcl-as-complement)
                                    (fn [] with-hpcl-as-complement)
                                    (fn [] with-hlcp-as-complement)
                                    (fn [] with-hpcp-as-complement)
                                    )))))
                                   
(defn random-lazy-cat [ & [ seqs ]]
  (if (not (empty? seqs))
    (let [the-fn (first seqs)]
      (lazy-cat (the-fn)
                (random-lazy-cat (rest seqs))))))

(defn rlc [ arg ]
  arg)

(defn hlcl [cache grammar & [spec depth chain]]
  "generate all the phrases where the head is a lexeme and the complement is a lexeme"
  (log/debug (str chain ": hlcl@" depth ":" (show-spec spec)))
  (if (fail? spec)
    nil
    (let [depth (if depth depth 0)
          ;; adding {:head {:phrasal false}} because head of hlcl is lexical, not phrasal.

          debug (log/debug (str chain ": hlcl@" depth ": hlcl::spec pre-modified(0) is: " (show-spec spec)))

          debug (log/trace (str "hlcl::spec pre-modified is: " spec))

;          spec (phrasal-spec (if spec spec :top) cache)

          debug (log/debug (str chain ": hlcl@" depth ": hlcl::spec pre-modified(1) is: " (show-spec spec)))

;          spec (unifyc {:head {:phrasal false}} (phrasal-spec (if spec spec :top) cache))
;          spec (phrasal-spec (if spec spec :top) cache)
          spec (unifyc {:head {:phrasal false}} spec)

          debug (log/trace (str "hlcl::spec post-modified is: " spec))

          debug (log/debug (str chain ": hlcl@" depth ": hlcl::spec pre-modified(2) is: " (show-spec spec)))

          spec (unifyc spec
                       {:head {:synsem {:subcat (subcat-constraints (get-in spec [:synsem :subcat]))}}})

          debug (log/debug (str chain ": hlcl@" depth ": hlcl::spec post-modified(2) is: " (show-spec spec)))

          debug (log/trace (str "hlcl::subcat is: " (get-in spec [:synsem :subcat])))
          debug (log/trace (str "hlcl::head's synsem is: " (get-in spec [:head :synsem])))
          debug (log/trace (str "hlcl::head's subcat is: " (get-in spec [:head :synsem :subcat])))

          chain (if chain chain "")]

      (if (fail? spec)
        nil
        ;; parents-with-heads is the lazy sequence of all possible lexical heads attached to all possible grammar rules.
        (let [debug (log/debug (str "calling hl with spec: " (show-spec spec)))
              parents-with-heads
              (hl cache grammar spec depth (str chain " -> hl"))]
          (cl cache parents-with-heads))))))

(defn hlcp [cache grammar & [spec depth chain]]
  "generate all the phrases where the head is a lexeme and the complement is a phrase."
  (let [depth (if depth depth 0)
        ;; adding {:head {:phrasal false}} because head of hlcp is lexical, not phrasal.
        spec (unifyc {:head {:phrasal false}} (phrasal-spec (if spec spec :top) cache))
        spec (unifyc spec
                     {:head {:synsem {:subcat (subcat-constraints (get-in spec [:synsem :subcat]))}}})
        head-spec (get-in spec [:head])
        show-spec ""
        chain (if chain 
                (str chain " :(generating as): "
                     (str "hlcp@" depth " " ""))
                (str "hlcp@" depth " " ""))]
    (if (fail? spec)
      nil
      (do
        (log/debug (str "hlcp:: " chain))

        ;; parents-with-heads is the lazy sequence of all possible heads attached to all possible grammar rules.
        (let [parents-with-heads
              (hl cache grammar spec)]
          (log/info (str "parents-with-heads: " (type parents-with-heads)))
          (lazy-seq (cp parents-with-heads cache grammar depth chain)))))))

(defn hpcl [cache grammar & [spec depth chain]]
  "generate all the phrases where the head is a phrase and the complement is a lexeme."
  (let [depth (if depth depth 0)
        spec (phrasal-spec (if spec spec :top) cache)
        spec (unifyc spec
                     {:head {:synsem {:subcat (subcat-constraints (get-in spec [:synsem :subcat]))}}})
        head-spec (get-in spec [:head])
        show-the-spec ""
        chain (if chain 
                (str chain " -> "
                     (str "hpcl@" depth "" show-the-spec ""))
                (str "hpcl@" depth "" show-the-spec ""))]
    (log/debug (str chain " spec: " (show-spec spec)))
    (let [hp (hp cache grammar head-spec (+ 0 depth) chain)]
      (cl cache hp))))

(defn hpcp [cache grammar & [spec depth chain]]
  "generate all the phrases where the head is a phrase and the complement is a phrase."
  (let [depth (if depth depth 0)
        spec (phrasal-spec (if spec spec :top) cache)
        spec (unifyc spec
                     {:head {:synsem {:subcat (subcat-constraints (get-in spec [:synsem :subcat]))}}})
        head-spec (get-in spec [:head])
        show-spec ""
        chain (if chain 
                (str chain " y->y "
                     (str "hpcp@" depth " [" show-spec "]"))
                (str "hpcp@" depth " [" show-spec "]"))]

    (if (fail? spec)
      nil
      (do
        (let [parents-with-heads
              (hp cache grammar head-spec (+ 0 depth))]
          (cp parents-with-heads cache grammar depth chain))))))

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

