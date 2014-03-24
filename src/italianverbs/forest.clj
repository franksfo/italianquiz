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
                                                   overc overh overh-with-cache overc-with-cache)]
   [italianverbs.lexicon :refer (it)]
   [italianverbs.morphology :refer (fo fo-ps)]
   [italianverbs.over :as over]
   [italianverbs.unify :as unify]
   [italianverbs.unify :refer (dissoc-paths get-in fail? lazy-shuffle remove-top-values-log show-spec unifyc)]))

(def concurrent false)

(def random-order true)
(defn rand-int [range constant]
  (if random-order
    (core/rand-int range)
    constant))

(declare lightning-bolt)
(declare lbl)

(defn add-comp-phrase-to-headed-phrase [parents phrases & [cache supplied-comp-spec]]
  (if (not (empty? parents))
    (let [debug
          (do
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
          (lbl phrases cache comp-spec)]
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
                 debug (log/debug (str " with head-spec: " head-spec))
                 bolts (lbl grammar cache head-spec (+ 1 depth))]
             (overh phrases bolts))
           (phrasal-headed-phrases (rest phrases) grammar depth cache path)))))))

(defn parents-at-this-depth [head-spec phrases depth]
  "subset of phrases possible at this depth where the phrase's head is the given head."
  (if (nil? phrases)
    (log/trace (str "no parents for spec: " (show-spec head-spec) " at depth: " depth)))
  (log/debug (str "parents-at-this-depth: head-spec:" (show-spec head-spec)))
  (log/debug (str "parents-at-this-depth: phrases:" (fo-ps phrases)))
  (filter #(not (fail? %))
          (map (fn [each-phrase]
                 (unifyc each-phrase head-spec))
          ;; TODO: possibly: remove-paths such as (subcat) from head: would make it easier to call with lexemes:
          ;; e.g. "generate a sentence whose head is the word 'mangiare'" (i.e. user passes the lexical entry as
          ;; head param of (lightning-bolt)".
               phrases)))

(defn lazy-cats [lists & [ show-first ]]
  (if (not (empty? lists))
    (if (not (empty? (first lists)))
      (do
        (if show-first
          (log/info (str "lazy-cats: first: " (fo-ps (first (first lists))))))
        (lazy-cat (first lists)
                  (lazy-cats (rest lists))))
      (lazy-cats (rest lists)))))

(defn headed-phrases [parents-with-lexical-heads parents-with-phrasal-heads]
  (let [parents-with-lexical-heads
        (if parents-with-lexical-heads
          (filter (fn [parent]
                    (do (log/trace "checking parent (1)")
                        (not (= false (get-in parent '(:comp :phrasal))))))
                  parents-with-lexical-heads)
          (list))

        parents-with-phrasal-heads
        (if parents-with-phrasal-heads
          (filter (fn [parent]
                    (do (log/trace "checking parent (2)")
                        (not (= false (get-in parent '(:comp :phrasal))))))
                  parents-with-phrasal-heads)
          (list))
        cats
        (lazy-cat
         parents-with-lexical-heads parents-with-phrasal-heads)]
    (do
      (if (not (empty? cats))
        (log/trace (str "first headed-phrases: " (fo-ps (first cats))))
        (log/debug (str " no headed-phrases.")))
      cats)))

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

(def maxdepth 4)

(defn lbl [grammar cache & [ spec depth]]
  "lightning-bolt lite"
  (let [depth (if depth depth 0)
        spec (if spec spec :top)
        parents (map #(unifyc % spec)
                     grammar)]
    (cond 
     (> depth maxdepth)
     nil

     true
     (let [parents-at-this-depth (parents-at-this-depth spec (lazy-shuffle grammar) depth)

           lexical-headed-phrases
           (map #(overh % (lazy-shuffle (:head (cache (:rule %)))))
                parents-at-this-depth)

           phrasal-headed-phrases
           (if (not (= false (get-in spec [:head :phrasal])))
             (phrasal-headed-phrases parents-at-this-depth
                                     grammar depth cache nil))
           one-level-trees
           (mapcat #(overc % (lazy-shuffle (:comp (cache (:rule (first parents-at-this-depth))))))
                   lexical-headed-phrases)
           
           headed-phrases
           (headed-phrases
            phrasal-headed-phrases
            lexical-headed-phrases)
           
           with-phrasal-complement
           (if (not (= false (get-in spec [:comp :phrasal])))
               (add-comp-phrase-to-headed-phrase headed-phrases
                                                 grammar cache
                                                 :top))
           
           hpcl (overc-with-cache phrasal-headed-phrases cache)
           
           ]
       (lazy-cats (lazy-shuffle (list one-level-trees with-phrasal-complement hpcl)))))))

;; aliases that might be easier to use in a repl:
(defn lb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

(defn lightningb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

