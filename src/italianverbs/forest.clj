(ns italianverbs.forest
  (:refer-clojure :exclude [get-in deref merge resolve find future parents rand-int])
  (:require
   [clojure.core :as core]
   [clojure.set :refer :all]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [italianverbs.cache :refer (build-lex-sch-cache get-comp-phrases-of get-head-phrases-of get-lex
                                                   overc overh overc-with-cache)]
   [italianverbs.lexicon :refer (it)]
   [italianverbs.morphology :refer (fo fo-ps)]
   [italianverbs.unify :as unify]
   [italianverbs.unify :refer (dissoc-paths get-in fail? lazy-shuffle remove-top-values-log show-spec)]))

(def concurrent true)
(defn deref [thing]
  (if concurrent
    (core/deref thing)
    thing))
(defn future [thing]
  (if concurrent
    (core/future thing)
    thing))

(def random-order true)
(defn rand-int [range constant]
  (if random-order
    (core/rand-int range)
    constant))

(defn unifyc [& args]
  (if (first args) 
    (log/trace (str "forest-unify 1 " (fo-ps (first args)))))
  (if (second args) 
    (log/trace (str "forest-unify 2 " (fo-ps (second args)))))
  (apply unify/unifyc args))

(declare lightning-bolt)

(defn add-comp-phrase-to-headed-phrase [parents phrases lexicon & [iter cache path]]
  (log/debug (str "starting add-comp-phrase-to-headed-phrase."))
  (log/debug (str "add-comp-phrase-to-headed-phrase: emptyness of parents: " (empty? parents)))

  (if (not (empty? parents))
    (let [debug (if false (throw (Exception. "GOT HERE: INSIDE MAIN PART OF add-comp-phrase-to-headed-phrase.")))
          debug (log/debug (str "add-comp-phrase-to-headed-phrase is non-empty.."))
          debug (log/debug (str "emptyness of phrases: " (empty? phrases)))
          iter (if (nil? iter) 0 iter)
          parent (first parents)

          cache (if cache cache
                    (do
                      (log/info (str "building cache (" (.size phrases) ")"))
                      (build-lex-sch-cache phrases lexicon)))
          comp-spec
          (dissoc-paths
           (get-in parent '(:comp))
           ;; TODO: do we need to dissoc these paths from the comp spec?
           '((:english :initial)
             (:italian :initial)))

          debug (do (log/debug (str "add-comp-phrase-to-headed-phrase: parent: " (fo-ps parent)))
                    (log/debug (str "add-comp-phrase-to-headed-phrase: comp-spec: " (show-spec comp-spec))))

          comp-phrases-for-parent (if (not (= false (get-in comp-spec '(:phrasal))))
                                    (filter (fn [phrase]
                                              (not (fail? phrase)))
                                            (map (fn [phrase]
                                                   (unifyc phrase comp-spec))
                                                 (get-comp-phrases-of parent cache))))

          comps 
          (if (not (empty? comp-phrases-for-parent))
            (let [lexicon-for-comp (get-lex parent :comp cache lexicon)]
              (log/debug (str "about to call lightning-bolt from add-comp-phrase-to-headed-phrase with head: " (show-spec comp-spec)
                              "; grammar: " (fo-ps comp-phrases-for-parent) "; lexicon size: " (.size lexicon-for-comp)))
              (deref
              (future
                (lightning-bolt
                 comp-spec lexicon-for-comp
                 comp-phrases-for-parent
                 0
                 cache (conj path 
                             {:h-or-c "C"
                              :depth 0
                              :grammar comp-phrases-for-parent
                              :lexicon-size (.size lexicon-for-comp)
                              :spec (show-spec comp-spec)
                              :parents comp-phrases-for-parent})))))
            (list))]

      (lazy-cat
       (overc parent comps)
       (add-comp-phrase-to-headed-phrase (rest parents) phrases lexicon (+ 1 iter) cache path)))))

(def can-log-if-in-sandbox-mode false)

(defn lexical-headed-phrases [parents lexicon phrases depth cache path]
  "return a lazy seq of phrases (maps) whose heads are lexemes."
  (if (not (empty? parents))
    (let [parent (first parents)
          cache (if cache cache
                    (do (log/warn (str "lexical-headed-parents given null cache: building cache from: (" (.size phrases) ")"))
                        (build-lex-sch-cache phrases lexicon)))]
      (log/trace (str "lexical-headed-phrases: looking at parent: " (fo-ps parent)))
      
      (lazy-seq
       (let [phrases-with-lexical-heads (get-lex parent :head cache lexicon)
             result (overh parent phrases-with-lexical-heads)]
         (cons {:parent parent
                :headed-phrases result}
               (lexical-headed-phrases (rest parents) lexicon phrases depth cache path)))))))

(defn phrasal-headed-phrases [parents lexicon grammar depth cache path]
  "return a lazy seq of phrases (maps) whose heads are themselves phrases."
  (if (not (empty? parents))
    (let [parent (first parents) ;; realizes possibly?
          debug (log/trace (str "phrasal-headed-phrases grammar size: " (.size grammar)))
          headed-phrases-of-parent (get-head-phrases-of parent cache)]
      (if (nil? headed-phrases-of-parent)
        (phrasal-headed-phrases (rest parents) lexicon grammar depth cache path)        
        (let [headed-phrases-of-parent (if (nil? headed-phrases-of-parent)
                                         (list)
                                         headed-phrases-of-parent)
              head-spec (dissoc-paths (get-in parent '(:head))
                                      '((:english :initial)
                                        (:italian :initial)))
              debug (log/trace (str "phrasal-headed-phrases: parent's head: " (show-spec head-spec)))]
          (lazy-seq
           (let [debug (log/debug (str "about to call lightning-bolt from phrasal-headed-phrase with parents: " (fo-ps parents)))
                 bolts 
                 (deref (future
                   (lightning-bolt head-spec
                                   lexicon headed-phrases-of-parent (+ 1 depth)
                                   cache
                                   path)))]
             (cons {:parent parent
                    :headed-phrases (overh parents bolts)}
                   (phrasal-headed-phrases (rest parents) lexicon grammar depth cache path)))))))))

;; TODO: move this to inside lightning-bolt.
(defn decode-gen-ordering2 [rand2]
  (cond (= rand2 0)
        "hLcP + hPcP"
        true
        "hPcP + hLcP"))

;; TODO: move this to inside lightning-bolt.
(defn decode-generation-ordering [rand1 rand2]
  (cond (= rand1 0)
        (str "hLcL + " (decode-gen-ordering2 rand2) " + hPcL")
        (= rand 1)
        (str (decode-gen-ordering2 rand2) " + hLcL + hPcL")
        (= rand 2)
        (str (decode-gen-ordering2 rand2) " + hPcL + hLcL")
        true
        (str "hPcL + "  (decode-gen-ordering2 rand2) " + hLcL")))

(defn parents-at-this-depth [head-spec phrases depth]
  "subset of phrases possible at this depth where the phrase's head is the given head."
  (if (nil? phrases)
    (log/trace (str "no parents for spec: " (show-spec head-spec) " at depth: " depth)))
  (log/trace (str "parents-at-this-depth: head-spec:" (show-spec head-spec)))
  (log/trace (str "parents-at-this-depth: phrases:" (fo-ps phrases)))
  (filter (fn [each-unified-parent]
            (not (fail? each-unified-parent)))
          (map (fn [each-phrase]
                 (unifyc each-phrase head-spec))
          ;; TODO: possibly: remove-paths such as (subcat) from head: would make it easier to call with lexemes:
          ;; e.g. "generate a sentence whose head is the word 'mangiare'" (i.e. user passes the lexical entry as
          ;; head param of (lightning-bolt)".
               phrases)))

(defn parents-with-phrasal-complements-candidates [parents-with-lexical-heads parents-with-phrasal-heads
                                                   rand-parent-type-order]
  (if false (list)
      (let [parents-with-lexical-heads (filter (fn [parent]
                                                 (do (log/debug "checking parent (1)")
                                                     (not (= false (get-in parent '(:comp :phrasal))))))
                                               parents-with-lexical-heads)
            parents-with-phrasal-heads (filter (fn [parent]
                                                 (do (log/debug "checking parent (2)")
                                                     (not (= false (get-in parent '(:comp :phrasal))))))
                                               parents-with-phrasal-heads)]
        (cond (= rand-parent-type-order 0)
              (lazy-cat parents-with-lexical-heads parents-with-phrasal-heads)
              true
              (lazy-cat parents-with-phrasal-heads parents-with-lexical-heads)))))

(defn log-path [path log-fn & [ depth]]
  (let [depth (if depth depth 0)
        print-blank-line false]
    (if (> (.size path) 0)
      (let [h-or-c (:h-or-c (first path))
            depth (:depth (first path))
            grammar (:grammar (first path))
            lexicon-size (:lexicon-size (first path))
            spec (:spec (first path))
            parents (fo-ps (:parents (first path)))]
        (log-fn (str "LB@[" depth "]: " h-or-c ":" spec))
        (log/trace (str "   grammar: " (fo-ps grammar) "; lexicon size: " lexicon-size))
        (log-fn (str "   applicable rules: " (fo-ps parents)))
        (log-path (rest path) log-fn (+ depth 1)))
      (if print-blank-line (log-fn (str ""))))))

(def maxdepth 4)

(defn lazy-cat-them-all [vals]
  (if (not (empty? vals))
    (do
      (log/info (str "lazy-cat-them-all: first first vals: " (fo (first (first vals)))))
      (lazy-cat (first vals)
                (lazy-cat-them-all (rest vals))))))

;; TODO: s/head/head-spec/
(defn lightning-bolt [ & [head lexicon grammar depth cache path]]
  (let [depth (if depth depth 0)]
  (cond (nil? lexicon)
        (do
          (log/warn "lightning-bolt: lexicon was nil.")
          nil)

        (nil? grammar)
        (do
          (log/warn "lightning-bolt: grammar was nil.")
          nil)
        
        (> depth maxdepth)
        nil

        (and (not (nil? lexicon))
             (not (nil? grammar)))
        (do

          (let [parents-at-this-depth (parents-at-this-depth head (lazy-shuffle grammar) depth)

                path (if path path [])
                path (if path (conj path
                                    ;; add one element representing this call of lightning-bolt.
                                    {:depth depth
                                     :grammar grammar
                                     :h-or-c "H"
                                     :lexicon-size (.size lexicon)
                                     :spec (show-spec head)
                                     :parents parents-at-this-depth}))]
            (log-path path (fn [x] (log/debug x)))

            (let [head (if head head :top)
                  ;; TODO: will probably remove this or make it only turned on in special cases.
                  ;; lightning-bolt should be efficient enough to handle :top as a spec
                  ;; efficiently.
                  too-general (if (= head :top)
                                (if true nil
                                    (throw (Exception. (str ": head-spec is too general: " head)))))

                  phrases grammar;; TODO: rename all uses of phrases to grammar.
                
                  depth (if depth depth 0)
                
                  rand-order (if true (rand-int 3 0) 1)
                
                  rand-parent-type-order (rand-int 2 0)
                
                  log (log/debug (str "rand-order at depth:" depth " is: "
                                      (decode-generation-ordering rand-order rand-parent-type-order)
                                      "(rand-order=" rand-order ";rand-parent-type-order=" rand-parent-type-order ")"))
            
                  ]
              (cond
               (empty? parents-at-this-depth)
               (do (log/trace "lb: no parents at depth:" depth ";returning empty list.")
                   nil)

               true
               (let [cache (if cache cache (build-lex-sch-cache phrases lexicon phrases))

                     phrasal-headed-phrases (phrasal-headed-phrases parents-at-this-depth (lazy-shuffle lexicon)
                                                                    phrases depth cache path)

                     parents-with-phrasal-heads-for-comp-phrases
                     (mapcat (fn [each-kv]
                               (let [parent (:parent each-kv)]
                                 (if (not (= false (get-in parent '(:comp :phrasal))))
                                   (let [phrases (:headed-phrases each-kv)]
                                     phrases))))
                             phrasal-headed-phrases)

                     phrasal-headed-phrases
                     (mapcat (fn [each-kv]
                               (let [phrases (:headed-phrases each-kv)]
                                 phrases))
                             phrasal-headed-phrases)

                     lexical-headed-phrases (lexical-headed-phrases parents-at-this-depth (lazy-shuffle lexicon)
                                                                    phrases depth cache path)

                     parents-with-lexical-heads-for-comp-phrases 
                     (mapcat (fn [each-kv]
                               (let [parent (:parent each-kv)]
                                 (if (not (= false (get-in parent '(:comp :phrasal))))
                                   (let [phrases (:headed-phrases each-kv)]
                                     phrases))))
                             lexical-headed-phrases)

                     lexical-headed-phrases
                     (mapcat (fn [each-kv]
                               (let [phrases (:headed-phrases each-kv)]
                                 phrases))
                             lexical-headed-phrases)
                     
               
                     ;; trees where both the head and comp are lexemes.
                     one-level-trees
                     (if (not (empty? lexical-headed-phrases))
                       (overc-with-cache lexical-headed-phrases cache (lazy-shuffle lexicon)))

                     parents-with-phrasal-complements-candidates
                     (parents-with-phrasal-complements-candidates
                      parents-with-phrasal-heads-for-comp-phrases
                      parents-with-lexical-heads-for-comp-phrases
                      rand-parent-type-order)

                     ]

           (cond (= rand-order 0) ;; hLcL + rand2 + hPcL
                 (lazy-cat
                  one-level-trees
                  (add-comp-phrase-to-headed-phrase parents-with-phrasal-complements-candidates
                                                    phrases (lazy-shuffle lexicon) 0 cache path)
                  (overc-with-cache phrasal-headed-phrases cache lexicon))

                 (= rand-order 1) ;; rand2 + hLcL + hPcL
                 (lazy-cat
                  (add-comp-phrase-to-headed-phrase parents-with-phrasal-complements-candidates
                                                    phrases (lazy-shuffle lexicon) 0 cache path)
                  one-level-trees
                  (overc-with-cache phrasal-headed-phrases cache lexicon))

                 (= rand-order 2) ;; hPcL + rand2 + hLcL
                 (lazy-cat
                  (overc-with-cache phrasal-headed-phrases cache lexicon)
                  (add-comp-phrase-to-headed-phrase parents-with-phrasal-complements-candidates
                                                    phrases (lazy-shuffle lexicon) 0 cache path)
                  one-level-trees
)
)))))))))

;; aliases that might be easier to use in a repl:
(defn lb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

(defn lightningb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

