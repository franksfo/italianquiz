(ns italianverbs.forest
  (:refer-clojure :exclude [get-in deref merge resolve find future parents rand-int])
  (:require
   [clojure.core :as core]
   [clojure.set :refer :all]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [italianverbs.cache :refer (build-lex-sch-cache get-comp-phrases-of get-head-phrases-of get-lex
                                                   overc overh overc-with-cache overh-with-cache)]
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

(defn headed-phrase-add-comp [parents phrases lexicon & [iter cache path]]
  (if (not (empty? parents))
    (let [iter (if (nil? iter) 0 iter)
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

          debug (do (log/debug (str "hpac: parent: " headed-phrase-add-comp))
                    (log/debug (str "hpac: comp-spec: " (remove-top-values-log comp-spec))))


          comp-phrases-for-parent (filter (fn [phrase]
                                            (not (fail? phrase)))
                                          (map (fn [phrase]
                                                 (unifyc phrase comp-spec))
                                               (get-comp-phrases-of parent cache)))
          comp-phrases-for-parent (if (nil? comp-phrases-for-parent) (list)
                                      comp-phrases-for-parent)

;;          debug (log/trace (str "SIZE OF COMP-PHRASES-FOR-PARENT:" (:comment parent) " IS " (.size comp-phrases-for-parent)))

          comps 
          (deref (future
            (lightning-bolt
             comp-spec (get-lex parent :comp cache lexicon)
             comp-phrases-for-parent
             0
             cache (conj path 
                         {:h-or-c "C"
                          :depth 0
                          :spec (show-spec comp-spec)
                          :parents comp-phrases-for-parent}))))]

      (if (not (empty? comps))
        (do
          (log/debug (str "headed-phrase-add-comp: first comp is: " (fo-ps (first comps)) " which we will add to parent: " (fo-ps parent)))
          (lazy-cat
           (overc parent comps)
           (headed-phrase-add-comp (rest parents) phrases lexicon (+ 1 iter) cache path)))
        (headed-phrase-add-comp (rest parents) phrases lexicon (+ 1 iter) cache path)))))

(def can-log-if-in-sandbox-mode false)

(defn lexical-headed-phrases [parents lexicon phrases depth cache path]
  "return a lazy seq of phrases (maps) whose heads are lexemes."
  (if (not (empty? parents))
    (let [parent (first parents)
          cache (if cache cache
                    (do (log/warn (str "lexical-headed-parents given null cache: building cache from: (" (.size phrases) ")"))
                        (build-lex-sch-cache phrases lexicon)))]
      (log/debug (str "lhp: checking parent: " (fo-ps parent)))
      (lazy-seq
       (let [result (overh parent (get-lex parent :head cache lexicon))]
         (cons {:parent parent
                :headed-phrases result}
               (lexical-headed-phrases (rest parents) lexicon phrases depth cache path)))))))

(defn phrasal-headed-phrases [parents lexicon grammar depth cache path]
  "return a lazy seq of phrases (maps) whose heads are themselves phrases."
  (if (not (empty? parents))
    (let [parent (first parents) ;; realizes possibly?
          debug (log/debug (str "phrasal-headed-phrases grammar: " (.size grammar)))
          headed-phrases-of-parent (get-head-phrases-of parent cache)
          headed-phrases-of-parent (if (nil? headed-phrases-of-parent)
                                     (list)
                                     headed-phrases-of-parent)
;          headed-phrases-of-parent phrases
          head-spec (dissoc-paths (get-in parent '(:head))
                                  '((:english :initial)
                                    (:italian :initial)))
          debug (log/debug (str "phrasal-headed-phrases: parent's head: " (show-spec head-spec)))

          ]
      (lazy-seq
       (cons {:parent parent
              :headed-phrases (let [bolts 
                                    (deref (future
                                             (lightning-bolt head-spec
                                                             lexicon headed-phrases-of-parent (+ 1 depth)
                                                             cache
                                                             path)))]
                                (overh parents bolts))}
             (phrasal-headed-phrases (rest parents) lexicon grammar depth cache path))))))

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
  (log/debug (str "parents-at-this-depth: head-spec:" (remove-top-values-log head-spec)))
  (log/debug (str "parents-at-this-depth: phrases:" (fo-ps phrases)))
  (filter (fn [each-unified-parent]
            (not (fail? each-unified-parent)))
          (map (fn [each-phrase]
                 (unifyc each-phrase head-spec))
          ;; TODO: possibly: remove-paths such as (subcat) from head: would make it easier to call with lexemes:
          ;; e.g. "generate a sentence whose head is the word 'mangiare'" (i.e. user passes the lexical entry as
          ;; head param of (lightning-bolt)".
               phrases)))

(defn parents-with-phrasal-complements [parents-with-lexical-heads parents-with-phrasal-heads
                                        rand-parent-type-order]
  (let [parents-with-lexical-heads (filter (fn [parent]
                                             (not (= false (get-in parent '(:comp :phrasal)))))
                                           parents-with-lexical-heads)
        parents-with-phrasal-heads (filter (fn [parent]
                                             (not (= false (get-in parent '(:comp :phrasal)))))
                                           parents-with-phrasal-heads)]
    (cond (= rand-parent-type-order 0)
          (lazy-cat parents-with-lexical-heads parents-with-phrasal-heads)
          true
          (lazy-cat parents-with-phrasal-heads parents-with-lexical-heads))))

(defn log-path [path log-fn & [ depth]]
  (let [depth (if depth depth 0)
        print-blank-line false]
    (if (> (.size path) 0)
      (let [h-or-c (:h-or-c (first path))
            depth (:depth (first path))
            spec (:spec (first path))
            parents (fo-ps (:parents (first path)))]
        (log-fn (str "LB@[" depth "]: " h-or-c "; spec=" spec))
        (log-fn (str "   " parents))
        (log-path (rest path) log-fn (+ depth 1)))
      (if print-blank-line (log-fn (str ""))))))

;; TODO: s/head/head-spec/
(defn lightning-bolt [ & [head lexicon phrases depth cache path]]

  (log/debug (str "lightning-bolt with lexicon size: " 
                  (.size lexicon) " and grammar size: "
                  (.size phrases) "."))

  (let [maxdepth 3
        head (if head head :top)
        ;; TODO: will probably remove this or make it only turned on in special cases.
        ;; lightning-bolt should be efficient enough to handle :top as a spec
        ;; efficiently.
        too-general (if (= head :top)
                      (throw (Exception. (str ": head-spec is too general: " head))))

        log (log/info (str "PHRASES: " (fo-ps phrases)))

        too-many (if (> (.size phrases) 1)
                   (throw (Exception. (str " too many phrases: should have only been one."))))

        remove-top-values (remove-top-values-log head)

        depth (if depth depth 0)

        rand-order (rand-int 3 0)

        rand-parent-type-order (rand-int 2 0)

        log (log/debug (str "rand-order at depth:" depth " is: "
                            (decode-generation-ordering rand-order rand-parent-type-order)
                            "(rand-order=" rand-order ";rand-parent-type-order=" rand-parent-type-order ")"))

        parents-at-this-depth (parents-at-this-depth head phrases depth)

        cache (if cache cache (build-lex-sch-cache phrases lexicon phrases))]
    (cond
     (> depth maxdepth)
     nil

     (empty? parents-at-this-depth)
     (do (log/debug "lb: no parents at depth:" depth ";returning empty list.")
         nil)

     true
     (let [debug (log/debug (str "about to call lexical-headed-phrases with phrases size: " (.size phrases)))
           debug (log/debug (str "about to call lexical-headed-phrases with parents-at-this-depth size: "
                                 (.size parents-at-this-depth)))
           debug (log/debug (str "about to call lexical-headed-phrases with lexicon size: "
                                 (.size lexicon)))
           
           lexical-headed-phrases 
           (let [lexical-headed-phrases (lexical-headed-phrases parents-at-this-depth
                                                                (lazy-shuffle lexicon)
                                                                phrases
                                                                depth
                                                                cache
                                                                path)]
             (if (empty? lexical-headed-phrases)
               (log/debug (str "no lexical-headed-phrases."))
               (log/debug (str "lexical-headed-phrases is non-empty; the first is: " (fo-ps (:parent (first lexical-headed-phrases))))))
             lexical-headed-phrases)

           path (if path (conj path
                               {:h-or-c "H"
                                :depth depth
                                :spec remove-top-values
                                :parents parents-at-this-depth})
                    ;; first element of path:
                    [ {:h-or-c "H"
                       :depth depth
                       :spec remove-top-values
                       :parents parents-at-this-depth}])
           log (log-path path (fn [x] (log/info x)))

           parents-with-phrasal-head-map (phrasal-headed-phrases parents-at-this-depth lexicon
                                                                 phrases depth
                                                                 cache path)

;           debug (do (log/debug (str "parents-with-phrasal-head-map size: " (.size parents-with-phrasal-head-map)))
;                     (log/debug (str "parents-with-phrasal-head-map: " (string/join " " parents-with-phrasal-head-map))))

           parents-with-phrasal-head 
           (let [parents-with-phrasal-head (mapcat (fn [each-kv]
                                                     (let [phrases (:headed-phrases each-kv)]
                                                       phrases))
                                                   parents-with-phrasal-head-map)]
             (if (empty? parents-with-phrasal-head)
               (log/debug (str "hP: empty."))
               (log/debug (str "hP: nonempty; first:"
                               (fo-ps (first parents-with-phrasal-head)))))
             parents-with-phrasal-head)

           debug (log/debug (str "size of lexical-headed-phrases: " (.size lexical-headed-phrases)))

           debug (log/info (str "lexical-headed-phrases: " (string/join (map (fn [phr] 
                                                                                (fo-ps (:parent phr)))
                                                                              lexical-headed-phrases))))


           debug (log/debug (str "====="))
           debug (log/debug (str "first lexical-headed-phrases: " (fo-ps (:parent (first lexical-headed-phrases)))))
           debug (log/debug (str "====="))

           parents-with-lexical-heads 
           (fn []
             (let [parents-with-lexical-heads (mapcat (fn [each-kv]
                                                        (let [phrases (:headed-phrases each-kv)]
                                                          phrases))
                                                      lexical-headed-phrases)]
               (if (empty? parents-with-lexical-heads)
                 (log/debug (str "hL: empty."))
                 (log/debug (str "hL: nonempty; first: " (fo-ps (first parents-with-lexical-heads)))))
               parents-with-lexical-heads))

           ;; TODO: (lazy-shuffle) this.
           ;; TODO: cache this.
           parents-with-phrasal-heads-for-comp-phrases 
           (fn [] 
             (let [parents-with-phrasal-heads-for-comp-phrases 
                   (mapcat (fn [each-kv]
                             (let [parent (:parent each-kv)]
                               (if (not (= false (get-in parent '(:comp :phrasal))))
                                 (let [phrases (:headed-phrases each-kv)]
                                   phrases))))
                           parents-with-phrasal-head-map)]
               (if (empty? parents-with-phrasal-heads-for-comp-phrases)
                 (log/trace (str "cP is empty."))
                 (log/debug (str "cP is nonempty; first: " (fo-ps (first parents-with-phrasal-heads-for-comp-phrases)))))
               parents-with-phrasal-heads-for-comp-phrases))

           parents-with-lexical-heads-for-comp-phrases 
           (fn [] (mapcat (fn [each-kv]
                            (let [parent (:parent each-kv)]
                              (if (not (= false (get-in parent '(:comp :phrasal))))
                                (let [phrases (:headed-phrases each-kv)]
                                  phrases))))
                          lexical-headed-phrases))

           one-level-trees
           (fn []
             (let [parents-with-lexical-heads (parents-with-lexical-heads)
                   one-level-trees
                   (if (not (empty? parents-with-lexical-heads))
                     (overc-with-cache parents-with-lexical-heads cache (lazy-shuffle lexicon)))]
               (if (empty? one-level-trees)
                 (log/debug (str "one-level-trees is empty."))
                 (log/debug (str "one-level-trees is not empty; first is: " (fo (first one-level-trees)))))
               one-level-trees))

           with-phrasal-comps 
           (fn []
             (let [with-phrasal-comps
                   (headed-phrase-add-comp (parents-with-phrasal-complements
                                            (parents-with-phrasal-heads-for-comp-phrases)
                                            (parents-with-lexical-heads-for-comp-phrases)
                                            rand-parent-type-order)
                                           phrases (lazy-shuffle lexicon) 0 cache path)]
               (if (empty? with-phrasal-comps)
                 (log/debug (str "cP is empty."))
                 (log/debug (str "cP is not empty; first is: " (fo-ps (first with-phrasal-comps)))))
               with-phrasal-comps))

          ]

       (cond (= rand-order 0) ;; hLcL + rand2 + hPcL
             (lazy-cat
              (one-level-trees)
              (with-phrasal-comps)
              (overc-with-cache parents-with-phrasal-head cache lexicon))


             (= rand-order 1) ;; rand2 + hLcL + hPcL
             (lazy-cat
              (with-phrasal-comps)
              (one-level-trees)
              (overc-with-cache parents-with-phrasal-head cache lexicon))

             (= rand-order 2) ;; hPcL + rand2 + hLcL
             (lazy-cat
              (overc-with-cache parents-with-phrasal-head cache lexicon)
              (with-phrasal-comps)
              (one-level-trees)))))))


;; aliases that might be easier to use in a repl:
(defn lb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

(defn lightningb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

