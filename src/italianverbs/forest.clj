(ns italianverbs.forest
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [clojure.set :refer :all]
   [clojure.tools.logging :as log]
   ;; italianverbs.config is not used yet but hopefully will be in the future.
   [italianverbs.config :as config]
   [italianverbs.lexiconfn :refer (sem-impl)]
   [italianverbs.morphology :refer (fo fo-ps)]
   [italianverbs.over :refer (overc overh get-lex overc-with-cache overh-with-cache)]
   [italianverbs.unify :refer :all]))

(declare lightning-bolt)

(defn show-spec [spec]
  (remove-top-values-log (dissoc-paths spec '((:english :initial)
                                              (:italian :initial)
                                              (:synsem :subcat)
                                              (:synsem :sem-mod)
                                              (:synsem :essere)
                                              (:synsem :agr)
                                              (:synsem :pronoun)
                                              (:synsem :sem :tense)
                                              (:synsem :sem :obj :tense)
                                              (:synsem :sem :mod)
                                              (:synsem :infl)))))

(defn build-lex-sch-cache [phrases lexicon]
  (if (not (empty? phrases))
    (conj
     {(:comment (first phrases))
      {:comp
       (filter (fn [lex]
                 (not (fail? (unifyc (first phrases)
                                     {:comp lex}))))
               lexicon)
       :head
       (filter (fn [lex]
                 (not (fail? (unifyc (first phrases)
                                     {:head lex}))))
               lexicon)}}
     (build-lex-sch-cache (rest phrases) lexicon))
    {}))

(defn headed-phrase-add-comp [parents phrases lexicon & [iter path-to-here cache]]
  (if (not (empty? parents))
    (let [iter (if (nil? iter) 0 iter)

          debug (if (nil? path-to-here)  (log/warn (str "COMP-PHRASES PATH-TO-HERE IS NULL.")))
          debug (if (= "" path-to-here)  (do (log/error (str "COMP-PHRASES PATH-TO-HERE IS EMPTYSTRING."))
                                             (throw (Exception. (str "COMP-PHRASES PATH-TO-HERE IS EMPTYSTRING.")))))
          debug (if (= "/" path-to-here) (log/warn (str "COMP-PHRASES PATH-TO-HERE IS SLASH.")))

          path-to-here (cond (nil? path-to-here)
                             "/"
                             (= "" path-to-here)
                             "/"
                             true path-to-here)
          parent (first parents)
          cache (if cache cache
                    (do
                      (log/info (str "building cache (" (.size phrases) ")"))
                      (build-lex-sch-cache phrases lexicon)))
          comp-spec
          (dissoc-paths
           (get-in parent '(:comp))
           '((:synsem :subcat)
             (:english :initial)
             (:italian :initial)))
          debug (log/debug (str "in headed-phrase-add-comp with spec: " (show-spec comp-spec) " for head: " path-to-here "/[H " (fo (get-in parent '(:head))) "]"))
          debug (log/debug (str "ABOUT TO CALL LB FROM headed-phrase-add-comp@" path-to-here))
]
      (lazy-cat
       (overc parent
              (lightning-bolt
               comp-spec (get-lex parent :comp cache lexicon)
               phrases 0
               (str path-to-here "/[C " (show-spec comp-spec) "]") cache))
       (headed-phrase-add-comp (rest parents) phrases lexicon (+ 1 iter) path-to-here cache)))))

(defn lexical-headed-phrases [parents lexicon phrases depth cache]
  "return a lazy seq of phrases (maps) whose heads are lexemes."
  (if (not (empty? parents))
    (let [parent (first parents)
          cache (if cache cache
                    (do (log/warn (str "lexical-headed-parents given null cache: building cache from: (" (.size phrases) ")"))
                        (build-lex-sch-cache phrases lexicon)))]
      (lazy-seq
       (do
         (log/debug (str "lexical-headed-phrases: with parent: " (fo-ps parent)))
         (cons {:parent parent
                :headed-phrases (overh parent (get-lex parent :head cache lexicon))}
               (lexical-headed-phrases (rest parents) lexicon phrases depth cache)))))))

(defn phrasal-headed-phrases [parents lexicon phrases depth path-to-here cache]
  "return a lazy seq of phrases (maps) whose heads are themselves phrases."
  (if (not (empty? parents))
    (let [parent (first parents)
          cache (if cache cache
                    (let [log (log/info (str "building cache (" (.size phrases) ")"))]
                      (build-lex-sch-cache phrases lexicon)))]
      (lazy-seq
       (cons {:parent parent
              :headed-phrases (let [path-to-here (str path-to-here "/[H " (show-spec (get-in parent '(:head))) "]")
                                    debug (log/debug (str "ABOUT TO CALL LB FROM phrasal-headed-phrases@" path-to-here))
                                    bolts (lightning-bolt (get-in parent '(:head))
                                                          lexicon phrases (+ 1 depth)
                                                          path-to-here
                                                          cache)]
                                (overh parents bolts))}
             (do
               (log/debug (str "DONE CALLING LB FROM phrasal-headed-phrases; proceeding with rest of parents."))
               (phrasal-headed-phrases (rest parents) lexicon phrases depth path-to-here cache)))))))

;; TODO: move this to inside lightning-bolt.
(defn decode-gen-ordering2 [rand2]
  (cond (= rand2 0)
        "hLcP + hPcP"
        true
        "hPcP + hLcP"))

;; TODO: make option to just call (lazy-cat seq1 seq2 seq3) for efficiency:
;; this is simply a diagnostic tool.
(defn try-all-debug [order seq1 seq2 seq3 seq1-label seq2-label seq3-label path]
  (log/debug (str "order:" order))
  (log/debug (str "seq1 type:" (type seq1)))
  (log/debug (str "seq2 type:" (type seq2)))
  (log/debug (str "seq3 type:" (type seq3)))
  (if (not (empty? seq1))
    (lazy-seq
     (cons
      (let [first-of-seq1 (first seq1)]
        (do
          (log/debug (str "try-all ("seq1-label"@" path ") candidate:" (fo-ps first-of-seq1)))
          first-of-seq1))
      (try-all-debug order (rest seq1) seq2 seq3 seq1-label seq2-label seq3-label path)))
    (if (not (empty? seq2))
      (do (log/debug (str "try-all-debug: doing seq2."))
          (try-all-debug order
                         seq2 seq3 nil seq2-label seq3-label nil path))
      (if (not (empty? seq3))
        (do (log/debug (str "try-all-debug: doing seq3."))
            (try-all-debug
             order seq3 nil nil seq3-label nil nil path))))))

(defn try-all [order seq1 seq2 seq3 seq1-label seq2-label seq3-label path]
  (if true
    (try-all-debug order seq1 seq2 seq3 seq1-label seq2-label seq3-label path)
    (lazy-cat seq1 seq2 seq3)))

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

(defn parents-at-this-depth [head phrases depth]
  "subset of phrases possible at this depth where the phrase's head is the given head."
  (log/debug (str "parents-at-this-depth d" depth " starting with head: " (show-spec head)))
  (let [result
        (filter (fn [each]
                  (not (fail? each)))
                (map (fn [phrase]
                       ;; TODO: possibly: remove-paths such as (subcat) from head: would make it easier to call with lexemes:
                       ;; e.g. "generate a sentence whose head is the word 'mangiare'" (i.e. user passes the lexical entry as
                       ;; head param of (lightning-bolt)".
                       (unifyc phrase head))
                     (cond (= depth 0) ;; if depth is 0 (top-level), only allow phrases with empty subcat.
                           (filter (fn [phrase]
                                     (or true
                                     (empty? (get-in phrase '(:synsem :subcat)))))
                                   phrases)
                           (= depth 1)
                           (filter (fn [phrase]
                                     (or true
                                     (and (not (empty? (get-in phrase '(:synsem :subcat))))
                                          (empty? (get-in phrase '(:synsem :subcat :2))))))
                                   phrases)
                           true
                           '())))]
    ;; REALIZES:
    (log/trace (str "parents-at-this-depth (depth=" depth ") for head: " (show-spec head) " returning result with size: " (.size result)))
    result))

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

;; TODO: s/head/head-spec/
(defn lightning-bolt [ & [head lexicon phrases depth path-to-here cache]]
  (let [maxdepth 2
        head (if head head :top)
        remove-top-values (remove-top-values-log head)
        depth (if depth depth 0)
        path-to-here (cond (nil? path-to-here)
                           "" ;; root path
                           (= "" path-to-here)
                           "" ;; root path
                           true path-to-here)
        log (log/debug (str "lightning-bolt d" depth ":" path-to-here "/[H " remove-top-values "]"))

        parents-at-this-depth (parents-at-this-depth head phrases depth)
        ;; the subset of the lexicon that matches the head-spec, with a few paths removed from the head-spec
        ;; that would cause unification failure because they are specific to the desired final top-level phrase,
        ;; not the lexical entry.
        cache (if cache cache (build-lex-sch-cache phrases lexicon))]
    (cond

     (empty? parents-at-this-depth)
     (do (log/debug "lb: no parents at depth:" depth ";returning empty list.")
         nil)

     true
     (let [debug (log/debug (str "lightning-bolt d" depth ":" path-to-here "/[H " remove-top-values "]; first parent at this depth: "
                                 (fo-ps (first parents-at-this-depth))))
           parents-with-phrasal-head-map (if (< depth maxdepth)
                                           (phrasal-headed-phrases
                                            parents-at-this-depth
                                            lexicon
                                            phrases
                                            depth
                                            path-to-here
                                            cache))

           lexical-headed-phrases (lexical-headed-phrases
                                   parents-at-this-depth
                                   lexicon
                                   phrases
                                   depth
                                   cache)

           debug (log/debug(str "lb begin pwph @" path-to-here))
           parents-with-phrasal-head (mapcat (fn [each-kv]
                                               (let [parent (:parent each-kv)]
                                                 (let [phrases (:headed-phrases each-kv)]
                                                   phrases)))
                                             parents-with-phrasal-head-map)

           debug (log/debug (str "lb begin pwlh @" path-to-here))
           parents-with-lexical-heads (mapcat (fn [each-kv]
                                                (let [parent (:parent each-kv)]
                                                  (let [phrases (:headed-phrases each-kv)]
                                                    phrases)))
                                              lexical-headed-phrases)

           debug (log/debug (str "lb done with pwlh @" path-to-here))
           ;; TODO: (lazy-shuffle) this
           parents-with-phrasal-heads-for-comp-phrases (mapcat (fn [each-kv]
                                                                 (let [parent (:parent each-kv)]
                                                                   (if (not (= false (get-in parent '(:comp :phrasal))))
                                                                     (let [phrases (:headed-phrases each-kv)]
                                                                       phrases))))
                                                               parents-with-phrasal-head-map)

           ;; TODO: (lazy-shuffle) this
           parents-with-lexical-heads-for-comp-phrases (mapcat (fn [each-kv]
                                                                 (let [parent (:parent each-kv)]
                                                                   (if (not (= false (get-in parent '(:comp :phrasal))))
                                                                     (let [phrases (:headed-phrases each-kv)]
                                                                       phrases))))
                                                               lexical-headed-phrases)


           one-level-trees
           (overc-with-cache parents-with-lexical-heads cache lexicon)

           rand-order (if true (rand-int 4) 1)
           rand-parent-type-order (if true (rand-int 2) 1)
           log (log/trace (str "->cp:" (str path-to-here "/[H" remove-top-values)))
           path-with-head (str path-to-here "/[H" remove-top-values "]")
           debug (log/debug (str "lb about to do with-phrasal-comps @" path-with-head ";d" depth))
           with-phrasal-comps (headed-phrase-add-comp (parents-with-phrasal-complements
                                             parents-with-phrasal-heads-for-comp-phrases
                                             parents-with-lexical-heads-for-comp-phrases
                                             rand-parent-type-order)
                                            phrases (lazy-shuffle lexicon) 0 path-with-head cache)
           debug (log/debug (str "lb done: with-phrasal-comps @" path-with-head ";d" depth))

          ]

       (log/debug (str "lb@" path-with-head ": rand-order at depth:" depth " is: " (decode-generation-ordering rand-order rand-parent-type-order) "(rand-order=" rand-order ";rand-parent-type-order=" rand-parent-type-order ")"))

       (let [result
             (cond (< depth maxdepth)
                   (cond (= rand-order 0)
                         (do (log/debug (str "lb: doing one-level first(0)."))
                             (log/debug (str "lb" path-to-here ": first one-level-tree: " (fo-ps (first one-level-trees))))

                             (try-all
                              rand-order
                              ;; 1. just a parent over 2 lexemes.
                              (do
                                (log/debug (str "lb:result@" path-with-head ";d" depth " one-level(0,0)"))
                                one-level-trees)

                              ;; 2. comp is phrase; head is either a lexeme or a phrase.
                              (do
                                (log/debug (str "lb:result@" path-with-head ";d" depth " with-phrasal-comps(0,1)"))
                                with-phrasal-comps)

                              ;; 3. head is a phrase, comp is a lexeme.
                              (do
                                (log/debug (str "lb:result@ " path-with-head ";d" depth " with-phrasal-comps(0,2)"))
                                (overc-with-cache parents-with-phrasal-head cache lexicon))

                              "hLcL"
                              (cond (= rand-parent-type-order 0)
                                    (str "hLcP " "hPcP")
                                    true
                                    (str "hPcP " "hLcP"))
                              "hPcL" path-with-head))

                         (= rand-order 1)
                         (do (log/debug (str "doing headed-phrase-add-comp first(1)."))
                             (try-all
                              rand-order
                              ;; 2. comp is phrase; head is either a lexeme or a phrase.
                              (do
                                (log/debug (str "lb:result@" path-with-head ";d" depth " with-phrasal-comps(1,0)"))
                                with-phrasal-comps)

                              ;; 1. just a parent over 2 lexemes.
                              (do
                                (log/debug (str "lb:result@" path-with-head ";d " depth " with one-level-trees(1,1)"))
                                one-level-trees)

                              ;; 3. head is a phrase, comp is a lexeme.
                              (do
                                (log/debug (str "lb:result@" path-with-head ";d " depth " with one-level-trees(1,2)"))
                                (overc-with-cache parents-with-phrasal-head cache lexicon))

                              (cond (= rand-parent-type-order 0)
                                    (str "hLcP " "hPcP")
                                    true
                                    (str "hPcP " "hLcP"))
                              "hLcL"
                              "hPcL" path-with-head))

                         (= rand-order 2)
                         (do (log/debug (str "doing headed-phrase-add-comp first(2)"))
                             (try-all
                              rand-order
                              ;; 2. comp is phrase; head is either a lexeme or a phrase.
                              (do
                                (log/debug (str "lb:result@" path-with-head ";d " depth " with phrasal-comps(2,0)"))
                                with-phrasal-comps)

                              ;; 3. head is a phrase, comp is a lexeme.
                              (do
                                (log/debug (str "lb:result@" path-with-head ";d" depth " with phrasal head and lexical comp (2,1)"))
                                (overc-with-cache parents-with-phrasal-head
                                                  cache lexicon)) ;; complement (the lexicon).
                              ;; 1. just a parent over 2 lexemes.
                              (do
                                (log/debug (str "lb:result@" path-with-head ";d" depth " with one-level-trees (2,2)"))
                                one-level-trees)

                              (cond (= rand-parent-type-order 0)
                                    (str "hLcP " "hPcP")
                                    true
                                    (str "hPcP " "hLcP"))
                              "hPcL"
                              "hLcL" path-with-head))

                         (= rand-order 3)
                         (do (log/debug (str "doing (phrasal head, lexical complement) first(3)"))
                             (try-all
                              rand-order
                              ;; 3. head is a phrase, comp is a lexeme.
                              (do
                                (log/debug (str "lb:result@" path-with-head ";d" depth " with phrasal head and lexical comp (3,0)"))
                                (overc-with-cache parents-with-phrasal-head cache lexicon))

                              ;; 2. comp is phrase; head is either a lexeme or a phrase.
                              (do
                                (log/debug (str "lb:result@" path-with-head ";d" depth " with phrasal comps (3,1)"))
                                with-phrasal-comps)

                              ;; 1. just a parent over 2 lexemes.
                              (do
                                (log/debug (str "lb:result@" path-with-head ";d" depth " with one-level trees (3,2)"))
                                one-level-trees)

                              (cond (= rand-parent-type-order 0)
                                    (str "hLcP" "hPcP")
                                    true
                                    (str "hPcP" "hLcP"))
                              "hPcL"
                              "hLcL" path-with-head))

                         true
                         (do (log/debug (str "doing (phrasal head, phrasal complement) first (4)"))
                             (try-all
                              rand-order
                              ;; 3. head is a phrase, comp is a lexeme.
                              (do
                                (log/debug (str "lb:result@" path-with-head ";d" depth " with phrasal head and lexical comp (4,0)"))
                                (overc-with-cache parents-with-phrasal-head cache lexicon))

                              ;; 1. just a parent over 2 lexemes.
                              (do
                                (log/debug (str "lb:result@" path-with-head ";d" depth " with one-level trees (4,1)"))
                                one-level-trees)

                              ;; 2. comp is phrase; head is either a lexeme or a phrase.
                              (do
                                (log/debug (str "lb:result@" path-with-head ";d" depth " with phrasal comps (4,2)"))
                                with-phrasal-comps)

                              (cond (= rand-parent-type-order 0)
                                    (str "hLcP" "hPcP")
                                    true
                                    (str "hPcP" "hLcP"))
                              "hLcL"
                              "hPcL" path-with-head)))

                   true
                   (do
                     (log/debug (str "lb: reached max depth@" path-to-here "/[H" remove-top-values))
                     (log/debug (str "lb:result@" path-with-head ";d" depth " returning one-level-trees, the first of which is: " (first one-level-trees)))))]
         (do
           (log/debug (str "lb@" path-with-head " has result type: " (type result)))
           (log/debug (str "lb@" path-with-head " realized?: " (realized? result)))
           (if (realized? result)
             (throw (Exception. (str "RESULT IS REALIZED."))))
           (log/debug (str "lb first result: " (fo-ps result) "@" path-with-head))

           result))))))

;; aliases that might be easier to use in a repl:
(defn lb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

(defn lightningb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

