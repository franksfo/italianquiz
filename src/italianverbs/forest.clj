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
          comps (lightning-bolt
                 comp-spec (get-lex parent :comp cache lexicon)
                 phrases 0
                 cache (conj path (str "C " " " (show-spec comp-spec))))]
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
      (lazy-seq
       (let [result (overh parent (get-lex parent :head cache lexicon))
             debug (if can-log-if-in-sandbox-mode
                     (if (empty? result)
                       (log/debug (str "could not add a lexeme as a head to parent: " (fo-ps parent)))
                       (log/debug (str "success adding a lexeme as a head to a parent; result is: " (fo-ps (first result))))))]
         (cons {:parent parent
                :headed-phrases result}
               (lexical-headed-phrases (rest parents) lexicon phrases depth cache path)))))))

(defn phrasal-headed-phrases [parents lexicon phrases depth cache path]
  "return a lazy seq of phrases (maps) whose heads are themselves phrases."
  (if (not (empty? parents))
    (let [parent (first parents)
          cache (if cache cache
                    (let [log (log/info (str "building cache (" (.size phrases) ")"))]
                      (build-lex-sch-cache phrases lexicon)))]
      (lazy-seq
       (cons {:parent parent
              :headed-phrases (let [bolts (lightning-bolt (get-in parent '(:head))
                                                          lexicon phrases (+ 1 depth)
                                                          cache
                                                          path)]
                                (overh parents bolts))}
             (phrasal-headed-phrases (rest parents) lexicon phrases depth cache path))))))

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

(defn log-path [path & [ depth ]]
  (let [depth (if depth depth 0)]
    (if (> (.size path) 0)
      (do
        (log/info (str "LB@[" depth "]: " (first path)))
        (log-path (rest path) (+ depth 1)))
      (log/info (str ""))))) ;; blank line after printing bolt.

;; TODO: s/head/head-spec/
(defn lightning-bolt [ & [head lexicon phrases depth cache path]]
  (let [maxdepth 2
        head (if head head :top)
        remove-top-values (remove-top-values-log head)
        depth (if depth depth 0)
        path (if path (conj path
                                          (str "H" depth " " remove-top-values))
                        ;; first element of path.
                        [ (str "H" depth " " remove-top-values) ])
        log (log-path path)

        parents-at-this-depth (parents-at-this-depth head phrases depth)
        ;; the subset of the lexicon that matches the head-spec, with a few paths removed from the head-spec
        ;; that would cause unification failure because they are specific to the desired final top-level phrase,
        ;; not the lexical entry.
        cache (if cache cache (build-lex-sch-cache phrases lexicon))]
    (cond

     (empty? parents-at-this-depth)
     (do (log/debug "lb: no parents with depth:" depth ";returning empty list.")
         nil)

     true
     (let [debug (log/debug (str "lightning-bolt first parent at this depth: "
                                 (fo-ps (first parents-at-this-depth))))
           parents-with-phrasal-head-map (if (< depth maxdepth)
                                           (phrasal-headed-phrases
                                            parents-at-this-depth
                                            lexicon
                                            phrases
                                            depth
                                            cache
                                            path))

           lexical-headed-phrases (lexical-headed-phrases
                                   parents-at-this-depth
                                   lexicon
                                   phrases
                                   depth
                                   cache
                                   path)

           parents-with-phrasal-head (mapcat (fn [each-kv]
                                               (let [parent (:parent each-kv)]
                                                 (let [phrases (:headed-phrases each-kv)]
                                                   phrases)))
                                             parents-with-phrasal-head-map)

           debug (if (empty? parents-with-phrasal-head)
                   (log/debug (str "phrases where head is itself a phrase is empty."))
                   (log/debug (str "phrases where head is itself a phrase is not empty; first is: " (fo-ps (first parents-with-phrasal-head)))))

           parents-with-lexical-heads (mapcat (fn [each-kv]
                                                (let [parent (:parent each-kv)]
                                                  (let [phrases (:headed-phrases each-kv)]
                                                    phrases)))
                                              lexical-headed-phrases)
           debug (if (empty? parents-with-lexical-heads)
                   (log/debug (str "phrases where head is a lexeme is empty."))
                   (log/debug (str "phrases where head is a lexeme is not empty; first is: " (fo-ps (first parents-with-lexical-heads)))))

           ;; TODO: (lazy-shuffle) this.
           ;; TODO: cache this.
           parents-with-phrasal-heads-for-comp-phrases (mapcat (fn [each-kv]
                                                                 (let [parent (:parent each-kv)]
                                                                   (if (not (= false (get-in parent '(:comp :phrasal))))
                                                                     (let [phrases (:headed-phrases each-kv)]
                                                                       phrases))))
                                                               parents-with-phrasal-head-map)

           debug (if (empty? parents-with-phrasal-heads-for-comp-phrases)
                   (log/debug (str "parents-with-phrasal-heads-for-comp-phrases is empty."))
                   (log/debug (str "parents-with-phrasal-heads-for-comp-phrases is not empty; first is: " (fo-ps (first parents-with-phrasal-heads-for-comp-phrases)))))


           parents-with-lexical-heads-for-comp-phrases (mapcat (fn [each-kv]
                                                                 (let [parent (:parent each-kv)]
                                                                   (if (not (= false (get-in parent '(:comp :phrasal))))
                                                                     (let [phrases (:headed-phrases each-kv)]
                                                                       phrases))))
                                                               lexical-headed-phrases)

           debug (if (empty? parents-with-lexical-heads-for-comp-phrases)
                   (if (not (empty? lexical-headed-phrases))
                     (log/debug (str "parents-with-lexical-heads-for-comp-phrases is empty."))
                     (log/trace (str "parents-with-lexical-heads-for-comp-phrases is empty, because it's a subset of an empty set (lexical-headed-phrases)")))
                   (log/debug (str "parents-with-lexical-heads-for-comp-phrases is not empty; first is: "
                                   (fo-ps (first parents-with-lexical-heads-for-comp-phrases)))))

           one-level-trees
           (if (not (empty? parents-with-lexical-heads))
             (overc-with-cache parents-with-lexical-heads cache lexicon))

           rand-order (if true (rand-int 3) 1)
           rand-parent-type-order (if true (rand-int 2) 1)

           with-phrasal-comps (headed-phrase-add-comp (parents-with-phrasal-complements
                                                       parents-with-phrasal-heads-for-comp-phrases
                                                       parents-with-lexical-heads-for-comp-phrases
                                                       rand-parent-type-order)
                                                      phrases (lazy-shuffle lexicon) 0 cache path)

           debug (if (empty? with-phrasal-comps)
                   (log/debug (str "with-phrasal-comps is empty."))
                   (log/debug (str "with-phrasal-comps is not empty; first is: " (fo-ps (first with-phrasal-comps)))))

           adding-a-lexeme-complement-to-a-parent-with-a-phrasal-head (overc-with-cache parents-with-phrasal-head cache lexicon)

           debug (if (empty? adding-a-lexeme-complement-to-a-parent-with-a-phrasal-head)
                   (if (not (empty? parents-with-phrasal-head))
                     (log/debug (str "no way to attach a lexeme as a complement to any parent with a phrasal head."))
                     (log/trace (str "no way to attach a lexeme as a complement to any parent with a phrasal head, but there "
                                     "were no parents with phrasal heads anyway, and you can't attach a lexeme to nothing.")))
                   (log/debug (str "success attaching a lexeme as a complement to a parent with a phrasal head; first is: "
                                   (fo-ps (first adding-a-lexeme-complement-to-a-parent-with-a-phrasal-head)))))



          ]

       (log/debug (str "rand-order at depth:" depth " is: " (decode-generation-ordering rand-order rand-parent-type-order)
                       "(rand-order=" rand-order ";rand-parent-type-order=" rand-parent-type-order ")"))

       (if (and (= rand-order 2)
                (empty? with-phrasal-comps)
                (not (empty? parents-with-phrasal-head))
                (= (first (fo (get-in (first one-level-trees) '(:comp)))) "Alto (Tall).")
                (not (empty? (overc-with-cache parents-with-phrasal-head cache lexicon)))
                (not (empty? one-level-trees)))
         (do
           (log/error (str "HIT A POSSIBLE PROBLEM; rand-order=" rand-order))
           (log/error (str "THE COMPLEMENT WAS: " (fo (get-in (first one-level-trees) '(:comp)))))
           (log/error (str "The first add-LC-to-LH is: " (fo (first one-level-trees))))
           (log/error (str "The first add-LC-to-LH synsem is: " (show-spec (get-in (first one-level-trees) '(:synsem)))))
           (log/error (str "The head spec is: " (show-spec head)))
           (if true (throw (Exception. (str "RATHOLISH.."))))))


       (cond (= rand-order 0)
             (lazy-cat
              with-phrasal-comps
              (overc-with-cache parents-with-phrasal-head cache lexicon)
              one-level-trees)

             (= rand-order 1)
             (lazy-cat
              (overc-with-cache parents-with-phrasal-head cache lexicon)
              with-phrasal-comps
              one-level-trees)

             (= rand-order 2)
             (lazy-cat
              one-level-trees
              (overc-with-cache parents-with-phrasal-head cache lexicon)


              with-phrasal-comps))))))




; rathole:
;       (lazy-cat
;        with-phrasal-comps
;        one-level-trees
;        (overc-with-cache parents-with-phrasal-head cache lexicon))))))

;; aliases that might be easier to use in a repl:
(defn lb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

(defn lightningb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

