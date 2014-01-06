(ns italianverbs.forest
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [clojure.set :refer :all]
   [clojure.tools.logging :as log]
   ;; italianverbs.config is not used yet but hopefully will be in the future.
   [italianverbs.config :as config]
   [italianverbs.lexiconfn :refer (sem-impl)]
   [italianverbs.morphology :refer (fo-ps)]
   [italianverbs.over :refer (overc overh)]
   [italianverbs.unify :refer :all]))

;; TODO: determine if this is done lazily or not: it should; if not,
;; will have to do laziness with (lazy-seq (cons..) over the lexicon.
(defn get-lexicon-of-head-spec [head-spec lexicon]
  (let [head-spec (dissoc-paths head-spec '((:synsem :subcat)
                                            (:english :initial)
                                            (:italian :initial)))]
    (filter (fn [lexeme]
              (and (not (= :det (get-in lexeme '(:synsem :cat)))) ;; dets don't have preds, making them likely to match head-spec without this.
                   (not (= true (get-in lexeme '(:synsem :aux)))) ;; auxs also don't have preds, same tendency applies.
                   (not (fail?
                         (unifyc
                          head-spec
                          (unify
                           {:synsem {:sem (sem-impl (get-in head-spec '(:synsem :sem)))}}
                           (copy lexeme)))))))
            lexicon)))

(declare lightning-bolt)

(defn comp-phrases [parents all-phrases lexicon & [iter path-to-here]]
  (if (not (empty? parents))
    (let [iter (if (nil? iter) 0 iter)
          path-to-here (if path-to-here path-to-here ":none")
          parent (first parents)
          comp-spec
          (dissoc-paths
           (get-in parent '(:comp))
           '((:synsem :subcat)
             (:english :initial)
             (:italian :initial)))
          debug (log/debug (str "comp-phrases: iter:" iter ": parent: " (fo-ps parent)))
          debug (log/debug (str "comp-phrases: iter:" iter ": path-to-here: " path-to-here))
          no-top-values (remove-top-values (dissoc-paths comp-spec '((:english :initial)
                                                                     (:italian :initial))))
          debug (log/debug (str "comp-spec   : iter:" iter ":" no-top-values))]
      (cond

       ;; Lexemes with certain grammatical categories (for now, only :det) cannot be heads of
       ;; a phrase, but only lexemes that are complements of a phrase, so save time by not trying
       ;; to recursively generate phrases that are headed with such lexemes.
       (or (= :det (get-in comp-spec '(:synsem :cat)))
           (= :adjective (get-in comp-spec '(:synsem :cat))))
       (do
         (log/error (str "comp-phrases: cannot be head of a phrase: cat="
                         (get-in comp-spec '(:synsem :cat))))
         (if true ;; TODO: add top-level 'throw-exception-or-not' switch.
           (throw (Exception. (str (str "Rat-hole detected: comp-phrases: cannot be head of a phrase: cat="
                                        (get-in comp-spec '(:synsem :cat))))))
           (comp-phrases (rest parents) all-phrases lexicon (+ 1 iter) path-to-here)))

       true
       (lazy-cat
        (overc parent
               (lightning-bolt
                comp-spec
                lexicon
                all-phrases
                0
                (str path-to-here "/[C " no-top-values "]")
                ))
        (comp-phrases (rest parents) all-phrases lexicon (+ 1 iter) path-to-here))))))

(defn parents-with-lexical-head-map [parents lexicon phrases depth]
  (if (not (empty? parents))
    (let [parent (first parents)]
      (lazy-seq
       (cons {:parent parent
              :headed-phrases (overh parent lexicon)}
             (parents-with-lexical-head-map (rest parents) lexicon phrases depth))))))

(defn get-parents-with-phrasal-head-map [parents lexicon phrases depth path-to-here lexicon-of-heads]
  (if (not (empty? parents))
    (let [parent (first parents)]
      (lazy-seq
       (cons {:parent parent
              :headed-phrases (let [bolts (lightning-bolt (get-in parent '(:head))
                                                          lexicon phrases (+ 1 depth)
                                                          (str path-to-here "/[H "
                                                               (remove-top-values
                                                                (dissoc-paths (get-in parent '(:head))
                                                                              '((:english :initial)
                                                                                (:italian :initial)))) "]")
                                                          lexicon-of-heads)]
                                (overh parents bolts))}
             (get-parents-with-phrasal-head-map (rest parents) lexicon phrases depth path-to-here lexicon-of-heads))))))

;; TODO: move this to inside lightning-bolt.
(defn decode-gen-ordering2 [rand2]
  (cond (= rand2 0)
        "hLcP + hPcP"
        true
        "hPcP + hLcP"))

;; TODO: make option to just call (lazy-cat seq1 seq2 seq3) for efficiency:
;; this is simply a diagnostic tool.
(defn try-all-debug [order seq1 seq2 seq3 seq1-label seq2-label seq3-label]
  (if (not (empty? seq1))
    (let [first-of-seq1 (first seq1)]
      (do
        (log/debug (str "seq1 ("seq1-label") has a candidate:" (fo-ps first-of-seq1)))
        (lazy-seq
         (cons
          first-of-seq1
          (try-all-debug order (rest seq1) seq2 seq3 seq1-label seq2-label seq3-label)))))
    (if seq2
      (try-all-debug order
       seq2 seq3 nil seq2-label seq3-label nil)
      (if seq3
        (try-all-debug
         order seq3 nil nil seq3-label nil nil)))))

(defn try-all [order seq1 seq2 seq3 seq1-label seq2-label seq3-label]
  (if false
    (try-all-debug order seq1 seq2 seq3 seq1-label seq2-label seq3-label)
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
  (filter (fn [each]
            (not (fail? each)))
          (map (fn [phrase]
                 ;; TODO: possibly: remove-paths such as (subcat) from head: would make it easier to call with lexemes:
                 ;; e.g. "generate a sentence whose head is the word 'mangiare'" (i.e. user passes the lexical entry as
                 ;; head param of (lightning-bolt)".
                 (unifyc phrase head))
               (cond (= depth 0) ;; if depth is 0 (top-level), only allow phrases with empty subcat.
                     (filter (fn [phrase]
                               (empty? (get-in phrase '(:synsem :subcat))))
                             phrases)
                     (= depth 1)
                     (filter (fn [phrase]
                               (and (not (empty? (get-in phrase '(:synsem :subcat))))
                                    (empty? (get-in phrase '(:synsem :subcat :2)))))
                             phrases)
                     true
                     '()))))

(defn parents-with-phrasal-complements [parents-with-lexical-heads parents-with-phrasal-heads
                                        rand-parent-type-order head-lexemes]
;;  (log/debug (str "parents-with-phrasal-complements with head-lexemes:" (.size head-lexemes))) ;; REALIZES.
  (let [parents-with-lexical-heads (filter (fn [parent]
                                             (if (= false (get-in parent '(:comp :phrasal)))
                                               (do
                                                 (log/debug (str "this parent: " (fo-ps parent)
                                                                 " cannot have a phrasal comp; omitting."))
                                                 false)
                                               true))
                                           parents-with-lexical-heads)
        parents-with-phrasal-heads (filter (fn [parent]
                                             (if (= false (get-in parent '(:comp :phrasal)))
                                               (do
                                                 (log/debug (str "this parent: " (fo-ps parent)
                                                                 " cannot have a phrasal comp; omitting."))
                                                 false)
                                               true))
                                           parents-with-phrasal-heads)]
    (cond (= rand-parent-type-order 0)
          (lazy-cat parents-with-lexical-heads parents-with-phrasal-heads)
          true
          (lazy-cat parents-with-phrasal-heads parents-with-lexical-heads))))


(defn lightning-bolt [ & [head lexicon phrases depth path-to-here lexicon-of-heads]]
  (log/info (str "lb depth: " depth "; cat: " (get-in head '(:synsem :cat))))
  (let [maxdepth 2
        depth (if depth depth 0)
        parents-at-this-depth (parents-at-this-depth head phrases depth)
        head (if head head :top)
        path-to-here (if path-to-here path-to-here head)

        ;; the subset of the lexicon that matches the head-spec, with a few paths removed from the head-spec
        ;; that would cause unification failure because they are specific to the desired final top-level phrase,
        ;; not the lexical entry.
        lexicon-of-heads (if lexicon-of-heads lexicon-of-heads (get-lexicon-of-head-spec head lexicon))
        ]

    (cond

     (empty? parents-at-this-depth)
     (do
       (log/debug (str "lb: returning nil since parents-at-this-depth is empty."))
       nil)

     true
     (let [debug (log/debug (str "lb start: depth:" depth "; head: " (remove-top-values head)))
           debug (log/debug (str "parents-at-this-depth: " (fo-ps parents-at-this-depth)))
           debug (log/debug (str "size of categories of comps of parents:"
                                 (.size (map (fn [parent]
                                               (get-in parent '(:comp :synsem :cat)))
                                             parents-at-this-depth))))

           parents-with-phrasal-head-map (if (< depth maxdepth)
                                           (get-parents-with-phrasal-head-map
                                            parents-at-this-depth
                                            lexicon
                                            phrases
                                            depth
                                            path-to-here
                                            lexicon-of-heads))

           parents-with-lexical-head-map (parents-with-lexical-head-map
                                          parents-at-this-depth
                                          lexicon-of-heads
                                          phrases
                                          depth)

           parents-with-phrasal-head (mapcat (fn [each-kv]
                                               (let [parent (:parent each-kv)]
                                                 (let [phrases (:headed-phrases each-kv)]
                                                   phrases)))
                                             parents-with-phrasal-head-map)

           parents-with-lexical-heads (mapcat (fn [each-kv]
                                                (let [parent (:parent each-kv)]
                                                  (let [phrases (:headed-phrases each-kv)]
                                                    phrases)))
                                              parents-with-lexical-head-map)


           parents-with-phrasal-head-for-comp-phrases (mapcat (fn [each-kv]
                                                                (let [parent (:parent each-kv)]
                                                                  (if (not (= false (get-in parent '(:comp :phrasal))))
                                                                    (let [phrases (:headed-phrases each-kv)]
                                                                      phrases))))
                                                              parents-with-phrasal-head-map)

           parents-with-lexical-heads-for-comp-phrases (mapcat (fn [each-kv]
                                                                 (let [parent (:parent each-kv)]
                                                                   (if (not (= false (get-in parent '(:comp :phrasal))))
                                                                     (let [phrases (:headed-phrases each-kv)]
                                                                       phrases))))
                                                               parents-with-lexical-head-map)


           one-level-trees
           (overc parents-with-lexical-heads (lazy-shuffle lexicon))

           rand-order (if true (rand-int 4) 1)
           rand-parent-type-order (if true (rand-int 2) 1)

           comp-phrases (comp-phrases (parents-with-phrasal-complements
                                       parents-with-phrasal-head-for-comp-phrases
                                       parents-with-lexical-heads-for-comp-phrases
                                       rand-parent-type-order lexicon-of-heads)
                                      phrases (lazy-shuffle lexicon) 0 path-to-here)

          ]

       (log/debug (str "lightning-bolt rand-order at depth:" depth " is: " (decode-generation-ordering rand-order rand-parent-type-order) "(rand-order=" rand-order ";rand-parent-type-order=" rand-parent-type-order ")"))

       (cond (< depth maxdepth)
             (cond (= rand-order 0)
                   (try-all
                    rand-order
                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    comp-phrases

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc parents-with-phrasal-head
                           (lazy-shuffle lexicon)) ;; complement (the lexicon).

                    "hLcL"
                    (cond (= rand-parent-type-order 0)
                          (str "hLcP " "hPcP")
                          true
                          (str "hPcP " "hLcP"))
                    "hPcL")

                   (= rand-order 1)
                   (try-all
                    rand-order
                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    comp-phrases

                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc parents-with-phrasal-head
                           (lazy-shuffle lexicon)) ;; complement (the lexicon).

                    (cond (= rand-parent-type-order 0)
                          (str "hLcP " "hPcP")
                          true
                          (str "hPcP " "hLcP"))
                    "hLcL"
                    "hPcL")

                   (= rand-order 2)
                   (try-all
                    rand-order
                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    comp-phrases

                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc parents-with-phrasal-head
                           (lazy-shuffle lexicon)) ;; complement (the lexicon).
                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    (cond (= rand-parent-type-order 0)
                          (str "hLcP " "hPcP")
                          true
                          (str "hPcP " "hLcP"))
                    "hPcL"
                    "hLcL")

                   (= rand-order 3)
                   (try-all
                    rand-order
                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc parents-with-phrasal-head
                           (lazy-shuffle lexicon)) ;; complement (the lexicon).


                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    comp-phrases

                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    (cond (= rand-parent-type-order 0)
                          (str "hLcP" "hPcP")
                          true
                          (str "hPcP" "hLcP"))
                    "hPcL"
                    "hLcL")

                   true
                   (try-all
                    rand-order
                    ;; 3. head is a phrase, comp is a lexeme.
                    (overc parents-with-phrasal-head
                           (lazy-shuffle lexicon)) ;; complement (the lexicon).

                    ;; 1. just a parent over 2 lexemes.
                    one-level-trees

                    ;; 2. comp is phrase; head is either a lexeme or a phrase.
                    comp-phrases

                    (cond (= rand-parent-type-order 0)
                          (str "hLcP" "hPcP")
                          true
                          (str "hPcP" "hLcP"))
                    "hLcL"
                    "hPcL"))

             true
             one-level-trees)))))

;; aliases that might be easier to use in a repl:
(defn lb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

(defn lightningb [ & [head lexicon phrases depth]]
  (let [depth (if depth depth 0)
        head (if head head :top)]
    (lightning-bolt head lexicon phrases depth)))

