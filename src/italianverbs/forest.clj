(ns italianverbs.forest
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [clojure.core :as core]
   [clojure.set :refer :all]
   [clojure.stacktrace :refer :all]
   [clojure.string :as string]
   [clojure.tools.logging :as log]

   [italianverbs.config :as config]
   [italianverbs.html :as html]

   [italianverbs.generate :refer :all]
   [italianverbs.grammar :refer :all]
   [italianverbs.lexicon :refer :all]
   [italianverbs.lexiconfn :refer (sem-impl)]
   [italianverbs.morphology :refer :all]
   [italianverbs.over :refer :all]

   [italianverbs.lev :as lev]
   [italianverbs.search :as search]
   [italianverbs.ug :refer :all]
   [italianverbs.unify :refer :all]
   [italianverbs.unify :as unify]))

(defn choose-at-random [set & [distrib]]
  "choose one from amongst set using a probability distribution."
  (if (nil? distrib)
    (first (shuffle set))
    (first (shuffle set))))

;(def parents (list hh10 hh21))
;(def parents (set (list 'hh10 'hh21)))

;(def parents (set (list 'cc10)))
;(def lex (set (list 'io 'tu 'dormire)))

(def parents (set (list (merge (unifyc cc10
                                       {:synsem {:infl :present
                                                 :cat :verb
                                                 :sem {:tense :present}}})
                               {:comment "parent1/cc10"})

                        (merge (unifyc hh21
                                       {:synsem {:infl :present
                                                 :cat :verb
                                                 :sem {:tense :present}}})
                               {:comment "parent2/hh21"})

                        (merge (unifyc cc10
                                       {:synsem {:cat :noun}})
                               {:comment "parent3/cc10"}))))

(def lex (union (it "il") (it "cane") (it "i")
                (it "io") (it "pane") (it "tu")
; (it "lui") (it "lei")
 (it "dormire") (it "sognare") (it "mangiare")))
;(def lex (union (it "io") (it "dormire")))

(defn in? [member of-set]
  (not (empty? (intersection (set (list member)) of-set))))

(defn one-tree [set]
  (let [choice (choose-at-random set)]
    (cond (in? choice parents)
          {:sch choice
           :h (one-tree set)
           :c (one-tree set)}
          true
          choice)))

(defn choose-head [parent set]
  {:h (choose-at-random set)})

(defn all-trees [tree-with-head set]
  (if (not (empty? set))
    (lazy-seq
     (cons
      (unifyc tree-with-head
              {:c (first set)})
      (all-trees tree-with-head (rest set))))))

(defn all-heads-and-parents [parent set]
  "generate all possible head-parent combinations."
  (lazy-seq
   (cons
    (unifyc parent
            {:h (first set)})
    (all-heads-and-parents parent set))))

(def upl (union parents lex))

(defn forest [set]
  "generate a lazy sequence of trees"
  (lazy-seq
   (cons
    (one-tree set)
    (forest set))))

(def pl (union parents lex))

(defn choose-at-random-with-depth [parents lex depth]
  (let [rand (rand-int 10)]
    (cond (= depth 0) ;; depth 0: branch with 80% probability
          (cond (> rand 0)
                (first (shuffle parents))
                true
                (first (shuffle lex)))

          (= depth 1) ;; depth 1: branch with 40% probabilty
          (cond (> rand 5)
                (first (shuffle parents))
                true
                (first (shuffle lex)))

          (= depth 2) ;; depth 2: branch with 20% probability
          (cond (> rand 7)
                (first (shuffle parents))
                true
                (first (shuffle lex)))

          true ;; greater depth: branch with 10% probability
          (cond (> rand 8)
                (first (shuffle parents))
                true
                (first (shuffle lex))))))


(defn next-gen-with-depth [parents lex depth]
  (let [rand (rand-int 10)]
    (cond (= depth 0) ;; depth 0: branch with 80% probability
          (cond (> rand 1)
                (lazy-cat parents lex)
                true
                (lazy-cat lex parents))

          (= depth 2) ;; depth 1: branch with 40% probabilty
          (cond (> rand 5)
                (lazy-cat parents lex)
                true
                (lazy-cat lex parents))

          (= depth 2) ;; depth 2: branch with 20% probability
          (cond (> rand 7)
                (lazy-cat parents lex)
                true
                (lazy-cat lex parents))

          true ;; greater depth: branch with 10% probability
          (cond (> rand 8)
                (lazy-cat parents lex)
                true
                (lazy-cat lex parents)))))

(defn h1d1 [parents lex & [depth head-spec]]
  "head-first,depth-first generation"
  (let [depth (if depth depth 0)
        head-spec (if head-spec head-spec :top)
        choice (choose-at-random-with-depth parents lex depth)]
    (log/debug (str "h1d1 at depth: " depth))
    (cond (in? choice parents)
          ;; this is a sub-tree: generate its head.
          (let [chosen-phrase choice

                debug (log/debug (str "h1d1: phrase: " (get-in chosen-phrase '(:comment))))

                head (h1d1 parents lex (+ depth 1) head-spec)

                debug (log/debug (str "h1d1: head: " (fo head)))

                head (if (fail? head) :fail head)

                phrase-with-head
                (if (and (not (fail? head))
                         (not (fail? chosen-phrase)))
                  (unifyc chosen-phrase
                          {:head head})
                  :fail)

                debug (log/debug (str "h1d1: phrase-with-head: " (fo phrase-with-head)))

                comp (if (fail? phrase-with-head)
                       :fail
                       (h1d1 parents lex 0 (get-in phrase-with-head '(:comp))))]
            (if (fail? comp)
              :fail
              (unifyc phrase-with-head
                      {:comp comp})))
          true
          ;; not a subtree: done.
          (unifyc choice head-spec))))

(declare h1d1s)

;; trying to figure out why h1d1s-1 doesn't work..
(defn h1d1s-2 [parents lex depth head-spec choices]
  (if (not (empty? choices))
    (lazy-seq
     (cons
      (let [choice (first choices)]
        choice)
      (h1d1s-2 parents lex depth head-spec (rest choices))))))

(defn partly [parents lex depth head-spec choice]
  (cond (in? choice parents)
        ;; this is a sub-tree: generate its head.
        (let [chosen-phrase choice

              debug (log/debug (str "h1d1: phrase: " (get-in chosen-phrase '(:comment))))

              head (h1d1 parents lex (+ depth 1) head-spec)

              debug (log/debug (str "h1d1: head: " (fo head)))

              head (if (fail? head) :fail head)

              phrase-with-head
              (if (and (not (fail? head))
                       (not (fail? chosen-phrase)))
                (unifyc chosen-phrase
                        {:head head})
                :fail)

              debug (log/debug (str "h1d1: phrase-with-head: " (fo phrase-with-head)))

              comp (if (fail? phrase-with-head)
                     :fail
                     (h1d1s parents lex 0 (get-in phrase-with-head '(:comp))))]
          (if (fail? comp)
            :fail

            ;; note: comp is a lazy seq; we coerce it and fully expand it to a set.
            ;; ideally, unify should deal with lazy sequences, then we would not need to
            ;; expand to a set here.
            (unifyc phrase-with-head
                    {:comp (set comp)})))

        ;; at implementation of (unify/unify)
        true
        ;; not a subtree: done.
        (unifyc choice head-spec)))

(defn h1d1s-1 [parents lex depth head-spec choice]
  (if (not (empty? choice))
    (lazy-seq
     (cons
      (let [choice (first choice)]
        (cond (in? choice parents)
              ;; this is a sub-tree: generate its head.
              (let [chosen-phrase choice

                    debug (log/debug (str "h1d1: phrase: " (get-in chosen-phrase '(:comment))))

                    head (h1d1 parents lex (+ depth 1) head-spec)

                    debug (log/debug (str "h1d1: head: " (fo head)))

                    head (if (fail? head) :fail head)

                    phrase-with-head
                    (if (and (not (fail? head))
                             (not (fail? chosen-phrase)))
                      (unifyc chosen-phrase
                              {:head head})
                      :fail)

                    debug (log/debug (str "h1d1: phrase-with-head: " (fo phrase-with-head)))

                    comp (if (fail? phrase-with-head)
                           :fail
                           (h1d1s parents lex 0 (get-in phrase-with-head '(:comp))))]
                (if (fail? comp)
                  :fail

                  ;; note: comp is a lazy seq; we coerce it and fully expand it to a set.
                  ;; ideally, unify should deal with lazy sequences, then we would not need to
                  ;; expand to a set here.
                  (unifyc phrase-with-head
                          {:comp (set comp)})))

              ;; at implementation of (unify/unify)
              true
              ;; not a subtree: done.
              (unifyc choice head-spec)))
      (h1d1s-1 parents lex depth head-spec (rest choice))))))

(defn h1d1s [parents lex & [depth head-spec]]
  "head-first,depth-first generation, but return a lazy seq rather than just one try"
  (let [depth (if depth depth 0)
        head-spec (if head-spec head-spec :top)
        choices (next-gen-with-depth parents lex depth)]
    (log/info (str "h1d1 at depth: " depth))
    (h1d1s-1 parents lex depth head-spec choices)))

(defn do-a-bunch []
  (take 5 (forest (union parents lex))))

;(h1d1 parents lex 0)
;(fo (remove fail? (take 100 (repeatedly #(h1d1 parents lex 0)))))

;(fo (remove (fn [x] (= :notfound (get-in x '(:head) :notfound)))
;     (take 100 (repeatedly #(h1d1 parents lex 0))))))))

(defn keep-trying []
  (let [result (h1d1 parents lex 0)]
    (if (fail? result)
      (keep-trying)
      result)))

(defn keep-trying-phrase []
  (let [result (h1d1 parents lex 0
                     {:synsem {:cat :verb}})]
    (if (or (fail? result)
            (= :notfound (get-in result '(:head) :notfound)))
      (keep-trying-phrase)
      result)))

;; Problem: why is it that i can generate "i cani sognano" with the following:
;;
;;(fo (over parents (over parents lex lex) lex))
;;
;; but not with:
;;(fo (take 100 (repeatedly #(keep-trying-phrase))))
;;

(defn il-libro []
  (over parents lex lex))

;; TODO: move to unify
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

(declare lightningb)
(declare lightning-bolt)

(defn comp-phrases [phrases-with-heads all-phrases lexicon]
  (if (not (empty? phrases-with-heads))
    (let [phrase-with-head (first phrases-with-heads)]
      (log/debug (str "comp-phrases: looking for phrases with phrase-with-head's comp: " (get-in phrase-with-head '(:comp))))
      (log/info (str "comp-phrases: looking for complements with phrase-with-head: " (fo-ps phrase-with-head)))
      (lazy-cat
       (overc phrase-with-head
              (lightning-bolt
               (remove-path-from
                (get-in phrase-with-head '(:comp))
                '((:synsem :subcat)
                  (:english :initial)
                  (:italian :initial)
                  ))
               all-phrases
               0
               lexicon))
       (comp-phrases (rest phrases-with-heads) all-phrases lexicon)))))

(defn lb [ & [head phrases depth lexicon]]
  (let [depth (if depth depth 0)
        head (if head head :top)
        lexicon (if lexicon lexicon lex)
        phrases (if phrases phrases parents)]
    (lightning-bolt head phrases depth lexicon)))

(defn lightningb [ & [head phrases depth lexicon] ]
  (let [depth (if depth depth 0)
        head (if head head :top)
        lexicon (if lexicon lexicon lex)
        phrases (if phrases phrases parents)]
    (lightning-bolt head phrases depth lexicon)))

(defn map-lexicon [head lexicon]
  "TODO: determine if this is done lazily or not: it should. If not, will have to do laziness with (lazy-seq (cons..) over the lexicon."
  (filter (fn [lexeme]
            (not (fail? lexeme)))
          (map (fn [lexeme]
                 (unifyc
                  head
                  (unifyc
                   {:synsem {:sem (sem-impl (get-in head '(:synsem :sem)))}}
                   lexeme)))
               lexicon)))

(defn lightning-bolt [ & [head phrases depth lexicon] ]
  (let [depth (if depth depth 0)
        head (if head head :top)
        lexicon (if lexicon lexicon lex)
        phrases (if phrases phrases parents)]
    (let [debug (log/debug (str "lightning-bolt head (fo): " (fo head)))
          debug (log/debug (str "lightning-bolt head: " head))
          debug (log/info (str "lightning-bolt depth: " depth))
          debug (log/debug (str "lightning-bolt lexicon: " (fo lexicon)))
          recursive-head
          (cond (= depth 0)
                (lightning-bolt head phrases (+ 1 depth) lexicon)

                (< depth 2)
                (lightning-bolt head phrases (+ 1 depth) lexicon)

                true  ;; bounded depth: if depth is greater than any matched above, don't branch any more.
                nil)]

      (let [debug (log/debug (str "lightning-bolt: recursive head type: " (type recursive-head)))
            debug (log/debug (str "-- /depth: " depth))
            debug (log/debug (str "lightning-bolt: end"))
            debug (log/debug (str "head's sem-impl: " (sem-impl (get-in head '(:synsem :sem)))))
            with-lexical-heads (overh phrases (map-lexicon head lexicon))
            debug (log/debug (str "first phrase with lexical heads: " (first with-lexical-heads)))]
        (lazy-cat

         ;; 1. both head and comp are lexemes.
         (overc with-lexical-heads lexicon)

         ;; 2. head is a lexeme, comp is a phrase.
         (comp-phrases with-lexical-heads phrases lexicon)

         ;; 3. head is a phrase, comp is a lexeme.
         (overhc phrases recursive-head lexicon)

         ;; 4. head is a phrase, comp is a phrase.
         (comp-phrases (overh phrases recursive-head) phrases lexicon))))))




