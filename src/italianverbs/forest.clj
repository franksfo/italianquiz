(ns italianverbs.forest
  (:refer-clojure :exclude [get-in deref merge resolve find future parents])
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

(defn lazy-mapcat-shuffle [fn args & [depth name]]
  (do
    (log/trace (str "lms@" depth ":" name))
    (log/trace (str "lms@" depth ":" name " : type of args: " (type args)))
    (log/trace (str "lms@" depth ":" name " : type of first arg: " (type (first args))))
    (let [result
          (lazy-mapcat fn (shuffle args))]
      (log/trace (str "lms@" depth ":" name " : returning type: " (if result (type result))))
      result)))

;; (take 10 (repeatedly #(fo-ps (take 1 (forest/gen1 (shuffle grammar) (shuffle lexicon) {:synsem {:cat :verb :subcat '()}})))))

;; TODO: add usage of rule-to-lexicon cache (rather than using lexicon directly)
(defn gen1 [grammar lexicon spec & [ depth ]]
  (log/trace (str "gen1@" depth))
  (let [depth (if depth depth 0)
        parents (lazy-shuffle (filter #(not (fail? (unifyc spec %)))
                                      (map (fn [rule]
                                             (unifyc spec rule))
                                           grammar)))]
    (if (and (not (nil? parents))
             (not (empty? parents)))
      (let [;; simple-case: hlcX
            simple
            (lazy-mapcat-shuffle
             (fn [parent]
               (do
                 (log/debug (str "gen1@" depth ": overh(lex) with parent: " (fo-ps parent)))
                 (overh parent (lazy-shuffle lexicon))))
             parents
             depth
             "overh(lex)")

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
                     (overh parent (lazy-shuffle lexicon))))
                 parents
                 depth
                 "overh(lex)"))
           
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
                   "overh(gen1)")
                  (do
                    (log/debug (str "gen1@" depth ": terminating."))
                    nil))))
             depth
             "overh(lex;gen1)")]

    (log/trace (str "type of parents-with-head t=" (type parents-with-head)))
    parents-with-head))))

(defn path-to-map [path val]
  (let [feat (first path)]
    (if feat
      {feat (path-to-map (rest path) val)}
      val)))

(declare add-complement)
(declare add-all-complements-to-bolts)
(declare add-complements-to-bolts)

(defn gen2 [grammar lexicon spec]
  (-> (gen1 grammar
            lexicon
            spec)
      (add-all-complements-to-bolts grammar lexicon)))

(defn add-all-complements-to-bolts [bolts grammar lexicon]
  (-> bolts
      (add-complements-to-bolts [:comp]       :top grammar lexicon)
      (add-complements-to-bolts [:head :comp] :top grammar lexicon)
      (add-complements-to-bolts [:head :head :comp] :top grammar lexicon)
      (add-complements-to-bolts [:head :head :head :comp] :top grammar lexicon)))

(defn add-complements-to-bolts [bolts path spec grammar lexicon]
  (if (not (empty? bolts))
    (let [bolt (first bolts)]
      (lazy-cat
       (add-complement bolt path spec grammar lexicon)
       (add-complements-to-bolts (rest bolts) path spec grammar lexicon)))))

;; (forest/add-complement lb [:comp] :top lexicon)
;; (fo-ps (forest/add-complement (first (take 1 (forest/gen1 (shuffle grammar) (shuffle lexicon) {:synsem {:cat :verb :subcat '()}}))) [:comp] :top lexicon))
(defn add-complement [bolt path spec grammar lexicon]
  (let [spec (unifyc spec (get-in bolt path :no-path))]
    (if (not (= spec :no-path))
      (do
        (log/debug (str "add-complement to: " (fo-ps bolt) " @path: " path))
        (log/trace (str "add-complement to: " (fo-ps bolt) " with spec " (show-spec spec) " at path: " path))
        (filter (fn [result]
                  (not (fail? result)))
                (map (fn [lexeme]
                       (log/trace (str "add-complement: " (fo-ps bolt) " with lexeme: " (fo lexeme)))
                       (let [result
                             (unifyc bolt
                                     (path-to-map path
                                                  lexeme))
                             is-fail? (fail? result)]
                         (log/trace (str "add-complement: " (fo-ps bolt) " with lexeme: " (fo lexeme) " => " (if (not is-fail?)
                                                                                                               (fo-ps result)
                                                                                                               ":fail")))
                         (if is-fail? :fail result)))
                     (if (= (rand-int 2) 0)
                       (lazy-cat (lazy-shuffle lexicon)
                                 (gen2 grammar lexicon spec))
                       (lazy-cat (gen2 grammar lexicon spec)
                                 (lazy-shuffle lexicon))))))

      ;; path doesn't exist in bolt: simply return the bolt unmodified.
      (do
        (log/warn "no path: " path " in bolt: " (fo-ps bolt))
        (list bolt)))))

(defn hlcl [cache grammar spec lexicon spec]
  (gen2 grammar lexicon (unifyc {:head {:phrasal false}
                                 :comp {:phrasal false}})))

(defn hlcp [cache grammar spec lexicon spec]
  (gen2 grammar lexicon (unifyc {:head {:phrasal false}
                                 :comp {:phrasal true}})))

(defn hpcl [cache grammar spec lexicon spec]
  (gen2 grammar lexicon (unifyc {:head {:phrasal true}
                                 :comp {:phrasal false}})))

(defn hpcp [cache grammar spec lexicon spec]
  (gen2 grammar lexicon (unifyc {:head {:phrasal true}
                                 :comp {:phrasal true}})))

(defn lightning-bolt [grammar cache lexicon spec]
  (gen2 grammar lexicon spec))

