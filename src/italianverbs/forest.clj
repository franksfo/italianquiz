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
(declare lazy-mapcat-shuffle)
(declare path-to-map)
(declare add-complement)
(declare add-all-complements-to-bolts)
(declare add-complements-to-bolts)

(declare lightning-bolt)

(defn generate [spec grammar lexicon & [cache]]
  (-> (lightning-bolt grammar
                      lexicon
                      spec 0 cache)
      (add-complements-to-bolts [:head :head :head :comp] :top grammar lexicon cache)
      (add-complements-to-bolts [:head :head :comp]       :top grammar lexicon cache)
      (add-complements-to-bolts [:head :comp]             :top grammar lexicon cache)
      (add-complements-to-bolts [:comp]                   :top grammar lexicon cache)))

;; TODO: add usage of rule-to-lexicon cache (rather than using lexicon directly)
(defn lightning-bolt [grammar lexicon spec & [ depth cache]]
  "Returns a lazy-sequence of all possible trees given a spec, where
there is only one child for each parent, and that single child is the
head of its parent. generate (above) 'decorates' each returned lightning bolt
of this function with complements."
  (log/trace (str "lighting-bolt@" depth))
  (let [maxdepth 3 ;; maximum depth of a lightning bolt: H1 -> H2 -> H3 where H3 must be a lexeme, not a phrase.
        depth (if depth depth 0)

        parents (lazy-shuffle (filter #(not (fail? (unifyc spec %)))
                                      (map (fn [rule]
                                             (unifyc spec rule))
                                           grammar)))]
    (if (and (not (nil? parents))
             (not (empty? parents)))
      (let [parents-with-head
            (lazy-mapcat-shuffle
             (fn [generates-parent-with-head]
               (generates-parent-with-head))
             (list

              ;; 1. generate list of all phrases where the head child of each parent is a lexeme.
              (fn []
                (lazy-mapcat-shuffle
                 (fn [parent]
                   (let [cached (if cache
                                  (get cache (get-lex parent :head cache spec)))
                         lexicon (if cached cached lexicon)]
                     (log/trace (str "lighting-bolt@" depth ": overh(lex) with parent: " (fo-ps parent)))
                     (log/trace (str "lighting-bolt@" depth ": overh(lex) with lexicon: " (fo lexicon)))
                     (log/trace (str "lighting-bolt@" depth ": overh(lex) with cache-entry type: " (type cached)))
                     (log/trace (str "lighting-bolt@" depth ": overh(lex) with cache-entry size: " (type cached)))
                     (overh parent (lazy-shuffle lexicon))))
                 parents
                 depth
                 "overh(lex)"))
           
              ;; 2. generate list of all phrases where the head child of each parent is itself a phrase.
              ;; note max-depth check and recursive call to lightning-bolt with (+ 1 depth).
              (fn []
                (if (< depth maxdepth)
                  (lazy-mapcat-shuffle
                   (fn [parent]
                     (do
                       (log/debug (str "lighting-bolt@" depth ": overh(lighting-bolt) with parent: " (fo-ps parent)))
                       (overh parent
                              (lightning-bolt grammar lexicon
                                              (get-in parent [:head])
                                              (+ 1 depth)
                                              cache))))
                   parents
                   depth
                   "overh(lighting-bolt)")
                  (do
                    (log/debug (str "lighting-bolt@" depth ": terminating."))
                    nil))))
             depth
             "overh(lex;lighting-bolt)")]

    (log/trace (str "type of parents-with-head t=" (type parents-with-head)))
    parents-with-head))))

(defn add-complement [bolt path spec grammar lexicon cache]
  (let [input-spec spec
        spec (unifyc spec (get-in bolt path :no-path))]
    (if (not (= spec :no-path))
      (let [immediate-parent (get-in bolt (butlast path))
            cached (if cache
                     (do
                       (let [result (get-lex immediate-parent :comp cache spec)]
                         (if (not (nil? result))
                           (log/debug (str " cached lexical subset is: " (.size result)))
                           (log/warn (str " no cached value for: " (fo-ps immediate-parent))))
                         result))
                     (do (log/warn (str "no cache: will go through entire lexicon."))
                         nil))
            complement-candidate-lexemes (if cached cached lexicon)]
        (log/debug (str "add-complement: " (fo-ps bolt) "@" path "::" (show-spec spec)))
        (let [semantics (get-in spec [:synsem :sem])]
          (if (not (nil? semantics))
            (if (not (nil? semantics)) (log/debug (str "  with semantics:" semantics)))))
        (log/trace (str " immediate parent:" (get-in immediate-parent [:rule])))
        (log/trace (str "add-complement to: " (fo-ps bolt) " with spec " (show-spec spec) " at path: " path))
        (filter (fn [result]
                  (not (fail? result)))
                (map (fn [complement]
                       (log/trace (str "add-complement: " (fo-ps bolt) " with complement: " (fo complement)))
                       (let [result
                             (unifyc bolt
                                     (path-to-map path
                                                  complement))
                             is-fail? (fail? result)]
                         (if is-fail?
                           (log/trace (str "add-complement: " (fo-ps bolt) " + " (fo complement) " => " (if (not is-fail?)
                                                                                                          (fo-ps result)
                                                                                                          ":fail")))
                           (log/debug (str "add-complement: " (fo-ps bolt) " + " (fo complement) " => " (if (not is-fail?)
                                                                                                          (fo-ps result)
                                                                                                          ":fail"))))
                         (if is-fail? :fail result)))
                     
                     ;; lazy-sequence of complements to pass one-by-one to the above (map)'s function.
                     (if (= (rand-int 2) 0)
                       (lazy-cat (lazy-shuffle complement-candidate-lexemes)
                                 (generate spec grammar lexicon cache))
                       (lazy-cat (generate spec grammar lexicon cache)
                                 (lazy-shuffle complement-candidate-lexemes))))))

      ;; path doesn't exist in bolt: simply return the bolt unmodified.
      (do
        (log/debug " complement added to: " (fo-ps bolt))
        (list bolt)))))

(defn add-complements-to-bolts [bolts path spec grammar lexicon cache]
  (if (not (empty? bolts))
    (lazy-cat
     (add-complement (first bolts) path spec grammar lexicon cache)
     (add-complements-to-bolts (rest bolts) path spec grammar lexicon cache))))

(defn hlcl [cache grammar spec lexicon spec]
  (generate (unifyc {:head {:phrasal false}
                     :comp {:phrasal false}})
            grammar lexicon cache))

(defn hpcl [cache grammar spec lexicon spec]
  (generate (unifyc {:head {:phrasal true}
                     :comp {:phrasal false}})
            grammar lexicon cache))

(defn hlcp [cache grammar spec lexicon spec]
  (generate (unifyc {:head {:phrasal false}
                     :comp {:phrasal true}})
            grammar lexicon cache))

(defn hpcp [cache grammar spec lexicon spec]
  (generate (unifyc {:head {:phrasal true}
                     :comp {:phrasal true}})
            grammar lexicon cache))

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

;; (take 10 (repeatedly #(fo-ps (take 1 (forest/lighting-bolt (shuffle grammar) (shuffle lexicon) {:synsem {:cat :verb :subcat '()}})))))
(defn path-to-map [path val]
  (let [feat (first path)]
    (if feat
      {feat (path-to-map (rest path) val)}
      val)))

