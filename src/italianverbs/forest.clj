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
(defn lightning-bolt [grammar lexicon spec & [ depth cache parent]]
  "Returns a lazy-sequence of all possible trees given a spec, where
there is only one child for each parent, and that single child is the
head of its parent. generate (above) 'decorates' each returned lightning bolt
of this function with complements."
  (log/debug (str "lighting-bolt@" depth))
  (let [maxdepth 3 ;; maximum depth of a lightning bolt: H1 -> H2 -> H3 where H3 must be a lexeme, not a phrase.
        depth (if depth depth 0)

        parents (lazy-shuffle (filter #(not (fail? (unifyc spec %)))
                                      (map (fn [rule]
                                             (unifyc spec rule))
                                           (if parent (get-head-phrases-of parent cache)
                                               grammar))))]
    (if (seq parents)
      (let [lexical ;; 1. generate list of all phrases where the head child of each parent is a lexeme.
            (mapcat (fn [parent]
                      (overh parent (lazy-shuffle (get-lex parent :head cache spec))))
                    parents)

            phrasal ;; 2. generate list of all phrases where the head child of each parent is itself a phrase.
            ;; note max-depth check and recursive call to lightning-bolt with (+ 1 depth).
            (if (< depth maxdepth)
              (mapcat (fn [parent]
                        (overh parent
                               (lightning-bolt grammar lexicon
                                               (get-in parent [:head])
                                               (+ 1 depth)
                                               cache parent)))
                      parents))]
        (log/debug (str "first parent: " (fo-ps (first parents))))
        (log/debug (str "first phrasal parent: " (fo-ps (first phrasal))))
        (if (= (rand-int 2) 0)
          (lazy-cat lexical phrasal)
          (lazy-cat phrasal lexical))))))

(defn add-complement [bolt path spec grammar lexicon cache]
  (let [input-spec spec
        from-bolt bolt ;; so we can show what (add-complement) did to the input bolt, for logging.
        bolt-spec (get-in bolt path :no-path)
        spec (unifyc spec bolt-spec)]
    (if (not (= bolt-spec :no-path)) ;; check if this bolt has this path in it.
      (log/debug (str "add-complement: " (fo-ps bolt) "@" path "::" (show-spec spec))))
    (if (not (= bolt-spec :no-path)) ;; check if this bolt has this path in it.
      (let [immediate-parent (get-in bolt (butlast path))
            start-time (System/currentTimeMillis)
            cached (if cache
                     (do
                       (let [result (get-lex immediate-parent :comp cache spec)]
                         (if (not (nil? result))
                           (log/debug (str " cached lexical subset ratio: " 
                                           (string/replace (str (/ (* 1.0 (/ (.size lexicon) (.size result)))))
                                                           #"\.(..).*"
                                                           (fn [[_ two-digits]] (str "." two-digits)))))
                           (log/warn (str " no cached value for: " (fo-ps immediate-parent))))
                         result))
                     (do (log/warn (str "no cache: will go through entire lexicon."))
                         nil))
            complement-candidate-lexemes (if cached cached lexicon)]
        (let [semantics (get-in spec [:synsem :sem])]
          (if (not (nil? semantics))
            (if (not (nil? semantics)) (log/debug (str "  with semantics:" semantics)))))
        (log/trace (str " immediate parent:" (get-in immediate-parent [:rule])))
        (log/trace (str "add-complement to: " (fo-ps bolt) " with spec " (show-spec spec) " at path: " path))
        (let [return-val
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
                                 (log/trace (str "add-complement: " (fo-ps bolt) " + " (fo complement) " => " (if (not is-fail?)
                                                                                                                (fo-ps result)
                                                                                                                ":fail"))))
                               (if is-fail? :fail result)))
                     
                           ;; lazy-sequence of complements to pass one-by-one to the above (map)'s function.
                           (let [phrasal (generate spec grammar lexicon cache)
                                 lexical (lazy-shuffle complement-candidate-lexemes)]
                             (if (= (rand-int 2) 0)
                               (lazy-cat lexical phrasal)
                               (lazy-cat phrasal lexical)))))]
          (let [run-time (- (System/currentTimeMillis) start-time)]
            (if (seq return-val)
              (log/debug (str " add-complement took " run-time " msec: " (fo-ps from-bolt) " => " (fo-ps (first return-val))))
              (log/warn (str " add-complement took " run-time " msec, but found no complement for " (fo-ps from-bolt) ".")))
            return-val)))

      ;; path doesn't exist in bolt: simply return the bolt unmodified.
      (do
        (log/trace " add-complement: " (fo-ps from-bolt) ": no path: " path " to which to add complement.")
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
