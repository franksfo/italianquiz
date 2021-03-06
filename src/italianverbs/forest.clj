(ns italianverbs.forest
  (:refer-clojure :exclude [get-in deref resolve find future parents])
  (:require
   [clojure.core :as core]
   ;; have to exclude partition-by because of some namespace clash, not sure how else to fix
   [clojure.core.async :as async :exclude [partition-by]]
   [clojure.set :refer :all]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [italianverbs.cache :refer (build-lex-sch-cache get-comp-phrases-of get-head-phrases-of get-lex
                                                   get-parent-phrases-for-spec
                                                   overc overh)]
   [italianverbs.morphology :refer (fo fo-ps)]
   [italianverbs.over :as over]
   [italianverbs.unify :refer (dissoc-paths get-in fail? fail-path-between lazy-shuffle ref? remove-false 
                                            remove-top-values-log strip-refs show-spec unifyc)]))

(def concurrent false)
(declare path-to-map)
(declare add-complement)
(declare add-all-complements-to-bolts)
(declare add-complements-to-bolts)

(declare lightning-bolt)
(declare generate-all)

(defn generate [spec grammar lexicon index]
  (first (take 1 (generate-all spec grammar lexicon index))))

(defn generate-all-with-model [spec {grammar :grammar
                                     index :index
                                     lexicon :lexicon}]
  (let [index (if (future? index) @index index)
        lexicon (if (future? lexicon) @lexicon lexicon)]
    (log/info (str "using grammar of size: " (.size grammar)))
    (log/info (str "using index of size: " (.size index)))
    (if (seq? spec)
      (map generate-all spec grammar lexicon index)
      (generate spec grammar
                (flatten (vals lexicon))
                index))))

(defn generate-all [spec grammar lexicon index]
  (filter #(not (fail? %))
          (cond (and (or (seq? spec)
                         (vector? spec))
                     (not (empty? spec)))
                (lazy-cat (generate-all (first spec) grammar lexicon index)
                          (generate-all (rest spec) grammar lexicon index))
                true
                (do
                  (log/debug (str "generate-all with semantics: " (show-spec (remove-false (get-in spec [:synsem :sem])))))
                  (log/trace (str "generate-all(details): " (show-spec spec)))
                  (-> (lightning-bolt grammar
                                      lexicon
                                      spec 0 index)
                      ;; TODO: allow more than a fixed maximum depth of generation (here, 4 levels from top of tree).
                      (add-complements-to-bolts [:head :head :head :comp] :top grammar lexicon index)
                      (add-complements-to-bolts [:head :head :comp]       :top grammar lexicon index)
                      (add-complements-to-bolts [:head :comp]             :top grammar lexicon index)
                      (add-complements-to-bolts [:comp]                   :top grammar lexicon index))))))
  
;; TODO: add usage of rule-to-lexicon cache (rather than using lexicon directly)
(defn lightning-bolt [grammar lexicon spec & [ depth index parent]]
  "Returns a lazy-sequence of all possible trees given a spec, where
there is only one child for each parent, and that single child is the
head of its parent. generate (above) 'decorates' each returned lightning bolt
of this function with complements."
  (log/debug (str "lighting-bolt@" depth ": grammar:" (string/join ", " (map :rule grammar))))
  (let [maxdepth 3 ;; maximum depth of a lightning bolt: H1 -> H2 -> H3 where H3 must be a lexeme, not a phrase.
        index (if (future? index) @index index)
        lexicon (if (future? lexicon) @lexicon lexicon)
        depth (if depth depth 0)
        ;; TODO: unifyc is expensive: factor out into a let.
        candidate-parents (lazy-shuffle (filter #(not (fail? %))
                                                (map (fn [rule]
                                                       (unifyc spec rule))
                                                     (if parent (get-head-phrases-of parent index)
                                                         grammar))))
        debug (if parent 
                (do (log/debug (str "parent: " (:rule parent)))
                    (log/debug (str "lexical head candidates given parent:"
                                    (fo (get-lex parent :head index spec))))))
        debug (log/debug (str "grammar size: " (.size grammar)))
        debug (log/debug (str "candidate-parents size: " (if (nil? candidate-parents)
                                                           "no candidate-parents"
                                                           (.size candidate-parents))))
        debug (log/debug (str "candidate-parents: " (if (nil? candidate-parents)
                                                      "no candidate-parents"
                                                      (string/join "," (map #(get-in % [:rule])
                                                                            candidate-parents)))))


        ]
    (if (seq candidate-parents)
      (let [lexical ;; 1. generate list of all phrases where the head child of each parent is a lexeme.
            (mapcat (fn [parent]
                      (overh parent (lazy-shuffle (get-lex parent :head index spec))))
                    candidate-parents)

            phrasal ;; 2. generate list of all phrases where the head child of each parent is itself a phrase.
            ;; recursively call lightning-bolt with (+ 1 depth).
            (if (< depth maxdepth)
              (mapcat (fn [parent]
                        (overh parent
                               (lightning-bolt grammar lexicon
                                               (get-in parent [:head])
                                               (+ 1 depth)
                                               index parent)))
                      candidate-parents))]
        (log/debug (str "first parent: " (fo-ps (first candidate-parents))))
        (log/debug (str "lightning-bolt phrasal result: " (string/join ", " (fo-ps phrasal))))
        (log/debug (str "lightning-bolt lexical result: " (string/join ", " (fo-ps lexical))))
        (if (= (rand-int 2) 0)
          (lazy-cat lexical phrasal)
          (lazy-cat phrasal lexical))))))

(defn add-complement [bolt path spec grammar lexicon cache]
  (let [input-spec spec
        cache (if (future? cache) @cache cache)
        from-bolt bolt ;; so we can show what (add-complement) did to the input bolt, for logging.
        bolt-spec (get-in bolt path :no-path)
        spec (unifyc spec bolt-spec)
        lexicon (if (future? lexicon) @lexicon lexicon)]
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
        (let [shuffled-candidate-lexical-complements (lazy-shuffle complement-candidate-lexemes)
              return-val
              (filter (fn [result]
                        (or true
                        (not (fail? result))))
                      (map (fn [complement]
                             (log/debug (str "add-complement: " (fo-ps bolt) " with complement: " (fo complement) " at path: " path))
                             (let [result
                                   (unifyc bolt
                                           (path-to-map path
                                                        complement))
                                   is-fail? (fail? result)]
                               (if is-fail?
                                 (do
                                   (log/debug (str "add-complement: " (fo-ps bolt) " + " (fo complement) " =(FAIL)=> " result))
                                   (log/trace (str "fail-path-between:" (fail-path-between (strip-refs (get-in bolt path))
                                                                                           (strip-refs complement)))))

                                 (log/debug (str "add-complement: " (fo-ps bolt) " + " (fo complement) " => " (if (not is-fail?)
                                                                                                                (fo-ps result)
                                                                                                                ":fail"))))
                               (if is-fail? :fail result)))
                     
                           ;; lazy-sequence of phrasal complements to pass one-by-one to the above (map)'s function.
                           (let [phrasal-complements (generate-all spec grammar lexicon cache)]
                             (log/debug (str "add-complements: generated phrasal complements: "
                                             (string/join ", " (map fo-ps phrasal-complements))))
                             (if (= (rand-int 2) 0)
                               (lazy-cat shuffled-candidate-lexical-complements phrasal-complements)
                               (lazy-cat phrasal-complements shuffled-candidate-lexical-complements)))))]
          (let [first-return-val-formatted (fo-ps (first return-val))
                run-time (- (System/currentTimeMillis) start-time)]
            (if (not (empty? (seq return-val)))
              (log/debug (str " add-complement took " run-time " msec: " (fo-ps from-bolt) " => " first-return-val-formatted))

              ;; else, no complements could be added to this bolt.
              (do
                (log/warn (str " add-complement took " run-time " msec, but found no lexical complements for " (fo-ps from-bolt) ". Complements tried were: " (vec (map fo complement-candidate-lexemes))))
                ;; TODO: show warn about not finding ny phrasal complements, as well as not finding any lexical complements.
                (log/debug (str " fail-paths:"))
                (vec (map (fn [lexeme]
                            (log/debug (str " path in bolt: " path))
                            (log/debug (str " value of bolt at path: " (get-in bolt path)))
                            (log/debug (str " value of gender at path: " (get-in bolt (concat path [:synsem :agr :gender]))))
                            (log/debug (str " FP:"
                                            (merge {:lexeme (fo lexeme)}
                                                   (fail-path-between (strip-refs (get-in bolt path))
                                                                      (strip-refs lexeme))))))
                          complement-candidate-lexemes))))
                                                               
            return-val)))

      ;; path doesn't exist in bolt: simply return the bolt unmodified.
      (do
        (log/trace " add-complement: " (fo-ps from-bolt) ": no path: " path " to which to add complement.")
        (list bolt)))))

(defn add-complements-to-bolts [bolts path spec grammar lexicon cache]
  (if (seq bolts)
    (lazy-cat (add-complement (first bolts) path spec grammar lexicon cache)
              (add-complements-to-bolts (rest bolts) path spec grammar lexicon cache))))

(defn path-to-map [path val]
  (let [feat (first path)]
    (if feat
      {feat (path-to-map (rest path) val)}
      val)))
