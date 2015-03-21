(ns italianverbs.borges.reader
  [:refer-clojure :exclude [get get-in resolve merge]]
  [:require
   [clojure.data.json :as json]
   [clojure.tools.logging :as log]
   [korma.core :as db]
   [italianverbs.korma :as korma]
   [italianverbs.unify :as unify :refer [deserialize get-in ref? strip-refs unify]]])

;; Configure database's 'expression' table to find the expressions.
;; requires Postgres 9.4 or higher for JSONb operator '@>' support.

;; TODO: calling functions in here 'generate-X' is misleading
;; since they are querying a table to find sentences, not generating sentences from scratch.
(declare generate)

(defn generate-using-db [spec source-language target-language]
  (let [spec (unify spec
                    {:synsem {:subcat '()}})]
    (log/debug (str "spec pre-borges/generate:" spec))
    (generate spec source-language target-language)))

(defn generate-question-and-correct-set [spec source-language source-locale target-language target-locale]
  "Return a set of semantically-equivalent expressions, for a given spec in the target language, and
   and a single expression in the source language that contains the semantics shared by this set.
   To rephrase, the set of expressions in the target language share an identical semantics, and the single expression in the source
   language contains that semantics."
  (log/debug (str "generate target language set with spec: " spec))
  (let [spec (unify spec
                    {:synsem {:subcat '()}})

        ;; normalize for JSON lookup: convert a spec which is simply :top to be {}.
        json-input-spec (if (= :top spec)
                          {}
                          spec)
        
        json-spec (json/write-str (strip-refs json-input-spec))
        ]
    (log/debug (str "looking for expressions in language: " target-language " with spec: " spec))
    (log/debug (str "looking for expressions in language: " target-language " with json-spec: " json-spec))

    ;; get the structure of a random expression in the target language that matches the specification _spec_.
    ;; TODO: this is wasteful - we are getting *all* possible expressions, when we only need one (random) expression.
    (let [results (db/exec-raw [(str "SELECT target.serialized::text AS target,target.surface
                                        FROM expression AS target
                                       WHERE target.language=?
                                         AND target.structure IS NOT NULL
                                         AND target.surface != ''
                                         AND target.structure @> '" json-spec "'
")
                                [target-language]]
                               :results)]
      (if (empty? results)
        (do
          (log/error (str "nothing found in target language: " target-language " that matches spec: " spec))
          (throw (Exception. (str "nothing found in target language: " target-language " that matches spec: " spec))))

        ;; choose a random expression from the results of the above.
        (let [size-of-results (.size results)
              index-of-result (rand-int (.size results))
              debug (log/debug (str "number of target results:" size-of-results))
              debug (log/debug (str "index of target result:" index-of-result))
              target-expression (nth results index-of-result)
              debug (log/debug (str "target-expression is nil?" (nil? target-expression)))
              debug (log/debug (str "target-expression is: " target-expression))
              ]

          ;; Now get all the target expressions that are semantically equivalent to this expression's semantics,
          ;; and a single source expression whose semantics contain that same semantics.
          ;; TODO: consider selecting source where semantics contains that semantics, OR is contained by that semantics.
          ;; Both possiblities might be necessary if the lexicon of the source language and target language differ in how
          ;; much semantic information are encoded in entries for specific lexemes. For example, 'to go' in English specifies
          ;; {activity: true}, whereas 'andare' in English does not. This might be a bug in the Italian lexicon, but
          ;; this database lookup should be able to handle bugs like this.
          (let [result (deserialize (read-string (:target target-expression)))
                ;; TODO: allow queries that have refs - might be useful for modeling anaphora and binding.
                json-semantics (json/write-str (strip-refs (get-in result [:synsem :sem])))]
            (log/debug (str "semantics:" (strip-refs (get-in result [:synsem :sem]))))
            (log/debug (str "json-semantics:" json-semantics))

            (let [results
                  (db/exec-raw [(str "SELECT source.surface AS source,
                                             target.surface AS target 
                                        FROM (SELECT surface, source.structure->'synsem'->'sem' AS sem
                                                FROM expression AS source
                                               WHERE source.language=?
                                                 AND source.structure->'synsem'->'sem' @> '" json-semantics "' LIMIT 1) AS source
                                  INNER JOIN (SELECT DISTINCT surface, target.structure->'synsem'->'sem' AS sem
                                                         FROM expression AS target
                                                        WHERE target.language=?
                                                          AND target.structure->'synsem'->'sem' = '" json-semantics "') AS target 
                                                           ON (source.surface IS NOT NULL) 
                                                          AND (target.surface IS NOT NULL) 
                                                          AND (source.sem @> target.sem)")
                            [source-language target-language]]
                           :results)]
          (if (nil? (first (map :source results)))
            (do
              (log/error (str "no source expression found for semantics: " (strip-refs (get-in target-expression [:synsem :sem]))))
              (throw (Exception. (str "no source expression found for target expression: '" (:surface target-expression) "' and semantics: " 
                                      (get-in (strip-refs (deserialize (read-string (:target target-expression))))
                                              [:synsem :sem])))))
            {:source (first (map :source results))
             :targets (map :target results)}))))))))

(defn generate-all [spec language]
  "find all sentences in the expression table matching 'spec' in a given language."
  (let [spec (unify spec
                    {:synsem {:subcat '()}})

        ;; normalize for JSON lookup
        json-input-spec (if (= :top spec)
                          {}
                          spec)
        
        json-spec (json/write-str (strip-refs json-input-spec))
        ]
    (log/debug (str "looking for expressions in language: " language " with spec: " spec))
    (log/debug (str "SQL: "
                   (str "SELECT surface FROM expression WHERE language='" language "' AND structure @> "
                        "'" json-spec "'")))

    (let [results (db/exec-raw [(str "SELECT serialized::text 
                                        FROM expression 
                                       WHERE language=? AND structure @> "
                                     "'" json-spec "'")
                                [language]]
                               :results)]
      (map (fn [result]
             (deserialize (read-string (:serialized result))))
           results))))

(defn contains [spec]
  "Find the sentences in English that match the spec, and the set of Italian sentences that each English sentence contains."
    (let [spec (if (= :top spec)
                 {}
                 spec)
          json-spec (json/write-str (strip-refs spec))
          results (db/exec-raw [(str "SELECT DISTINCT * 
                                        FROM (SELECT english.surface   AS en,
                                                      italiano.surface AS it,               
                                   italiano.structure->'synsem'->'sem' AS italian_semantics,
                                    english.structure->'synsem'->'sem' AS english_semantics         
                                                FROM expression AS italiano
                                          INNER JOIN expression AS english                                 
                                                  ON english.structure @> '" json-spec "'
                                                 AND italiano.language = 'it'
                                                 AND english.language  = 'en'
                                                 AND (italiano.structure->'synsem'->'sem') @> 
                                                     (english.structure->'synsem'->'sem')) AS pairs 
                                    ORDER BY pairs.en")
                                []]
                               :results)]
      results))

;; (map #(str (get-in % [:en]) " / " (get-in % [:it]) " | ") (contains {:synsem {:sem {:pred :mangiare :subj {:pred :noi}}}}))

(defn get-meaning [input-map]
  "create a language-independent syntax+semantics that can be translated efficiently. The :cat specification helps speed up generation by avoiding searching syntactic constructs that are different from the desired input."
  (if (seq? input-map)
    (map get-meaning
         input-map)
    {:synsem {:cat (get-in input-map [:synsem :cat] :top)
              :sem (get-in input-map [:synsem :sem] :top)
              :subcat (get-in input-map [:synsem :subcat] :top)}}))
