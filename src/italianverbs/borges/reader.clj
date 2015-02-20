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

(defn generate-question-and-correct-set [spec source-language target-language]
  "Return a set of semantically-equivalent expressions, for a given spec in the target language, and
   and a single expression in the source language that is also semantically equivalent to each of them.
   To rephrase, both the set of expressions in the target language and the single expression in the source
   language will share identical semantics. Another implementation might instead implement
   the case where the source language semantics is not identical to that of the target-language semantics,
   but rather contains it, or, in other words, that the source-language expression is possibly more general
   than that of the target language."
  (log/debug (str "generate target language set with spec: " spec))
  (let [spec (unify spec
                    {:synsem {:subcat '()}})

        ;; normalize for JSON lookup: convert a spec which is simply :top to be {}.
        json-input-spec (if (= :top spec)
                          {}
                          spec)
        
        json-spec (json/write-str (strip-refs json-input-spec))
        ]
    (log/debug (str "looking for expressions in language: " source-language " with spec: " spec))

    ;; get the structure of a random expression in the target language that matches the specification _spec_.
    (let [results (db/exec-raw [(str "SELECT target.serialized::text AS target,surface
                                        FROM expression AS target
                                       WHERE target.language=? 
                                         AND target.structure @> '" json-spec "'")
                                [target-language]]
                               :results)
          size-of-results (.size results)
          index-of-result (rand-int (.size results))
          debug (log/debug (str "number of results:" size-of-results))
          debug (log/debug (str "index of result:" index-of-result))
          initial-result (nth results index-of-result)
          ]
      ;; now get all the target expressions that are semantically equivalent to this expression's semantics.
      (let [result (deserialize (read-string (:target initial-result)))
            ;; TODO: allow queries that have refs - might be useful for modeling anaphora and binding.
            json-semantics (json/write-str (strip-refs (get-in result [:synsem :sem])))]
        (log/debug (str "semantics:" (strip-refs (get-in result [:synsem :sem]))))
        (log/debug (str "json-semantics:" json-semantics))
        (let [results
              (db/exec-raw [(str "SELECT source.surface 
                                      AS source,
                                         target.surface 
                                      AS target 
                                    FROM (SELECT surface, source.structure->'synsem'->'sem' AS sem
                                            FROM expression AS source
                                           WHERE source.language=?
                                             AND source.structure->'synsem'->'sem' = '" json-semantics "' LIMIT 1) AS source
                              INNER JOIN (SELECT surface, target.structure->'synsem'->'sem' AS sem
                                            FROM expression AS target
                                           WHERE target.language=?
                                             AND target.structure->'synsem'->'sem' = '" json-semantics "') AS target 
                                              ON (source.surface IS NOT NULL) 
                                             AND (target.surface IS NOT NULL) 
                                             AND (source.sem = target.sem)")
                            [source-language target-language]]
                           :results)]
          (if (nil? (first (map :source results)))
            (do
              (log/error (str "no source found for semantics: " (strip-refs (get-in result [:synsem :sem]))))
              (throw (Exception. (str "no source found for semantics: " (strip-refs (get-in result [:synsem :sem]))))))
            {:source (first (map :source results))
             :targets (map :target results)}))))))

(defn generate-all [spec language]
  "find all sentences in the library matching 'spec' in a given language."
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

(defn get-meaning [input-map]
  "create a language-independent syntax+semantics that can be translated efficiently. The :cat specification helps speed up generation by avoiding searching syntactic constructs that are different from the desired input."
  (if (seq? input-map)
    (map get-meaning
         input-map)
    {:synsem {:cat (get-in input-map [:synsem :cat] :top)
              :sem (get-in input-map [:synsem :sem] :top)
              :subcat (get-in input-map [:synsem :subcat] :top)}}))

(defn generate-question-and-correct-set-mockup [source-language target-language]
  "generate a question and a set of possible correct answers, given request."
  {:question "I went"
   :answer-set ["io sono andato","sono andato"]})

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
                                   english.structure->'synsem'->'sem'  AS english_semantics         
                                                FROM expression AS italiano
                                          INNER JOIN expression AS english                                 
                                                  ON english.structure @> '" json-spec "'
                                                 AND italiano.language = 'it'
                                                 AND english.language = 'en'
                                                 AND (italiano.structure->'synsem'->'sem') @> 
                                                     (english.structure->'synsem'->'sem')) AS pairs 
                                    ORDER BY pairs.en")
                                []]
                               :results)]
      results))

;; (map #(str (get-in % [:en]) " / " (get-in % [:it]) " | ") (contains {:synsem {:sem {:pred :mangiare :subj {:pred :noi}}}}))

