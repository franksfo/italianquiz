(ns italianverbs.borges.reader
  [:require
   [clojure.data.json :as json]
   [clojure.tools.logging :as log]
   [korma.core :as db]
   [italianverbs.korma :as korma]
   [italianverbs.unify :as unify :refer [deserialize ref? strip-refs unify]]])

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

(defn generate [spec source-language target-language]
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
          result (if (not (empty? results)) (nth results index-of-result))]
      ;; now get all the target expressions that are semantically equivalent to this expression's semantics.
      (let [result (deserialize (read-string (:target result)))
            ;; TODO: allow queries that have refs - might be useful for modeling anaphora and binding.
            json-semantics (json/write-str (strip-refs (get-in result [:synsem :sem])))]
        (let [source-expression (db/exec-raw [(str "SELECT surface
                                                      FROM expression AS source
                                                      WHERE source.language=?
                                                        AND source.structure->'synsem'->'sem' = '" json-semantics "' LIMIT 1")
                                              [source-language]]
                                             :results)
              
              target-expressions (db/exec-raw [(str "SELECT 
                                                   DISTINCT surface
                                                       FROM expression AS target
                                                      WHERE target.language=?
                                                        AND target.structure->'synsem'->'sem' = '" json-semantics "'")
                                               [target-language]]
                                              :results)]
          {:source (first (map :surface source-expression))
           :targets (map :surface target-expressions)})))))

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

;; thanks to http://schinckel.net/2014/05/25/querying-json-in-postgres/ for his good info.

;; SELECT surface FROM italiano WHERE synsem->'sem' @> '{"pred":"andare"}';


;; SELECT count(*) FROM (SELECT DISTINCT english.surface AS en, italiano.surface AS it FROM italiano INNER JOIN english ON italiano.structure->synsem->'sem' = english.structure->synsem->'sem' ORDER BY english.surface) AS foo;
    
;; SELECT * FROM (SELECT synsem->'sem'->'pred'::text AS pred,surface FROM english) AS en WHERE en.pred='"andare"';
;; SELECT it.surface  FROM (SELECT synsem->'sem'->'pred' AS pred,surface,synsem FROM italiano) AS it WHERE it.synsem->'sem' @> '{"pred":"andare"}';


;;SELECT italiano.surface,english.surface FROM italiano INNER JOIN english ON italiano.synsem->'sem' = english.synsem->'sem';

;; number of distinct english <-> italiano translation pairs
;; SELECT count(*) FROM (SELECT DISTINCT english.surface AS en, italiano.surface AS it FROM italiano INNER JOIN english ON italiano.synsem->'sem' = english.synsem->'sem' ORDER BY english.surface) AS foo;

(defn get-meaning [input-map]
  "create a language-independent syntax+semantics that can be translated efficiently. The :cat specification helps speed up generation by avoiding searching syntactic constructs that are different from the desired input."
  (if (seq? input-map)
    (map get-meaning
         input-map)
    {:synsem {:cat (get-in input-map [:synsem :cat] :top)
              :sem (get-in input-map [:synsem :sem] :top)
              :subcat (get-in input-map [:synsem :subcat] :top)}}))

(defn generate-question-and-correct-set [source-language target-language]
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

