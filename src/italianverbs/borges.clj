(ns italianverbs.borges
  [:require
   [clojure.data.json :as json]
   [clojure.tools.logging :as log]
   [korma.core :as db]
   [italianverbs.korma :as korma]
   [italianverbs.unify :as unify :refer [deserialize strip-refs unify]]])

(declare generate)

(defn generate-using-db [spec language-name]
  (let [spec (unify spec
                    {:synsem {:subcat '()}})]
    (log/debug (str "spec pre-borges/generate:" spec))
    (generate spec language-name)))

;; requires Postgres 9.4 or higher for JSON operator '@>' support.
(defn generate [spec language-name]
  "generate a sentence matching 'spec' given the supplied language model."
  (let [spec (unify spec
                    {:synsem {:subcat '()}})

        ;; normalize for JSON lookup
        json-input-spec (if (= :top spec)
                          {}
                          spec)
        
        json-spec (json/write-str (strip-refs json-input-spec))
        ]
    (log/debug (str "looking for expressions in language: " language-name " with spec: " spec))
    (log/debug (str "SQL: "
                   (str "SELECT surface FROM expression WHERE language='" language-name "' AND structure @> "
                        "'" json-spec "'")))

    (let [results (db/exec-raw [(str "SELECT serialized::text 
                                        FROM expression 
                                       WHERE language=? AND structure @> "
                                     "'" json-spec "'")
                                [language-name]]
                               :results)
          size-of-results (.size results)
          index-of-result (rand-int (.size results))
          debug (log/debug (str "number of results:" size-of-results))
          debug (log/debug (str "index of result:" index-of-result))]
      (if (not (empty? results))
        (deserialize (read-string (:serialized (nth results index-of-result))))
        (do (log/error "Nothing found in database that matches search: " json-spec)
            (throw (Exception. (str "Nothing found in database that matches search: " json-spec))))))))

;; thanks to http://schinckel.net/2014/05/25/querying-json-in-postgres/ for his good info.

;; SELECT count(*) FROM (SELECT DISTINCT english.surface AS en, italiano.surface AS it FROM italiano INNER JOIN english ON italiano.structure->synsem->'sem' = english.structure->synsem->'sem' ORDER BY english.surface) AS foo;
    
;; SELECT * FROM (SELECT synsem->'sem'->'pred'::text AS pred,surface FROM english) AS en WHERE en.pred='"andare"';
;; SELECT it.surface  FROM (SELECT synsem->'sem'->'pred' AS pred,surface,synsem FROM italiano) AS it WHERE it.synsem->'sem' @> '{"pred":"andare"}';

;; SELECT surface FROM italiano WHERE synsem->'sem' @> '{"pred":"andare"}';

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

