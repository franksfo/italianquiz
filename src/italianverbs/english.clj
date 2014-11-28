(ns italianverbs.english
  (:refer-clojure :exclude [get-in]))

(require '[clojure.tools.logging :as log])
(require '[italianverbs.cache :refer (build-lex-sch-cache create-index over spec-to-phrases)])
(require '[italianverbs.generate :as generate])
(require '[italianverbs.grammar.english :as gram])
(require '[italianverbs.lexicon.english :as lex])
(require '[italianverbs.lexiconfn :refer (compile-lex unify)])
(require '[italianverbs.morphology.english :as morph])
(require '[italianverbs.parse :as parse])
(require '[italianverbs.ug :refer :all])

(def get-string morph/get-string)
(def grammar gram/grammar)

;; TODO: just a stub for now:
(defn exception-generator [lexicon]
  (let [lexeme-kv (first lexicon)
        lexemes (second lexeme-kv)]
    (if lexeme-kv
      (list {})
      (list {}))))

(defn phonize [a-map a-string]
  (let [common {:phrasal false}]
    ;; TODO: remove support for either list-of-maps - too confusing. Instead, just require a list of maps.
    (cond (or (vector? a-map) (seq? a-map))
          (map (fn [each-entry]
                 (phonize each-entry a-string))
               a-map)

          (map? a-map)
          (unify {:english {:english a-string}}
                 common
                 a-map)

        true
        (unify a-map
               {:english a-string}
               common))))

(def lexicon (compile-lex lex/lexicon-source exception-generator phonize 
                          morph/english-specific-rules))

(defn lookup [token]
  "return the subset of lexemes that match this token from the lexicon."
  (morph/analyze token #(get lexicon %)))

(def begin (System/currentTimeMillis))
(log/info "building grammatical and lexical index..")
(def cache nil)
;; TODO: trying to print cache takes forever and blows up emacs buffer:
;; figure out how to change printable version to show only keys and first value or something.
(def cache (create-index grammar (flatten (vals lexicon)) head-principle))

(def end (System/currentTimeMillis))
(log/info "Built grammatical and lexical cache in " (- end begin) " msec.")

(defn parse [string]
  (parse/parse string lexicon lookup grammar))

(defn sentence [ & [spec]]
  (let [spec (if spec spec :top)]
    (generate/sentence spec grammar (flatten (vals lexicon)) cache)))

(defn generate [ & [spec]]
  (let [spec (if spec spec :top)]
    (if (seq? spec)
      (map generate spec)
      (generate/generate spec grammar (flatten (vals lexicon)) cache))))


