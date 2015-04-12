(ns italianverbs.borges.writer
  (:refer-clojure :exclude [get-in merge])
  (:require
    [clojure.data.json :as json]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [italianverbs.engine :as engine]
    [italianverbs.korma :as korma]
    [italianverbs.lexiconfn :refer [sem-impl]]
    [italianverbs.morphology :refer [fo]]
    [italianverbs.unify :refer [get-in strip-refs serialize unify]]
    [korma.core :as k]
    ))

(defn truncate []
  (k/exec-raw ["TRUNCATE expression"]))

;; catch exceptions when trying to populate
;; TODO: more fine-grained approach to dealing with exceptions:
;; should be sensitive to what caused the failure:
;; (which language,lexicon,grammar,etc..).
(def mask-populate-errors false)

(defn populate [num source-language-model target-language-model & [ spec ]]
  (let [spec (if spec spec :top)
        debug (log/debug (str "spec(1): " spec))
        spec (cond
              (not (= :notfound (get-in spec [:synsem :sem :subj] :notfound)))
              (unify spec
                     {:synsem {:sem {:subj (sem-impl (get-in spec [:synsem :sem :subj]))}}})
              true
              spec)

        debug (log/debug (str "spec(2): " spec))

        ;; subcat is empty, so that this is a complete expression with no missing arguments.
        ;; e.g. "she sleeps" rather than "sleeps".
        spec (unify spec {:synsem {:subcat '()}}) 

        debug (log/debug (str "spec(3): " spec))

        ]
    (dotimes [n num]
      (let [target-language-sentence (engine/generate spec
                                                      target-language-model :enrich true)

            target-language-sentence (let [subj (get-in target-language-sentence
                                                        [:synsem :sem :subj] :notfound)]
                                       (cond (not (= :notfound subj))
                                             (do
                                               (log/debug (str "subject constraints: " subj))
                                               (unify target-language-sentence
                                                      {:synsem {:sem {:subj subj}}}))
                                             true
                                             target-language-sentence))
            
            semantics (strip-refs (get-in target-language-sentence [:synsem :sem] :top))
            debug (log/debug (str "semantics: " semantics))

            target-language-surface (fo target-language-sentence)
            debug (log/debug (str "lang-1 surface: " target-language-surface))

            source-language (:language (if (future? source-language-model)
                                         @source-language-model
                                         source-language-model))
            
            target-language (:language (if (future? target-language-model)
                                         @target-language-model
                                         target-language-model))
            
            error (if (or (nil? target-language-surface)
                          (= target-language-surface ""))
                    (let [message (str "Could not generate a sentence in target language '" target-language 
                                       "' for this semantics: " semantics)]
                      (if (= true mask-populate-errors)
                        (log/warn message)
                        ;; else
                        (throw (Exception. message)))))

            source-language-sentence (engine/generate {:synsem {:sem semantics
                                                                :subcat '()}}
                                                      source-language-model
                                                      :enrich true)
            source-language-surface (fo source-language-sentence)
            debug (log/debug (str "lang-2 surface: " source-language-surface))

            error (if (or (nil? source-language-surface)
                          (= source-language-surface ""))
                    (let [message (str "Could not generate a sentence in source language '" source-language 
                                       "' for this semantics: " semantics "; target language was: " target-language 
                                       "; target expression was: '" (fo target-language-sentence) "'")]
                      (if (= true mask-populate-errors)
                        (log/warn message)
                        ;; else
                        (throw (Exception. message)))))

            ]
        
        (k/exec-raw [(str "INSERT INTO expression (surface, structure, serialized, language, model) VALUES (?,"
                          "'" (json/write-str (strip-refs target-language-sentence)) "'"
                          ","
                          "'" (str (serialize target-language-sentence)) "'"
                          ","
                          "?,?)")
                     [target-language-surface
                      target-language
                      (:name target-language-model)]])

        (k/exec-raw [(str "INSERT INTO expression (surface, structure, serialized, language, model) VALUES (?,"
                          "'" (json/write-str (strip-refs source-language-sentence)) "'"
                          ","
                          "'" (str (serialize source-language-sentence)) "'"
                          ","
                          "?,?)")
                     [source-language-surface
                      source-language
                      (:name source-language-model)]])))))

(defn fill [num source-lm target-lm & [spec]]
  "wipe out current table and replace with (populate num spec)"
  (truncate)
  (let [spec (if spec spec :top)]
    (populate num source-lm target-lm spec)))

(defn -main [& args]
  (if (not (nil? (first args)))
    (populate (Integer. (first args)))
    (populate 100)))
