(ns italianverbs.borges.writer
  (:refer-clojure :exclude [get-in merge])
  (:require
    [clojure.data.json :as json]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [italianverbs.engine :as engine]
    [italianverbs.english :as en]
    [italianverbs.espanol :as es]
    [italianverbs.italiano :as it]
    [italianverbs.korma :as korma]
    [italianverbs.lexiconfn :as lexfn]
    [italianverbs.morphology :as morph :refer [fo]]
    [italianverbs.unify :as unify :refer [get-in]]
    [korma.core :as k]
    ))

(defn truncate []
  (k/exec-raw ["TRUNCATE expression"]))

(defn populate [num source-language-model target-language-model & [ spec ]]
  (let [spec (if spec spec :top)
        debug (log/debug (str "spec(1): " spec))
        spec (cond
              (not (= :notfound (get-in spec [:synsem :sem :subj] :notfound)))
              (unify/unify spec
                           {:synsem {:sem {:subj (lexfn/sem-impl (unify/get-in spec [:synsem :sem :subj]))}}})
              true
              spec)

        debug (log/debug (str "spec(2): " spec))

        spec (unify/unify spec {:synsem {:subcat '()}})

        debug (log/debug (str "spec(3): " spec))

        source-language (:language (if (future? source-language-model)
                                     @source-language-model
                                     source-language-model))

        target-language (:language (if (future? target-language-model)
                                     @target-language-model
                                     target-language-model))


        ]
    (dotimes [n num]
      (let [target-language-sentence (engine/generate spec
                                                      target-language-model :enrich true)

            target-language-sentence (cond
                                 (not (= :notfound (get-in target-language-sentence [:synsem :sem :subj] :notfound)))
                                 (let [subj (lexfn/sem-impl (unify/get-in target-language-sentence
                                                                          [:synsem :sem :subj]))]
                                   (log/debug (str "subject constraints: " subj))
                                   (unify/unify target-language-sentence
                                                {:synsem {:sem {:subj subj}}}))

                                 true
                                 target-language-sentence)
            
            semantics (unify/strip-refs (get-in target-language-sentence [:synsem :sem] :top))
            debug (log/debug (str "semantics: " semantics))

            target-language-surface (morph/fo target-language-sentence)
            debug (log/debug (str "lang-1 surface: " target-language-surface))

            source-language-sentence (engine/generate {:synsem {:sem semantics
                                                                :subcat '()}}
                                                 source-language-model
                                                 :enrich true)
            source-language-surface (morph/fo source-language-sentence)
            debug (log/debug (str "lang-2 surface: " source-language-surface))

            error (if (nil? target-language-surface)
                    (do (log/error (str "surface of target-language was null with semantics: " semantics ))
                        (throw (Exception. (str "surface of target-language was null with semantics: " semantics )))))

            error (if (nil? source-language-surface)
                    (do (log/error (str "surface of source-language was null with semantics: " semantics))
                        (throw (Exception. (str "surface of source-language was null with semantics: " semantics )))))

            ]
        
        (try
          (do
            (if (= target-language-surface "")
              (throw (Exception. (str "could not generate a sentence in target language for this semantics: " semantics "; source langauge expression was: " source-language-surface))))

            (if (= source-language-surface "")
              (throw (Exception. (str "could not generate a sentence in source language (English) for this semantics: " semantics))))

            (k/exec-raw [(str "INSERT INTO expression (surface, structure, serialized, language, model) VALUES (?,"
                              "'" (json/write-str (unify/strip-refs target-language-sentence)) "'"
                              ","
                              "'" (str (unify/serialize target-language-sentence)) "'"
                              ","
                              "?,?)")
                       [target-language-surface
                        target-language
                        (:name target-language-model)]])

            (k/exec-raw [(str "INSERT INTO expression (surface, structure, serialized, language, model) VALUES (?,"
                              "'" (json/write-str (unify/strip-refs source-language-sentence)) "'"
                              ","
                              "'" (str (unify/serialize source-language-sentence)) "'"
                              ","
                              "?,?)")
                         [source-language-surface
                          source-language
                          (:name source-language-model)]]))
          (catch Exception e (log/error (str "Could not add expression pair to database - caused by: " e))))))))

(defn fill [num & [spec]]
  "wipe out current table and replace with (populate num spec)"
  (truncate)
  (populate num en/small it/small spec))


(defn -main [& args]
  (if (not (nil? (first args)))
    (populate (Integer. (first args)))
    (populate 100)))
