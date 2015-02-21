(ns italianverbs.populate
   [:require
    [clojure.data.json :as json]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [italianverbs.engine :as engine]
    [italianverbs.english :as en]
    [italianverbs.italiano :as it]
    [italianverbs.korma :as korma]
    [italianverbs.lexiconfn :as lexfn]
    [italianverbs.morphology :as morph :refer [fo]]
    [italianverbs.unify :as unify]
    [korma.core :as k]
    ])

(defn truncate []
  (k/exec-raw ["TRUNCATE expression"]))

;; (populate 1 {:synsem {:infl :futuro :sem {:pred :chiedere :subj {:pred :lei}}}})
;; (populate 1 {:synsem {:infl :present :sem {:pred :chiedere :subj {:pred :lei}}}})
;;(do (truncate)  (populate 1 {:synsem {:sem {:pred :chiedere :subj {:pred :lei}}}}))

(defn populate [num & [ spec source-model target-model]]
  (let [spec (if spec spec :top)
        debug (log/debug (str "spec(1): " spec))
        source-model (if source-model source-model it/small)
        target-model (if target-model target-model en/small)
        spec (cond
              (not (= :notfound (get-in spec [:synsem :sem :subj] :notfound)))
              (unify/unify spec
                           {:synsem {:sem {:subj (lexfn/sem-impl (unify/get-in spec [:synsem :sem :subj]))}}})
              true
              spec)

        debug (log/debug (str "spec(2): " spec))

        spec (unify/unify spec {:synsem {:subcat '()}})

        debug (log/debug (str "spec(3): " spec))

        ]
    (dotimes [n num]
      (let [source-language-sentence (engine/generate spec
                                                 source-model :do-enrich true)

            source-language-sentence (cond
                                 (not (= :notfound (get-in source-language-sentence [:synsem :sem :subj] :notfound)))
                                 (let [subj (lexfn/sem-impl (unify/get-in source-language-sentence
                                                                          [:synsem :sem :subj]))]
                                   (log/debug (str "subject constraints: " subj))
                                   (unify/unify source-language-sentence
                                                {:synsem {:sem {:subj subj}}}))

                                 true
                                 source-language-sentence)
            
            semantics (unify/strip-refs (get-in source-language-sentence [:synsem :sem] :top))

            debug (log/debug (str "semantics: " semantics))

            target-language-sentence (engine/generate {:synsem {:sem semantics
                                                           :subcat '()}}
                                                 target-model :do-enrich true)

            source-language-surface (morph/fo source-language-sentence)
            target-language-surface (morph/fo target-language-sentence)

            debug (log/debug (str "lang-1 surface: " source-language-surface))
            debug (log/debug (str "lang-2 surface: " target-language-surface))

            ;; TODO: provide more diagnostics about what the problem was.
            error (if (or (= source-language-surface "") (nil? source-language-surface))
                    (do (log/error (str "surface of source language was null with semantics: " semantics ))
                        (throw (Exception. (str "surface of language-1 was null with semantics: " semantics )))))

            error (if (or (= target-language-surface "") (nil? target-language-surface))
                    (do (log/error (str "surface of target language was null with semantics: " semantics))
                        (throw (Exception. (str "surface of language-2 was null with semantics: " semantics )))))

            ]

        (k/exec-raw [(str "INSERT INTO expression (surface, structure, serialized, language, model) VALUES (?,"
                          "'" (json/write-str (unify/strip-refs source-language-sentence)) "'"
                          ","
                          "'" (str (unify/serialize source-language-sentence)) "'"
                          ","
                          "?,?)")
                     [source-language-surface
                      "it"
                      (:name source-model)]])

        (k/exec-raw [(str "INSERT INTO expression (surface, structure, serialized, language,model) VALUES (?,"
                          "'" (json/write-str (unify/strip-refs target-language-sentence)) "'"
                          ","
                          "'" (str (unify/serialize target-language-sentence)) "'"
                          ","
                          "?,?)")
                     [target-language-surface
                      "en"
                      (:name target-model)]])))))

(defn -main [& args]
  (if (not (nil? (first args)))
    (populate (Integer. (first args)))
    (populate 100)))

(defn do-a-bunch []
  (do (populate 20 {:synsem {:sem {:pred :enter}}})
      (populate 20 {:synsem {:sem {:pred :tornare}}})
      (populate 20 {:synsem {:sem {:pred :venire}}})
      (populate 20 {:synsem {:sem {:pred :ritornare}}})
      (populate 20 {:synsem {:sem {:pred :chiedere}}})
      (populate 20 {:synsem {:sem {:pred :abbraciare}}})
      (populate 20 {:synsem {:sem {:pred :abbraciare}}})
      (populate 20 {:synsem {:sem {:pred :abbracciare}}})
      (populate 20 {:synsem {:sem {:pred :abandon}}})
      (populate 20 {:synsem {:sem {:pred :accep}}})
      (populate 20 {:synsem {:sem {:pred :accept}}})
      (populate 20 {:synsem {:sem {:pred :accompany}}})
      (populate 20 {:synsem {:sem {:pred :dormire}}})
      (populate 20 {:synsem {:sem {:pred :essere}}})

))

;; (do (truncate)(populate 10 {:synsem {:sem {:pred :mangiare}}} it/medium en/medium))
