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
    [italianverbs.morphology :as morph]
    [italianverbs.tour :as tour]
    [italianverbs.unify :as unify]
    [korma.core :as k]
    ])

(defn truncate []
  (k/exec-raw ["TRUNCATE expression"]))

;; (populate 1 {:synsem {:infl :futuro :sem {:pred :chiedere :subj {:pred :lei}}}})
;; (populate 1 {:synsem {:infl :present :sem {:pred :chiedere :subj {:pred :lei}}}})
(defn populate [num & [ spec ]]
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

        ]
    (dotimes [n num]
      (let [language-1-sentence (engine/generate spec
                                                 it/small)

            language-1-sentence (cond
                                 (not (= :notfound (get-in language-1-sentence [:synsem :sem :subj] :notfound)))
                                 (unify/unify language-1-sentence
                                              {:synsem {:sem {:subj (lexfn/sem-impl (unify/get-in language-1-sentence
                                                                                                  [:synsem :sem :subj]))}}})
                                 true
                                 language-1-sentence)
            
            semantics (unify/strip-refs (get-in language-1-sentence [:synsem :sem] :top))

            debug (log/debug (str "semantics: " semantics))

            language-2-sentence (engine/generate {:synsem {:sem semantics
                                                           :subcat '()}}
                                                 en/small)]

        (k/exec-raw [(str "INSERT INTO expression (surface, structure, serialized, language, model) VALUES (?,"
                          "'" (json/write-str (unify/strip-refs language-1-sentence)) "'"
                          ","
                          "'" (str (unify/serialize language-1-sentence)) "'"
                          ","
                          "?,?)")
                     [(morph/fo language-1-sentence)
                      "it"
                      "small"]])

        (k/exec-raw [(str "INSERT INTO expression (surface, structure, serialized, language,model) VALUES (?,"
                          "'" (json/write-str (unify/strip-refs language-2-sentence)) "'"
                          ","
                          "'" (str (unify/serialize language-2-sentence)) "'"
                          ","
                          "?,?)")
                     [(morph/fo language-2-sentence)
                      "en"
                      "small"]])))))

(defn -main [& args]
  (if (not (nil? (first args)))
    (populate (Integer. (first args)))
    (populate 100)))
