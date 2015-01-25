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

(defn populate [num & [ spec ]]
  (let [spec (if spec spec :top)]
    (dotimes [n num]
      (let [language-1-sentence (engine/generate (unify/unify spec {:synsem {:subcat '()}})
                                                 it/small)
            semantics (unify/get-in language-1-sentence [:synsem :sem])
            
            semantics (unify/unifyc semantics
                                    {:subj (lexfn/sem-impl (unify/get-in language-1-sentence [:synsem :sem :subj]))})
            
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
