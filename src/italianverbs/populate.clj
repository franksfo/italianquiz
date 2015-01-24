(ns italianverbs.populate
   [:require
    [clojure.data.json :as json]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [italianverbs.engine :as engine]
    [italianverbs.english :as en]
    [italianverbs.italiano :as it]
    [italianverbs.korma :as korma]
    [italianverbs.morphology :as morph]
    [italianverbs.tour :as tour]
    [italianverbs.unify :as unify]
    [korma.core :as k]
    ])

(defn truncate []
  (k/exec-raw ["TRUNCATE expression"]))

(defn populate [num]
  (dotimes [n num]
    (let [italian-sentence (engine/generate {:synsem {:subcat '()}}
                                            it/small)
          italian-semantics (unify/get-in italian-sentence [:synsem :sem])

          english-sentence (engine/generate {:synsem {:sem italian-semantics}}
                                            en/small)]

      (k/exec-raw [(str "INSERT INTO expression (surface, synsem, language, model) VALUES (?,"
                        "'" (json/write-str (unify/strip-refs (unify/get-in italian-sentence [:synsem]))) "'"
                        ",?,?)")
                   [(morph/fo italian-sentence)
                    "it"
                    "small"]])

      (k/exec-raw [(str "INSERT INTO expression (surface, synsem,language,model) VALUES (?,"
                        "'" (json/write-str (unify/strip-refs (unify/get-in english-sentence [:synsem]))) "'"
                        ",?,?)")
                   [(morph/fo english-sentence)
                    "en"
                    "small"]]))))

(defn -main [& args]
  (if (not (nil? (first args)))
    (populate (Integer. (first args)))
    (populate 100)))
