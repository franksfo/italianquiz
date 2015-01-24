(ns italianverbs.populate
   [:require
    [clojure.data.json :as json]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [italianverbs.engine :as engine]
    [italianverbs.english :as en]
    [italianverbs.italiano :as it]
    [italianverbs.korma :as korma]
    [italianverbs.tour :as tour]
    [italianverbs.unify :as unify]
    [korma.core :as k]
    ])

(defn truncate []
  (k/exec-raw ["TRUNCATE english"])
  (k/exec-raw ["TRUNCATE italiano"])
  (k/exec-raw ["TRUNCATE espanol"]))

(defn populate [num]
  (dotimes [n num]
    (let [italian-sentence (engine/generate {:synsem {:subcat '()}}
                                            it/small)
          english-sentence (engine/generate {:synsem {:sem (unify/get-in italian-sentence [:synsem :sem])}}
                                            en/small)]

      (k/exec-raw [(str "INSERT INTO italiano (surface, syntax, semantics) VALUES (?,to_json(?::text),to_json(?::text))")
                   [(fo italian-sentence)
                    (json/write-str (unify/strip-refs (unify/get-in italian-sentence [:synsem])))
                    (json/write-str (unify/strip-refs (unify/get-in italian-sentence [:synsem :sem])))]])

      (k/exec-raw [(str "INSERT INTO english (surface, syntax, semantics) VALUES (?,to_json(?::text),to_json(?::text))")
                   [(fo english-sentence)
                    (json/write-str (unify/strip-refs (unify/get-in english-sentence [:synsem])))
                    (json/write-str (unify/strip-refs (unify/get-in english-sentence [:synsem :sem])))]]))))

(defn -main [& args]
  (if (not (nil? (first args)))
    (populate (Integer. (first args)))
    (populate 100)))
