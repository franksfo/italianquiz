(ns italianverbs.populate
  [:use
   [clojure.core :exclude [find]]
   [italianverbs.lexiconfn]
   [italianverbs.lexicon]
   [italianverbs.morphology]
   ]
   [:require
    [somnium.congomongo :as mongo]
    [italianverbs.engine :as engine]
    [italianverbs.english :as en]
    [italianverbs.html :as html]
    [italianverbs.italiano :as it]
    [italianverbs.unify :as unify]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    ]
  )

(defn populate [num]
  (mongo/mongo! :db "mydb")
  (mongo/make-connection "mydb" :host "localhost")
  ;; TODO: add switch to avoid removing existing mongodb, if desired.
  (mongo/destroy! :sentences {})
  (dotimes [n num]
    (let [italian-sentence (it/sentence)
          english-sentence (engine/generate {:synsem {:sem (get-in italian-sentence [:synsem :sem])}})]
      (mongo/insert! :sentences {:italian (fo italian-sentence)
                                 :english (fo english-sentence)})))
  (let [count (mongo/fetch-count :sentences)]
    (println (str "sentence collection now has " count " sentences."))
    count))

(defn -main [& args]
  (if (not (nil? (first args)))
    (populate (Integer. (first args)))
    (populate 100)))
