(ns italianverbs.populate
  [:use
   [clojure.core :exclude [find]]
   [italianverbs.lexiconfn]
   [italianverbs.lexicon]
   [italianverbs.morphology]
   ]
   [:require
    [somnium.congomongo :as mongo]
    [italianverbs.generate :as gen]
    [italianverbs.html :as html]
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
    (let [sentence (gen/finalize (gen/random-sentence))]
      (mongo/insert! :sentences {:italian (unify/get-in sentence '(:italian))
                                 :english (unify/get-in sentence '(:english))})))
  (let [count (mongo/fetch-count :sentences)]
    (println (str "sentence collection now has " count " sentences."))
    count))

(defn -main [& args]
  (if (not (nil? (first args)))
    (populate (Integer. (first args)))
    (populate 100)))

(defn random-sentence []
  (let [count (mongo/fetch-count :sentences)
        sentences (mongo/fetch :sentences)]
    (nth sentences (rand-int count))))
