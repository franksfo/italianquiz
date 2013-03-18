(ns italianverbs.populate
  [:use
   [clojure.core :exclude [find]]
   [italianverbs.lexiconfn]
   [italianverbs.lexicon]
   [italianverbs.grammar]
   [italianverbs.morphology]]
  [:require
   [somnium.congomongo :as mongo]
   [italianverbs.fs :as fs]
   [italianverbs.generate :as gen]
   [italianverbs.html :as html]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.tools.logging :as log]])

(defn populate [num]
  (mongo/mongo! :db "mydb")
  (mongo/make-connection "mydb" :host "localhost")
  (mongo/destroy! :sentences {})
  (dotimes [n num]
    (let [sentence (gen/random-sentence)]
      (mongo/insert! :sentences {:italian (fs/get-in sentence '(:italian))
                                 :english (fs/get-in sentence '(:english))})))
  (let [count (mongo/fetch-count :sentences)]
    (println (str "sentence collection now has " count " sentences."))
    count))

(defn random-sentence []
  (let [count (mongo/fetch-count :sentences)
        sentences (mongo/fetch :sentences)]
    (nth sentences (rand-int count))))
