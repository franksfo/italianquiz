(ns italianverbs.loadtesting
  (:require
   [clojure.math.numeric-tower :as math]
   [italianverbs.cache :refer (create-index)]
   [italianverbs.english :as en]
   [italianverbs.italiano :as it]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.translate :refer [get-meaning]]
   [italianverbs.ug :refer (head-principle)]))

;; <begin linguistic setup>
(def possible-preds [:bere :dormire :leggere :mangiare :parlare])

(def mini-en-grammar
  (filter #(= (:rule %) "s-present")
          en/grammar))

(def mini-en-index (create-index mini-en-grammar (flatten (vals en/lexicon)) head-principle))

(def mini-it-grammar
  (filter #(= (:rule %) "s-present")
          it/grammar))

(def mini-it-index (create-index mini-it-grammar (flatten (vals it/lexicon)) head-principle))

(def en {:generate en/generate
         :grammar mini-en-grammar
         :index mini-en-index})

(def it {:generate it/generate
         :grammar mini-it-grammar
         :index mini-it-index})
;; </end linguistic setup>

(defn load-bearing [ {source :source
                      target :target}]
  ;; choose a question's semantic's pred at random:
  (let [pred (nth possible-preds (rand-int (.size possible-preds)))
        spec
        {:head {:phrasal :top}
         :comp {:phrasal false}
         :synsem {:sem {:pred pred}
                  :cat :verb
                  :subcat '()}}]
    ;; generate a question and an
    (let [question
          ((:generate source)
           spec 
           {:grammar (:grammar source)
            :index (:index source)})


          answer-constraints {:synsem {:subcat '()}
                              :head {:phrasal :top}
                              :comp {:phrasal false}}

          answer
          ((:generate target)
           (merge (get-meaning question)
                                        ; answer-constraints
                  )
           {:grammar (:grammar target)
            :index (:index target)})]
      {:question (fo question)
       :answer (fo answer)})))


