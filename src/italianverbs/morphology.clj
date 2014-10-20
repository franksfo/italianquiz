(ns italianverbs.morphology
  (:refer-clojure :exclude [get-in merge resolve]))

(require '[clojure.core :as core])
(require '[clojure.string :as string])
(require '[clojure.tools.logging :as log])
(require '[italianverbs.morphology.english :as english])
(require '[italianverbs.morphology.italiano :as italiano])
(require '[italianverbs.stringutils :refer :all])
(require '[italianverbs.unify :refer :all])

(defn phrase-is-finished? [phrase]
  (cond
   (string? phrase) true
   (map? phrase)
   (or (phrase-is-finished? (get-in phrase '(:italian)))
       (string? (get-in phrase '(:infinitive)))
       (and (phrase-is-finished? (get-in phrase '(:a)))
            (phrase-is-finished? (get-in phrase '(:b)))))
   :else false))

(defn fo [input]
  (cond (:italiano input)
        (merge
         {:italiano (italiano/get-string (:italiano input))}
         (fo (dissoc input :italiano)))
        (:english input)
        (merge {:english (english/get-string (:english input))}
               (fo (dissoc input :english)))
        true
        ""))

(defn fo-ps [input]
  (cond (:italiano input)
        (merge
         {:italiano (italiano/get-string (:italiano input))}
         (fo (dissoc input :italiano)))
        (:english input)
        (merge {:english (english/get-string (:english input))}
               (fo (dissoc input :english)))
        true
        ""))

(defn remove-parens [str]
  (replace str #"\(.*\)" ""))
