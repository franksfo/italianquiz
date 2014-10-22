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
  (cond 

   (:italiano input)
   (string/trim (italiano/get-string (:italiano input)))

   (seq? input)
   (string/join ""
                (map fo input))

   (:italiano input)
   (merge
    {:italiano (string/trim (italiano/get-string (:italiano input)))}
    (fo (dissoc input :italiano)))

   (:english input)
   (merge {:english (string/trim (english/get-string (:english input)))}
          (fo (dissoc input :english)))

   true
   ""))

(defn fo-ps [input]
  (cond (seq? input)
        (map fo-ps input)

        (:italiano input)
        {:italiano (fo input)
         :semantics (strip-refs (get-in input [:synsem :sem]))
         :rule (:rule input)
         :head (fo (:head input))
         :comp (fo (:comp input))}

        (:english input)
        {:english (fo input)
         :rule (:rule input)
         :head (fo (:head input))
         :comp (fo (:comp input))}

        true
        ""))

(defn remove-parens [str]
  (replace str #"\(.*\)" ""))

(defn finalize [expr]
  (cond

   ;; This is the case where we've generated totally separate english and italian phrase structure trees,
   ;; in which case we need to extract the english and italian strings from their respective trees.
   (and (map? expr)
        (= (.size (keys expr)) 2)
        (= (set (keys expr))
           #{:italiano :english}))
   (let [english
         (english/get-string (get-in expr '(:english :english)))
         italian
         ;; TODO: much inconsistent. make the path: [:italiano :italiano]  rather than [:italiano :italian]
         (italiano/get-string (get-in expr '(:italiano :italian)))]
     (log/debug (str "input expr: " (fo expr)))
     (log/debug (str "finalized english: " english))
     (log/debug (str "finalized italian: " italian))
     {:italian italian
      :english english
      :english-tree (get-in expr [:english])
      :italian-tree (get-in expr [:italiano])})

   (= :fail expr)
   :fail

   true
   "TODO: fix."))
