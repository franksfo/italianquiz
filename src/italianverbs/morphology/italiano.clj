(ns italianverbs.morphology.italiano
  (:refer-clojure :exclude [get-in])
  (:use [italianverbs.unify :only (ref? get-in fail?)])
  (:require
   [clojure.core :as core]
   [clojure.string :as string]
   [clojure.tools.logging :as log]))

(defn suffix-of [word]
  "compute the final character given a lexical entry and agreement info in :agr."
  (let [suffix (cond

                (and (= (get-in word '(:obj-agr :gender)) :fem)
                     (= (get-in word '(:obj-agr :number)) :sing))
                "a"

                (and (= (get-in word '(:obj-agr :gender)) :fem)
                     (= (get-in word '(:obj-agr :number)) :plur))
                "e"

                (= (get-in word '(:obj-agr :number)) :plur)
                "i"

                (and (= (get-in word '(:agr :gender)) :fem)
                     (= (get-in word '(:agr :number)) :sing)
                     (= (get-in word '(:essere)) true))
                "a"

                (and (= (get-in word '(:agr :gender)) :fem)
                     (= (get-in word '(:agr :number)) :plur)
                     (= (get-in word '(:essere)) true))
                "e"

                (and (= (get-in word '(:agr :number)) :plur)
                     (= (get-in word '(:essere)) true))
                "i"

                true
                "o"

                )]
    suffix))

(defn stem-per-futuro [infinitive drop-e]
  "_infinitive_ should be a string (italian verb infinitive form)"
  (cond
   (re-find #"giare$" infinitive)
   (string/replace infinitive #"giare$" "ger")

   (re-find #"ciare$" infinitive)
   (string/replace infinitive #"ciare$" "cer")

   (and
    (= true drop-e)
    (re-find #"are$" infinitive))
   (string/replace infinitive #"are$" "r")

   (re-find #"are$" infinitive)
   (string/replace infinitive #"are$" "er")

   (and
    (= true drop-e)
    (re-find #"ere$" infinitive))
   (string/replace infinitive #"ere$" "r")

   (re-find #"ere$" infinitive)
   (string/replace infinitive #"ere$" "er")

   (re-find #"ire$" infinitive)
   (string/replace infinitive #"ire$" "ir")

   true
   infinitive))

(defn fo-ps-it [expr]
  "show the phrase-structure of a phrase structure tree, e.g [hh21 'mangiare (to eat)' [cc10 'il (the)' 'pane(bread)']]"
  ;; [:first = {:head,:comp}] will not yet be found in expr, so this head-first? will always be false.
  (let [head-first? (= :head (get-in expr [:first]))]
    (cond

     (and
      (or (set? expr)
          (seq? expr)
          (vector? expr))
      (empty? expr))
     (str "")


     (and
      (or (set? expr)
          (seq? expr)
          (vector? expr))
      (not (empty? expr)))

     ;; expr is a sequence of some kind. Assume each element is a phrase structure tree and show each.
     (map (fn [each]
            (fo-ps-it each))
          expr)


     (and (map? expr)
          (:italiano expr))
     (fo-ps-it (:italiano expr))

     (and (map? expr)
          (:rule expr)
          (= (get-in expr '(:italian :a))
             (get-in expr '(:comp :italian))))
     ;; complement first
     (str "[" (:rule expr) " "
          (fo-ps-it (get-in expr '(:comp)))
          " "
          (fo-ps-it (get-in expr '(:head)))
          "]")

     (and (map? expr)
          (:rule expr))
     ;; head first ('else' case of above.)
     (str "[" (:rule expr) " "
          (fo-ps-it (get-in expr '(:head)))
          " "
          (fo-ps-it (get-in expr '(:comp)))
          "]")


     (and (map? expr)
          (:comment expr)
          (= (get-in expr '(:italian :a))
             (get-in expr '(:comp :italian))))
     ;; complement first
     (str "[" (:comment expr) " "
          (fo-ps-it (get-in expr '(:comp)))
          " "
          (fo-ps-it (get-in expr '(:head)))
          "]")

     (and (map? expr)
          (:comment expr))
     ;; head first ('else' case of above.)
     (str "[" (:comment expr) " "
          (fo-ps-it (get-in expr '(:head)))
          " "
          (fo-ps-it (get-in expr '(:comp)))
          "]")

     (and
      (map? expr)
      (:italian expr))
     (str (get-italian-1 (get-in expr '(:italian))))

     true
     expr)))

(defn fo-ps [expr]
  "show the phrase-structure of a phrase structure tree, e.g [hh21 'mangiare (to eat)' [cc10 'il (the)' 'pane(bread)']]"
  ;; [:first = {:head,:comp}] will not yet be found in expr, so this head-first? will always be false.
  (let [head-first? (= :head (get-in expr [:first]))]
    (cond

     (and
      (or (set? expr)
          (seq? expr)
          (vector? expr))
      (empty? expr))
     (str "")


     (and
      (or (set? expr)
          (seq? expr)
          (vector? expr))
      (not (empty? expr)))

     ;; expr is a sequence of some kind. Assume each element is a phrase structure tree and show each.
     (map (fn [each]
            (fo-ps each))
          expr)


     (and (map? expr)
          (:italiano expr))
     (fo-ps-it (:italiano expr))

     (and (map? expr)
          (:rule expr)
          (= (get-in expr '(:italian :a))
             (get-in expr '(:comp :italian))))
     ;; complement first
     (str "[" (:rule expr) " "
          (fo-ps (get-in expr '(:comp)))
          " "
          (fo-ps (get-in expr '(:head)))
          "]")

     (and (map? expr)
          (:rule expr))
     ;; head first ('else' case of above.)
     (str "[" (:rule expr) " "
          (fo-ps (get-in expr '(:head)))
          " "
          (fo-ps (get-in expr '(:comp)))
          "]")


     (and (map? expr)
          (:comment expr)
          (= (get-in expr '(:italian :a))
             (get-in expr '(:comp :italian))))
     ;; complement first
     (str "[" (:comment expr) " "
          (fo-ps (get-in expr '(:comp)))
          " "
          (fo-ps (get-in expr '(:head)))
          "]")

     (and (map? expr)
          (:comment expr))
     ;; head first ('else' case of above.)
     (str "[" (:comment expr) " "
          (fo-ps (get-in expr '(:head)))
          " "
          (fo-ps (get-in expr '(:comp)))
          "]")

     (and
      (map? expr)
      (:italian expr))
     (get-italian-1 (get-in expr '(:italian)))

     true
     expr)))
