(ns italianverbs.lexicon
  (:refer-clojure :exclude [get-in resolve find merge])
  (:require
   [clojure.set :refer (union)]
   [clojure.tools.logging :as log]
   [italianverbs.lexiconfn :refer (cache-serialization sem-impl subcat0 subcat1)]
   ;; TODO: i.l.a_essere,i.l.esso_noi,i.l.notizie_potere,i.l.qualche_volte_volere are going away:
   ;; instead use i.l.italiano and i.l.english.
   [italianverbs.lex.a_essere :refer :all]
   [italianverbs.lex.esso_noi :refer :all]
   [italianverbs.lex.notizie_potere :refer :all]
   [italianverbs.lex.qualche_volta_volere :refer :all]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.pos :refer :all]
   [italianverbs.unify :as unify]
   [italianverbs.unify :refer :all]))

;; stub that is redefined by italianverbs/mongo or interfaces to other dbs.
(defn clear! [])

(def lookup-in
  "find all members of the collection that matches with query successfully."
  (fn [query collection]
    (loop [coll collection matches nil]
      (if (not (empty? coll))
        (let [first-val (first coll)
              result (unify/match (unify/copy query) (unify/copy first-val))]
          (if (not (unify/fail? result))
            (recur (rest coll)
                   (cons first-val matches))
            (recur (rest coll)
                   matches)))
        matches))))

(declare lexicon)

(defn lookup [query]
  (lookup-in query @lexicon))

(defn choose-lexeme [spec]
  (first (lazy-shuffle (lookup spec))))

(defn it [italian]
  "same as it but no type conversion of singleton sets to take the first member."
  (let [result
        (union (set (lookup {:italiano italian}))
               (set (lookup {:italiano {:infinitive italian}}))
               (set (lookup {:italiano {:infinitive {:infinitive italian}}}))
               (set (lookup {:italiano {:italiano italian}}))
               (set (lookup {:italiano {:irregular {:passato italian}}})))]
    result))

(defn en [english]
  (let [result
        (union (set (lookup {:english english}))
               (set (lookup {:english {:infinitive english}}))
               (set (lookup {:english {:infinitive {:infinitive english}}}))
               (set (lookup {:english {:english english}}))
               (set (lookup {:english {:irregular {:passato english}}})))]
    result))

(clear!)

(load "pos")

(load "lex/a_essere")
(load "lex/esso_noi")
(load "lex/notizie_potere")
(load "lex/qualche_volta_volere")

(defn commonnoun [lexical-entry]
  ;; subcat non-empty: pronoun is false
  (cond (and (= (get-in lexical-entry '(:synsem :cat)) :noun)
             (= (not (empty? (get-in lexical-entry '(:synsem :subcat)))))
             (not (= (get-in lexical-entry '(:synsem :pronoun)) true))
             (not (= (get-in lexical-entry '(:synsem :propernoun)) true)))
        (unifyc lexical-entry
                (unifyc agreement-noun
                        common-noun
                        {:synsem {:pronoun false
                                  :subcat {:1 {:cat :det}
                                           :2 '()}}}))
        true
        lexical-entry))

(defn semantic-implicature [lexical-entry]
  {:synsem {:sem (sem-impl (get-in lexical-entry '(:synsem :sem)))}})

(defn put-a-bird-on-it [lexical-entry]
  "example lexical entry transformer."
  (cond (map? lexical-entry)
        (conj {:bird 42}
              lexical-entry)
        true
        lexical-entry))

(defn category-to-subcat [lexical-entry]
  (cond (or (= (get-in lexical-entry '(:synsem :cat)) :det)
            (= (get-in lexical-entry '(:synsem :cat)) :adverb))
        (unifyc
         subcat0
         lexical-entry)

        (and (= (get-in lexical-entry '(:synsem :cat)) :adjective)
             (not (= (get-in lexical-entry '(:synsem :sem :comparative)) true)))
        (unifyc
         subcat1
         lexical-entry)

        (= (get-in lexical-entry '(:synsem :cat)) :sent-modifier)
        (unifyc
         {:synsem {:subcat {:1 {:cat :verb
                                :subcat '()}
                            :2 '()}}}
         lexical-entry)

        true
        lexical-entry))

(defn determiner-stuff [lexical-entry]
  (cond (= (get-in lexical-entry '(:synsem :cat)) :det)
        (unifyc determiner
                lexical-entry)
        true
        lexical-entry))

;; TODO: regenerate :serialized whenever creating a new lexical entry
(defn make-intransitive-variant [lexical-entry]
  (cond

   (and (= (get-in lexical-entry [:synsem :cat]) :verb)
        (exists? lexical-entry [:synsem :subcat :2])
        (not (empty? (get-in lexical-entry [:synsem :subcat :2]))))

   ;; create an intransitive version of this transitive verb by removing the second arg (:synsem :subcat :2), and replacing with nil.
   (list
    ;; MUSTDO: regenerate :serialized.

    (cache-serialization
     (merge (dissoc-paths lexical-entry (list [:synsem :subcat :2]
                                              [:serialized]))
            {:synsem {:subcat {:2 '()}}
             :canary :tweet43})) ;; if the canary tweets, then the runtime is getting updated correctly.

    lexical-entry) ;; the original transitive lexeme.

   true
   (list lexical-entry)))

;; rules like make-intransitive-variant multiply a single lexeme into zero or more lexemes: i.e. their function signature is map => seq(map).
(defn apply-multi-rules [lexeme]
  (make-intransitive-variant lexeme))

(def lexicon

  ;; this filter is for debugging purposes to restrict lexicon to particular entries, if desired.
  ;; default shown is (not (nil? entry)) i.e. no restrictions except that an entry must be non-nil.
  ;;  (currently there is one nil below: "chiunque (anyone)").
  (future
    (filter (fn [entry]
              (or (= entry :fail)
                  (not (nil? entry))))

            ;; TODO: move this fn to lexiconfn: keep any code out of the lexicon proper.
            ;; this (map) adds, to each lexical entry, a copy of the serialized form of the entry.

            (mapcat #(apply-multi-rules %)
                    (map transform
                         (concat
                          a-essere
                          esso-noi
                          notizie-potere
                          qualche_volta-volere))))))
