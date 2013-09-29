(ns italianverbs.grammar
  (:refer-clojure :exclude [get-in resolve])
  (:use [clojure.set :only (union intersection)]
        [clojure.core :exclude (get-in resolve merge)]
        [italianverbs.generate :only (generate moreover-head moreover-comp gen14 over3)]
        [italianverbs.lexicon :only (it1)]
        [italianverbs.lexiconfn :only (unify sem-impl)]
        [italianverbs.morphology :only (finalize fo italian-article get-italian-1 get-italian)]
        [italianverbs.ug]
        [italianverbs.unify :only (copy fail? serialize get-in resolve)]
        )

  (:require [clojure.tools.logging :as log]
            [italianverbs.lexicon :as lex]
            [italianverbs.unify :as unify]
            [clojure.string :as string])
)

(def tinylex
  (union ;(it1 "aiutare")
         (it1 "andare")
         (it1 "dormire")
         (it1 "la")
         (it1 "il")
         (it1 "io")
         (it1 "ragazzo")
         (it1 "ragazza")
         (it1 "un")
         (it1 "vedere")
        ))

(log/info "begin italian-english specifics.")

(def common-nouns
  (filter (fn [lexeme]
            (and (= (get-in lexeme '(:synsem :cat)) :noun)
                 (= (get-in lexeme '(:synsem :subcat :1 :cat)) :det)))
          cc10-heads))

(def dets
  (filter (fn [lexeme]
            (= (get-in lexeme '(:synsem :cat)) :det))
          cc10-heads))

(def propernouns-and-pronouns
  ;; TODO: more compile-time filtering
  (filter (fn [lexeme]
            (and (= (unify/get-in lexeme '(:synsem :cat)) :noun)
                 (= (unify/get-in lexeme '(:synsem :subcat)) '())))
          cc10-comps))

(def pronouns
  ;; TODO: more compile-time filtering
  (filter (fn [lexeme]
            (and (= (unify/get-in lexeme '(:synsem :cat)) :noun)
                 (not (= (unify/get-in lexeme '(:synsem :pronoun)) false))))
          propernouns-and-pronouns))

(def propernouns
  ;; TODO: more compile-time filtering
  (filter (fn [lexeme]
            (and (= (unify/get-in lexeme '(:synsem :cat)) :noun)
                 (not (= (unify/get-in lexeme '(:synsem :pronoun)) true))))
          propernouns-and-pronouns))

(def intransitive-verbs
  (filter (fn [candidate]
            (and (= :notfound (unify/get-in candidate '(:synsem :subcat :2 :cat) :notfound))
                 (= (unify/get-in candidate '(:synsem :cat)) :verb)))
          lex/lexicon))

(def transitive-verbs
  (filter (fn [candidate]
            ;; filter Vs to reduce number of candidates we need to filter:
            ;; (only transitive verbs)
            (and (not (= :notfound (unify/get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                 (= (unify/get-in candidate '(:synsem :cat)) :verb)))
          (lazy-shuffle hh21-heads)))

(def intransitive-verbs
  (filter (fn [lexeme]
            (and (= (get-in lexeme '(:synsem :cat)) :verb)
                 (= (get-in lexeme '(:synsem :subcat :2)) '())))
          lex/lexicon))

(def vp-v
  (fn [] (lazy-shuffle intransitive-verbs)))

(def vp-pron-v
  (fn []
    (gen-ch21
     ;; Object Pronoun
     (lazy-shuffle pronouns)

     (filter (fn [candidate]
               ;; filter Vs to reduce number of candidates we need to filter:
               ;; (only transitive verbs)
               (and (not (= :notfound (unify/get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                    (= (unify/get-in candidate '(:synsem :cat)) :verb)))
             (lazy-shuffle hh21-heads)))))

(def vp-vaux-past-intransitive
  (fn []
    (gen-hh21
     ;; v[aux]
     (filter (fn [candidate]
               (and (not (= :notfound (unify/get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                    (= (unify/get-in candidate '(:synsem :cat)) :verb)
                    (= (unify/get-in candidate '(:synsem :aux)) true)))
             (lazy-shuffle hh21-heads))

     ;; v[past]
     (lazy-shuffle intransitive-verbs))))

;; note that order of arguments to gen-cc10 is reverse of s-to-np-vp, because
;; gen-cc10 and other generic functions always have their arguments head, then comp.

(defn s-to-np-vp-inner [np vps]
  (if (first vps)
    (lazy-cat (gen-cc10 (first vps) np)
              (s-to-np-vp-inner np (rest vps)))))

(defn s-to-np-vp [nps vps]
  (if (first nps)
    (lazy-cat (s-to-np-vp-inner (first nps) vps)
              (s-to-np-vp (rest nps) vps))))

(defn sentences []
  (lazy-seq
   ;; parent: S -> NP VP
   (s-to-np-vp

    ;; subject NP.
    (shuffle (list ;np-to-det-n
                   (lazy-shuffle propernouns-and-pronouns)))

    ;; VP: 4 expansions:
    (shuffle
     (list

      ;; 1. VP -> V
      vp-v

      ;; 2. VP -> V NP
;      vp-v-np

      ;; 3. VP -> Pronoun V
      vp-pron-v

      ;; 4. VP -> v[aux] v[past]
      vp-vaux-past-intransitive

      ;; doesn't work yet.
;      vp-vaux-past-transitive

      )))))

;; TODO: move to somewhere else that uses both grammar and lexicon (e.g. quiz or workbook): grammar itself should not depend on lexicon (lex/lexicon).
(defn random-sentence []
  (let [result
        (first (take 1 (sentences)))]
    result))

(defn random-sentences [n]
  (repeatedly n (fn [] (random-sentence))))

(defn speed-test [ & times]
  "TODO: show benchmark results and statistics (min,max,95%tile,stddev,etc)"
  (let [times (if times times 3)]
    (fo (take times (repeatedly #(time (random-sentence)))))))

(defmacro rewrite-as [name value]
  (if (ns-resolve *ns* (symbol (str name)))
    `(def ~name (cons ~value ~name))
    `(def ~name (list ~value))))

;; undefine any previous values.
(ns-unmap 'italianverbs.grammar 'declarative-sentence)
(ns-unmap 'italianverbs.grammar 'np)
(ns-unmap 'italianverbs.grammar 'vp)


;; -- define rewrite rules --
(rewrite-as declarative-sentence {:schema 'cc10
                                  :comp 'np
                                  :head 'vp})
(rewrite-as np {:schema 'cc10
                :comp 'dets
                :head 'common-nouns})

(rewrite-as np 'propernouns)
(rewrite-as np 'pronouns)

(rewrite-as vp 'intransitive-verbs)
(rewrite-as vp {:schema 'hh21
                :comp 'np
                :head 'transitive-verbs})
(rewrite-as vp {:schema 'ch21
                :comp 'pronouns
                :head 'transitive-verbs})

;; -- aliases --
(def ds declarative-sentence)

(log/info "done loading grammar.")

(defn gen-all [ alternatives & [filter-against filter-fn]]
  (if (first alternatives)
    (let [first-alt (first alternatives)]
      (let [filter-fn (if filter-fn
                        filter-fn
                        (if filter-against
                          ;; create a function using the filter-against we were given.
                          (fn [x]
                            (let [debug (log/debug (str "filtering: " x))
                                  debug (log/debug (str "against: " filter-against))
                                  result (unify x filter-against)
                                  debug (log/debug (str "result: " result))]
                              (not (unify/fail? result))))

                          ;; no filter was desired by the caller: just use the pass-through filter.
                          (fn [x] true)))]
        (lazy-cat
         (let [lazy-returned-sequence
               (cond (symbol? first-alt)
                     (lazy-shuffle
                      (filter filter-fn (eval first-alt)))

                     (and (map? first-alt)
                          (not (nil? (:schema first-alt))))
                     (let [schema (:schema first-alt)
                           head (:head first-alt)
                           comp (:comp first-alt)]
                       (log/info (str "schema: " schema))
                       (gen15 (eval schema)
                              (filter filter-fn
                                      (gen-all (lazy-shuffle (eval head))))
                              ;; TODO: comp filter should use a function of the head's subcat value for
                              ;; the filter.
                              (gen-all (if (symbol? comp) (lazy-shuffle (eval comp)) (lazy-shuffle comp)))))

                     (map? first-alt)
                     (list first-alt)

                     true (throw (Exception. "don't know what to do with this; type=" (type first-alt))))]
           lazy-returned-sequence)
         (gen-all (rest alternatives) filter-against filter-fn))))))


;; these work:
(defn speed-test2 []
  (do
    (time (fo (take 1 (gen-all (shuffle np)))))
    (time (fo (take 1 (gen-all (shuffle vp)))))
    (time (fo (take 1 (gen-all (shuffle ds)))))))

(defn speed-test3 [ & times]
  "TODO: show benchmark results and statistics (min,max,95%tile,stddev,etc)"
  (let [times (if times times 3)]
    (list
     (fo (take times (repeatedly #(time (take 1 (gen-all (shuffle np)))))))
     (fo (take times (repeatedly #(time (take 1 (gen-all (shuffle vp)))))))
     (fo (take times (repeatedly #(time (take 1 (gen-all (shuffle ds))))))))))


