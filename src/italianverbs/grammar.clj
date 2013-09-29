(ns italianverbs.grammar
  (:refer-clojure :exclude [get-in resolve])
  (:use [clojure.set :only (union intersection)]
        [clojure.core :exclude (get-in resolve merge)]
        [italianverbs.generate :only (generate moreover-head moreover-comp gen14 over3 rewrite-as lazy-shuffle)]
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

(def aux-verbs
  (filter (fn [candidate]
            (and (not (= :notfound (unify/get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                 (= (unify/get-in candidate '(:synsem :cat)) :verb)
                 (= (unify/get-in candidate '(:synsem :aux)) true)))
          (lazy-shuffle hh21-heads)))

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
(rewrite-as vp {:schema 'hh21
                :head 'aux-verbs
                :comp 'intransitive-verbs})

;; -- aliases --
(def ds declarative-sentence)

(log/info "done loading grammar.")

(defn sentences []
  (gen-all (shuffle declarative-sentence)))

(defn random-sentence []
  (take 1 (gen-all (shuffle declarative-sentence))))

(defn random-sentences [n]
  (repeatedly n (fn [] (random-sentence))))

(defn speed-test [ & times]
  "TODO: show benchmark results and statistics (min,max,95%tile,stddev,etc)"
  (let [times (if times times 3)]
    (fo (take times (repeatedly #(time (random-sentence)))))))

(defn speed-test2 []
  (do
    (time (fo (take 1 (gen-all (shuffle np)))))
    (time (fo (take 1 (gen-all (shuffle vp)))))
    (time (fo (take 1 (gen-all (shuffle declarative-sentence)))))))

(defn speed-test3 [ & times]
  "TODO: show benchmark results and statistics (min,max,95%tile,stddev,etc)"
  (let [times (if times times 10)]
    (list
     (fo (take times (repeatedly #(time (take 1 (gen-all (shuffle np)))))))
     (fo (take times (repeatedly #(time (take 1 (gen-all (shuffle vp)))))))
     (fo (take times (repeatedly #(time (take 1 (gen-all (shuffle declarative-sentence))))))))))

