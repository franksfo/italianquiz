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

(defn gen-np [use-filter]
  (do
    (log/debug "base-cc10-random: start: filtering cc10 heads.")
    (gen15 cc10
           (filter use-filter
                   (lazy-shuffle common-nouns))
           (lazy-shuffle cc10-comps))))

(defn gen-the-np [expansion use-filter]
  (let [schema (:schema expansion)
        heads (:head expansion)
        comps (:comp expansion)]
    (gen15 schema
           (filter use-filter
                   (lazy-shuffle heads))
           (lazy-shuffle cc10-comps))))

(def np-to-det-n
  (fn [filter]
    (do
      (log/debug "looking for nouns..")
      (lazy-seq (gen-np (merge filter))))))

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

(defn nps []
  (lazy-shuffle (list np-to-det-n
                      (lazy-shuffle propernouns-and-pronouns)
                      )))

(def vp-v
  (fn [] (lazy-shuffle intransitive-verbs)))

(defn vp-to-v-np [v nps]
  (if (first nps)
    (lazy-cat (gen-hh21 v (first nps))
              (vp-to-v-np v (rest nps)))))

(def vp-v-np
  (fn []
    (vp-to-v-np
     (lazy-shuffle transitive-verbs)
     (nps)))) ;; Object NP

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

(def vp-vaux-past-transitive
  (fn []
    (gen-hh21
     ;; v[aux]
     (filter (fn [candidate]
               (and (not (= :notfound (unify/get-in candidate '(:synsem :subcat :2 :cat) :notfound)))
                    (= (unify/get-in candidate '(:synsem :cat)) :verb)
                    (= (unify/get-in candidate '(:synsem :aux)) true)))
             (lazy-shuffle hh21-heads))

     ;; vp[past] -> v[past] np
     vp-v-np)))

(def vp-vaux-x
  (lazy-shuffle (list vp-vaux-past-intransitive
                      vp-vaux-past-transitive)))

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
    (shuffle (list np-to-det-n
                   (lazy-shuffle propernouns-and-pronouns)))

    ;; VP: 4 expansions:
    (shuffle
     (list

      ;; 1. VP -> V
      vp-v

      ;; 2. VP -> V NP
      vp-v-np

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

(defn gen [rule]
  (log/info (str "gen: rule:" rule))
  (log/info (str "gen: rule type:" (type rule)))
  (log/info (str "gen: rule type:" (type rule)))
  (cond (seq? rule)
        (let [expansions (lazy-shuffle rule)]
          (log/info (str "first expansion:" (first expansions)))

          (lazy-seq
           ;; parent: S -> NP VP
           (s-to-np-vp

            ;; subject NP.
            (shuffle (list np-to-det-n))

            ;; VP: 4 expansions:
            (shuffle
             (list
              vp-v)))))
        true
        (log/error "no idea what to do with left side: its type is: " (type rule))))

(defmacro rewrite-as [name value]
  (if (ns-resolve *ns* (symbol (str name)))
    `(def ~name (cons ~value ~name))
    `(def ~name (list ~value))))

;; undefine any previous values.
(ns-unmap 'italianverbs.grammar 'declarative-sentence)
(ns-unmap 'italianverbs.grammar 'np)
(ns-unmap 'italianverbs.grammar 'vp)


;; define rewrite rules.
(rewrite-as declarative-sentence {:schema 'cc10
                                  :comp 'np
                                  :head 'vp})
(rewrite-as np {:schema 'cc10
                :comp 'det
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

;; aliases
(def ds declarative-sentence)

(log/info "done loading grammar.")

(defn gen-all [ alternatives & [filter-against filter-fn]]
  (if (> (.size alternatives) 0)
    (let [first-alt (first alternatives)
          ;; TODO: we are creating this function everytime: instead, just create and pass.
          filter-fn (if filter-fn
                      filter-fn
                      (if filter-against
                        (fn [x]
                          (let [debug (log/info (str "filtering: " x))
                                debug (log/info (str "against: " filter-against))
                                result (unify x filter-against)
                                debug (log/info (str "result: " result))]
                            (not (unify/fail? result))))
                        (fn [x] true)))]
      (lazy-cat
       (let [lazy-returned-sequence
             (cond (symbol? first-alt)
                   (lazy-shuffle
                    (filter filter-fn (eval first-alt)))

                   (map? first-alt)
                   nil



                   true (throw (Exception. "don't know what to do with this; type=" (type first-alt))))]
         lazy-returned-sequence)
       (gen-all (rest alternatives) filter-against filter-fn)))))
