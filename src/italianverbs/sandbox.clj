(ns italianverbs.sandbox
  [:use
   [clojure.core :exclude [find]]
   [italianverbs.lexicon]
   ;; Prohibit generate/printfs because it writes directly to the filesystem:
   ;; attacker could DOS server by filling up filesystem.
   [italianverbs.generate :exclude [printfs]]
   [italianverbs.grammar]
   [italianverbs.morphology]
   [clojail.core :only [sandbox]]
   [clojail.testers]
   ]
  [:require
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.unify :as fs]
   [italianverbs.html :as html]
   [clojure.set :as set]
   [italianverbs.test.generate :as tgen]
   [clojure.string :as string]
   [clojure.tools.logging :as log]])

;; Sandbox specification derived from:
;;    https://github.com/flatland/clojail/blob/4d3f58f69c2d22f0df9f0b843c7dea0c6a0a5cd1/src/clojail/testers.clj#L76
;;    http://docs.oracle.com/javase/6/docs/api/overview-summary.html
;;    http://richhickey.github.com/clojure/api-index.html

(def workbook-sandbox
  (sandbox
   (conj
    clojail.testers/secure-tester-without-def
    (blacklist-nses '[clojure.main
                      java
                      javax
                      org.omg
                      org.w3c
                      org.xml
                      ])
    (blacklist-objects [clojure.lang.Compiler
                        clojure.lang.Ref
                        clojure.lang.Reflector
                        clojure.lang.Namespace
                        clojure.lang.Var clojure.lang.RT]))
   ;; TODO: make this configurable:
   ;;   might want to have a value for production usage lower/higher than
   ;;   for development usage.
   :timeout 1000000 ;; for development, set high (1000 seconds)
   :namespace 'italianverbs.sandbox))

(defn sandbox-load-string [expression]
  (workbook-sandbox (read-string expression)))

(defn show-lexicon []
  (map (fn [entry]
         (let [inflection
               (fs/get-in entry '(:synsem :infl))
               italian
               (fs/get-in entry '(:italian))
               italian-infinitive
               (fs/get-in entry '(:italian :infinitive))
               italian-infinitive-infinitive
               (fs/get-in entry
                          '(:italian :infinitive :infinitive))]
           (merge entry
                  {:header
                   (cond
                    (= inflection :present)
                    (str
                     (if (string? italian-infinitive)
                       italian-infinitive
                       italian-infinitive-infinitive)
                     " (present)")
                    italian-infinitive
                    (str italian-infinitive " (infinitive)")
                    italian-infinitive-infinitive
                    (str italian-infinitive-infinitive " (present)")
                    :else italian)})))
       lexicon))

;;;workbook examples:

;;;(dotimes [n 10] (def foo (time (args1-cached "mangiare"))))

(if false
  (map (fn [x] {:it (fs/get-in x '(:italian))
                :it-inf (fs/get-in x '(:italian :infinitive))})
       lexicon)

  )
(if false
  (do
    (take-last 3 (take 3 (show-lexicon)))
    (take-last 3 (take 6 (show-lexicon)))
    (take-last 3 (take 9 (show-lexicon)))
))
;;
;;

;; find semantic implicatures of "cane (dog)"
(if false
  (sem-impl (fs/get-in (it "cane") '(:synsem :sem))))

;(take 10 (repeatedly #(fo (take 1 (generate s-present)))))

;(fo (take 1 (over2 s-present (shuffle nominative-pronouns) (shuffle intransitive-verbs))))

;(def skel (over2 s-present (over2 np :top (over2 nbar :top :top)) (over2 vp :top (over2 np :top :top))))

(if false
  (do
(fo
 (take 1
       (over2 np
              (take 4 (shuffle determiners))
              (over2 nbar
                     (take 1 (shuffle nouns))
                     (take 4 (shuffle adjectives))))))

(fo
 (take 1
       (over2 np
              (shuffle determiners)
              (over2 nbar
                     (take 1 (shuffle nouns))
                     (shuffle adjectives)))))

(fo
 (take 1
       (over2 np
              (shuffle determiners)
              (over2 nbar
                     (take 5 (shuffle nouns))
                     (shuffle adjectives)))))

(fo
 (take 1

       (over2 s-present
              (over2 np
                     (shuffle determiners)
                     (over2 nbar
                            (take 5 (shuffle nouns))
                            (shuffle adjectives)))
              (shuffle intransitive-verbs))))
))


42

