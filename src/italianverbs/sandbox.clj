(ns italianverbs.sandbox
  [:use
   [clojure.core :exclude [find]]
   [italianverbs.lexiconfn]
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
   [italianverbs.fs :as fs]
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

;;
;;

    (take-last 3 (take 15 (show-lexicon)))
    
    ;; get past tense form of "leggere":
    
    (lookup {:italian {:infinitive {:infinitive "leggere"}}
             :synsem {:infl :past}})
    
;; generate a complete vp-past:
    (let [letto (lookup {:italian {:infinitive {:infinitive "leggere"}}
                         :synsem {:infl :past}})]
      (over (over rules verbs) (over (over np "il") "libro")))
    
))
    
    

(if false
  (formattare
   (take-last
    5
    (take 100
          (over (over s-present lexicon)
                (over (over vp-present lexicon)
                      (over (over vp-past-avere lexicon)
                            (over (over np lexicon)
                                  lexicon))))))))

;; find semantic implicatures of "cane (dog)"
(if false
  (sem-impl (fs/get-in (it "cane") '(:synsem :sem))))

 ;(dotimes [n 200] (time (random-sentence)))

;;(take 20 (repeatedly #(fo (random-sentence))))


(def foo5 (over vp-present "andare" (over prep-phrase "a" (over np "il" "mare"))))
