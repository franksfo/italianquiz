(ns italianverbs.test.lexiconfn
  (:use [rdutest] ;; transitional
        [clojure.test]
        [italianverbs.lexiconfn])
  (:require
   [italianverbs.fs :as fs]))

;; TODO: for testing, consider some kind of mongodb mocking of some kind,
;; so that no actual mongodb connection would be necessary.

(deftest foo
  (is (= true true)))

(def tests
  (list
   (rdutest
    "A lexical entry for the word: 'parlare'."
    (let [verb {:cat :verb}
          human-subj {:subj {:human true}}
          third-sing {:subj {:number :singular :person :3rd}}
          parlare (add "parlare" "to speak"
                       verb
                       human-subj
                       {
                        :obj {:speakable true}
                        })
          merge (fs/merge
                 parlare
                 third-sing
                 {:root parlare})
          parla (add "parla" "speaks"
                     parlare
                     third-sing
                     {:root parlare})]
      {:merge merge
       :parla parla})
    (fn [merge-and-parla]
      (let [merge (:merge merge-and-parla)
            parla (:parla merge-and-parla)]
        (and
         (= (:cat parla) :verb)
         (= (get-in parla (list :subj :human)) true)
                                        ;         (= (fs/get-path parla (list :root :subj :human)) true)
                                        ;         (= (fs/get-path parla (list :subj :number)) :singular)
                                        ;         (= (fs/get-path parla (list :subj :person)) :3rd)
                                        ;         (= (fs/get-path parla (list :obj :speakable)) true)
                                        ;         (= (:english parla) "speaks")
                                        ;         (= (:italian parla) "parla")
         )))
    :parla)
   (rdutest
    "calcio (no complement for this noun)"
    (let [third-sing {:number :singular
                      :person :3rd
                      :cat :noun}
          common-noun {:comp {:cat :det
                              :number (:number third-sing);; TODO: use reference.
                              }
                       }
          masc {:gender :masc}
          mass {:mass true :comp {:def {:not :indef}}} ; you can say 'the pasta', but not 'a pasta'.
          nil-complement {:comp :nil! :sport true}
          featuremaps (list common-noun masc mass nil-complement)]
      {
       :copied-list (concat (map #'fs/copy featuremaps))
       :with-apply (apply fs/merge
                     (concat (map #'fs/copy featuremaps)))
       })
    (fn [result]
      (or (nil? (:comp result))
          (= {} (:comp result))))
    :lexiconfn-calcio)

   (rdutest
    "order of arguments containing references should not matter."
    (let [agr (ref :top)]
      (add "letto1" "bed1"
           {:gender :masc}
           {:gender agr}))
    (fn [result]
      (and (= (type (:gender result)) clojure.lang.Ref)
           (= @(:gender result) :masc)))
    :argument-order-of-add)

))

;(def parla
;  (:test-result ((:3s tests) tests)))

;;usage : (run-query (pathify trans-verbs)))


