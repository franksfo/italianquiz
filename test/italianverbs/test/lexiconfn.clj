(ns italianverbs.test.lexiconfn
  (:use [clojure.test]
        [italianverbs.lexiconfn])
  (:require
   [italianverbs.fs :as fs]))

;; TODO: for testing, consider some kind of mongodb mocking of some kind,
;; so that no actual mongodb connection would be necessary.

(deftest parlare ;; A lexical entry for the word: 'parlare'
  (let [merge-and-parla
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
           :parla parla})]
    (let [merge (:merge merge-and-parla)
          parla (:parla merge-and-parla)]
      (is (= (:cat parla) :verb))
      (is (= (get-in parla (list :subj :human)) true)
                                        ;         (= (fs/get-path parla (list :root :subj :human)) true)
                                        ;         (= (fs/get-path parla (list :subj :number)) :singular)
                                        ;         (= (fs/get-path parla (list :subj :person)) :3rd)
                                        ;         (= (fs/get-path parla (list :obj :speakable)) true)
                                        ;         (= (:english parla) "speaks")
                                        ;         (= (:italian parla) "parla")
          ))))

(deftest calcio ;; No article for the noun "calcio" (soccer).
  (let [result
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
           })]
    (is (or (nil? (:comp result))
            (= {} (:comp result))))))

(deftest order-independence ;; Order of arguments containing references should not matter.
  (let [result
        (let [agr (ref :top)]
          (add "letto1" "bed1"
               {:gender :masc}
               {:gender agr}))]
    (is (= (type (:gender result)) clojure.lang.Ref))
    (is (= @(:gender result) :masc))))


