(ns italianverbs.test.espanol
  (:refer-clojure :exclude [get get-in lookup merge resolve])
  (:require
   [clojure.test :refer :all]
   [italianverbs.cache :refer (build-lex-sch-cache create-index over spec-to-phrases)]
   [italianverbs.engine :as engine]
   [italianverbs.forest :as forest]
   [italianverbs.espanol :as es]
   [italianverbs.grammar.espanol :as gram]
   [italianverbs.lexicon.espanol :as lex]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.over :refer (overc overh)]
   [italianverbs.ug :refer :all]
   [italianverbs.unify :refer :all]
   ))

(def test-model
  (let [lexicon
        (into {}
              (for [[k v] @es/lexicon]
                (let [filtered-v
                      (filter #(or (= (get-in % [:synsem :sem :pred]) :mangiare)
                                   (and (= (get-in % [:synsem :cat]) :det)
                                        (= (get-in % [:synsem :def]) :def)
                                        (= (get-in % [:synsem :number]) :sing)
                                        )
                                   (= (get-in % [:synsem :sem :pred]) :donna)
                                   (= (get-in % [:synsem :sem :pred]) :io)
                                   (= (get-in % [:synsem :sem :pred]) :pane))
                              v)]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))
        grammar
        (filter #(or (= (:rule %) "vp-present")
                     (= (:rule %) "s-present")
                     (= (:rule %) "noun-phrase1"))
                es/grammar)]

    {:enrich es/enrich
     :grammar grammar
     :lexicon lexicon
     :index (create-index grammar (flatten (vals lexicon)) head-principle)}))

(def el-pan
  (over (:grammar test-model) (es/es "el") (es/es "pan")))

(def comer-vp (over (:grammar test-model) (es/es "comer")))

(def spec-with-yo {:head {:phrasal true}
                   :synsem {:subcat '()
                            :sem {:pred :mangiare
                                  :subj {:pred :io}
                                  :obj {:pred :pane}}}})

(def spec-with-mujer {:head {:phrasal true}
                      :synsem {:subcat '()
                               :sem {:pred :mangiare
                                     :subj {:pred :donna
                                            :number :sing}
                                     :obj {:pred :pane}}}})

(deftest comer-il-pane
  (let [vp (overc comer-vp el-pan)]
    (is (not (empty? vp)))
    (is (= (fo (first vp)) "comer el pan"))))

(deftest yo-como-el-pan
  (let [generated (engine/generate spec-with-yo
                                   test-model)]
    (is (= (fo generated) "yo como el pan"))))

(deftest la-mujer-come-el-pan
  (let [generated (engine/generate spec-with-mujer
                                   test-model)]
    (is (= (fo generated) "la mujer come el pan"))))

