(ns italianverbs.test.italiano
  (:refer-clojure :exclude [get get-in lookup merge resolve])
  (:require
   [clojure.test :refer :all]
   [italianverbs.cache :refer (build-lex-sch-cache create-index over spec-to-phrases)]
   [italianverbs.engine :as engine]
   [italianverbs.forest :as forest]
   [italianverbs.italiano :refer :all]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.over :refer (overc overh)]
   [italianverbs.ug :refer :all]
   [italianverbs.unify :refer :all]
   ))

(def test-vp
  (let [lexicon
        (into {}
              (for [[k v] @lexicon]
                (let [filtered-v
                      (filter #(or (= (get-in % [:synsem :sem :pred]) :mangiare)
                                   (and (= (get-in % [:synsem :cat]) :det)
                                        (= (get-in % [:synsem :gender]) :masc)
                                        (= (get-in % [:synsem :def]) :def)
                                        (= (get-in % [:synsem :number]) :sing)
                                        )
                                   (= (get-in % [:synsem :sem :pred]) :pane)
                                   (= (get-in % [:synsem :sem :pred]) :io))
                              v)]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))
        grammar
        (filter #(or (= (:rule %) "vp-present")
                     (= (:rule %) "s-present")
                     (= (:rule %) "noun-phrase1"))
                grammar)]

    {:enrich enrich
     :grammar grammar
     :lexicon lexicon
     :index (create-index grammar (flatten (vals lexicon)) head-principle)}))


(def il-pane
  (over (:grammar test-vp) (it "il") (it "pane")))

(def mangiare-vp (over (:grammar test-vp) (it "mangiare")))

(def spec {:synsem {:subcat '()
                    :sem {:pred :mangiare
                          :subj {:pred :io}
                          :obj {:pred :pane}}}})

(deftest mangiare-il-pane
  (let [vp (overc mangiare-vp il-pane)]
    (is (not (empty? vp)))
    (is (= (fo (first vp)) "mangiare il pane"))))

(deftest mangiare-il-pane2
  (let [gen (forest/generate-all spec
                                 (:grammar test-vp)
                                 (:lexicon test-vp)
                                 (:index test-vp))]
    (is (not (empty? gen)))))

(deftest io-mangio-il-pane
  (let [generated (engine/generate spec
                                   test-vp)]
    (= generate "io mangio il pane")))






