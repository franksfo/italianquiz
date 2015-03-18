(ns italianverbs.test.italiano
  (:refer-clojure :exclude [get get-in lookup merge resolve])
  (:require
   [clojure.test :refer :all]
   [italianverbs.cache :refer (build-lex-sch-cache create-index over spec-to-phrases)]
   [italianverbs.engine :as engine]
   [italianverbs.forest :as forest]
   [italianverbs.italiano :as it]
   [italianverbs.lexiconfn :as lexiconfn]
   [italianverbs.morphology :refer (fo)]
   [italianverbs.over :refer (overc overh)]
   [italianverbs.ug :refer :all]
   [italianverbs.unify :refer :all]
   ))

(def test-grammar
  (let [lexicon
        (into {}
              (for [[k v] @it/lexicon]
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
                it/grammar)]

    {:enrich it/enrich
     :grammar grammar
     :lexicon lexicon
     :index (create-index grammar (flatten (vals lexicon)) head-principle)}))

(def il-pane
  (over (:grammar test-grammar) (it/it "il") (it/it "pane")))

(def mangiare-vp (over (:grammar test-grammar) (it/it "mangiare")))

(def spec-with-io {:synsem {:subcat '()
                            :sem {:pred :mangiare
                                  :subj {:pred :io}
                                  :obj {:pred :pane}}}})

(def spec-with-donna {:synsem {:subcat '()
                            :sem {:pred :mangiare
                                  :subj {:pred :donna
                                         :number :sing}
                                  :obj {:pred :pane}}}})

(deftest mangiare-il-pane
  (let [vp (overc mangiare-vp il-pane)]
    (is (not (empty? vp)))
    (is (= (fo (first vp)) "mangiare il pane"))))

(deftest io-mangio-il-pane
  (let [generated (engine/generate spec-with-io
                                   test-grammar)]
    (is (= (fo generated) "io mangio il pane"))))

(deftest la-donna-mangia-il-pane
  (let [generated (engine/generate spec-with-donna
                                   test-grammar)]
    (is (= (fo generated) "la donna mangia il pane"))))

;;(get-in (engine/generate {:synsem {:cat :noun :sem {:number :plur :pred :donna}}} it/medium) [:italiano :b])
