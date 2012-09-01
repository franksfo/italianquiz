(ns italianverbs.test.generate
  (:use [clojure.test]
        [italianverbs.generate])
  (:require
   [somnium.congomongo :as mongo]
   [italianverbs.fs :as fs]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.search :as search]))

(deftest t1
  (let [ref3 (ref :top)
        ref2 (ref :top)
        ref4 (ref :infinitive)
        ref1 (ref {:infl ref4
                   :italian ref3})

        ;; irregular vp rule.
        irreg-vp
        {:a ref1
         :b {:italian ref2
             :root {:infl :infinitive
                    :passato-prossimo {:italian ref2}
                    ;; a) note: no italian here: compare to b) below.
                    :passato-prossimo-aux ref1}} 
         :italian {:a ref3
                   :b ref2}
         :infl ref4}

        ;; an irregular infinitive verb.
        fare 
        {:infl :infinitive
         :italian "fare"
         :passato-prossimo-aux {:infl :infinitive
                                :italian "avere"}
         :passato-prossimo {:italian "fatto"}}

        unified
        (fs/unify (fs/copy irreg-vp)
                  {:b {:root (fs/copy fare)}})]

    ;; TODO: more tests.
    (is (= (fs/get-in unified '(:a :italian)) "avere"))
    (printfs (list irreg-vp {:b {:root fare}} unified) "fare.html")))

(deftest t2
  (let [ref3 (ref :top)
        ref4 (ref :top)
        ref2 (ref {:passato-prossimo ref4})
        ref5 (ref :infinitive)
        ref1 (ref {:infl ref5
                   :italian ref3})

        ;; regular vp rule
        reg-vp
        {:a ref1
         :b {:italian ref2
             :infl :passato-prossimo
             :root {:infl :infinitive
                    ;; b) note: italian here.
                    :italian ref4 
                    :passato-prossimo-aux ref1}}
         :italian {:a ref3
                   :b ref2}
         :infl ref5}

        ;; an regular infinitive verb.
        lavorare
        {:infl :infinitive
         :italian "lavorare"
         :passato-prossimo-aux {:infl :infinitive
                                :italian "avere"}}

        unified
        (fs/unify reg-vp
                  {:b {:root lavorare}})]
    ;; TODO: more tests.
    (is (= (fs/get-in unified '(:a :italian)) "avere"))
    (printfs (list reg-vp {:b {:root lavorare}} unified) "lavorare.html")))

(def np-1-lexicon
  (let [compito
        {:cat :noun
         :subcat {:cat :det}
         :italian "compito"
         :english "homework"}
        il
        (let []
          {:cat :det
           :italian "il"
           :english "the"})]
    (list compito il)))

(def np-1-rules
  (let [np
        (let [head (ref {:subcat :top})
              comp (ref :top)]
          {:head head
           :comp comp
           :a comp
           :b head})]
    (list np)))

(deftest np-1
  "generate a noun phrase."
  (let [rules np-1-rules
        lexicon np-1-lexicon]
    (let [rule (nth rules (rand-int (.size rules)))]
      (is (not (nil? rule)))
      (let [head-lexemes (seq (search/query-with-lexicon (set lexicon) (list {:subcat :top})))]
        (is (not (nil? head-lexemes)))
        (let [head (nth head-lexemes (rand-int (.size head-lexemes)))]
          (is (not (nil? head)))
          (let [comps (seq (search/query-with-lexicon (set lexicon) (list (fs/get-in head '(:subcat)))))]
            (is (not (nil? comps)))
            (is (> (.size comps) 0))
            (let [comp (nth comps (rand-int (.size comps)))]
              (is (not (nil? comp)))
              (let [unified (fs/unify (fs/copy rule) {:comp comp :head head})
                    read-off-italian
                    (map (fn [child]
                           (fs/get-in child '(:italian)))
                         (list (fs/get-in unified '(:a))
                               (fs/get-in unified '(:b))))]
                (= read-off-italian '("il" "compito"))))))))))

(deftest t3
  (let [rules
        (let [passato-regular
              (let [ref1 (ref :top)
                    ref2 (ref :top)]
                {:italian {:passato-prossimo-fn ref1}
                 :infl :passato-prossimo
                 :passato-prossimo-aux ref2
                 :a {:italian ref1
                     :passato-prossimo-aux ref2
                     :infl :infinitive}})
              passato-irregular
              (let [ref1 (ref :top)
                    ref2 (ref :top)]
                {:italian ref1
                 :infl :passato-prossimo
                 :passato-prossimo-aux ref2
                 :a {:passato-prossimo ref1
                     :passato-prossimo-aux ref2
                     :infl :infinitive}})
              vp
              (let [ref1 (ref :top)
                    ref2 (ref :top)
                    ref3 (ref :top)]
                {:italian {:a ref1
                           :b ref2}
                 :infl ref3
                 :a {:italian ref1
                     :infl ref3}
                 :b {:italian ref2}})]
          (list passato-regular passato-irregular vp)) ;; end of rules
        lexicon (let [fare {:infl :infinitive
                            :italian "fare"
                            :passato-prossimo-aux {:infl :infinitive
                                                   :italian "avere"}}]
                  (list
                   {:infl :infinitive
                    :italian "avere"}
                   fare
                   (let [ref (ref {:italian "fatto"})]
                     {:italian ref
                      :root (fs/merge
                             fare
                             {:passato-prossimo ref})})
                   {:infl :infinitive
                    :italian "lavorare"
                    :passato-prossimo-aux {:infl :infinitive
                                           :italian "avere"}}))]  ;; end of lexicon
    ;; 1. choose rule at random.
    (let [rule (nth rules (rand-int (.size rules)))
          search-a (seq (search/query-with-lexicon (set lexicon) (list (fs/get-in rule '(:a)))))
          search-b (seq (search/query-with-lexicon (set lexicon) (list (fs/get-in rule '(:b :root)))))
          random-lexeme-a (if search-a (nth search-a (rand-int (.size search-a))))
          random-lexeme-b (if search-b (nth search-b (rand-int (.size search-b))))]
      (printfs
       rules
       "rules.html")
      (printfs
       lexicon
       "lexicon.html")
      (printfs
       (list rule random-lexeme-a random-lexeme-b)
       "random.html"))))


          
              
        
