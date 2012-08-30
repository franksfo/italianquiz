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


          
              
        
