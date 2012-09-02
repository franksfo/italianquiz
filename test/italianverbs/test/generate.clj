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
        (let [cat (ref :noun)
              head (ref {:cat cat
                         :subcat :top})
              comp (ref :top)]
          {:head head
           :comp comp
           :cat cat
           :a comp
           :b head})]
    (list np)))

(defn read-off-italian [expression]
  (if (not (nil? (fs/get-in expression '(:italian))))
    (fs/get-in expression '(:italian))
    (map (fn [child]
           (read-off-italian child))
         (list (fs/get-in expression '(:a))
               (fs/get-in expression '(:b))))))

(defn generate-np [rules lexicon head]
  (let [rules ;; filter for rules for NPs.
        (filter (fn [rule] (= (fs/get-in rule '(:head :cat)) :noun))
                rules)]
    (let [rule (nth rules (rand-int (.size rules)))]
      (let [head-lexemes ;; filter for lexemes that can be a head of a NP.
            (if (not (nil? head)) (list head)
                (seq (search/query-with-lexicon (set lexicon) (list {:subcat :top}))))]
        (let [head (nth head-lexemes (rand-int (.size head-lexemes)))]
          (let [comps (seq (search/query-with-lexicon (set lexicon)
                             (list (fs/get-in head '(:subcat)))))]
            (let [comp (nth comps (rand-int (.size comps)))]
              (fs/unify (fs/copy rule) {:comp comp :head head}))))))))

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
              (let [unified (fs/unify (fs/copy rule) {:comp comp :head head})]
                (= (read-off-italian unified) '("il" "compito"))
                (printfs (list head comp unified) "np-1.html")))))))))

(def vp-1-rules
  (concat
   np-1-rules
   (let [cat (ref :verb)
         comp (ref {:cat :top})
         subcat1 (ref {:cat :top})
         head (ref {:cat cat
                    :subcat comp
                    :subcat1 subcat1})]
     (list
      {:cat cat
       :subcat subcat1
       :head head
       :comp comp
       :a head
       :b comp}))))

(def vp-1-lexicon
  (concat
   np-1-lexicon
   (list
    {:cat :verb
     :italian "fare"
     :english "do"
     :subcat {:cat :noun}
     :subcat1 {:human true
               :cat :noun}})))

(defn generate-vp [rules lexicon head]
  (let [rules ;; filter for rules for VPs.
        (filter (fn [rule] (= (fs/get-in rule '(:head :cat)) :verb))
                rules)]
    (let [rule (nth rules (rand-int (.size rules)))]
      (let [head-lexemes ;; filter by both rule's :head and head param (either may be nil)
            (seq (search/query-with-lexicon (set lexicon)
                   (concat
                    (list (fs/get-in rule '(:head)))
                    (list head))))]
        (let [head (nth head-lexemes (rand-int (.size head-lexemes)))]
          (let [lexical-comps
                (seq (search/query-with-lexicon (set lexicon)
                       (list (fs/get-in head '(:subcat)))))
                lexical-comp
                (nth lexical-comps (rand-int (.size lexical-comps)))]
            (let [np (generate-np vp-1-rules lexicon lexical-comp)
                  unified
                  (fs/unify
                   (fs/copy rule) {:head head :comp np})]
            (merge
             {:italian (join (flatten (read-off-italian unified)) " ")}
             unified))))))))

(deftest vp-1
  "generate a vp (transitive verb+np)"
  (let [rules vp-1-rules
        lexicon vp-1-lexicon]
    (let [rules (filter (fn [rule]
                          (= (fs/get-in rule '(:head :cat)) :verb))
                        rules)
          rule (nth rules (rand-int (.size rules)))]
      (= (not (nil? rule)))
      (let [head-lexemes
            (seq (search/query-with-lexicon (set lexicon)
                   (list (fs/get-in rule '(:head)))))]
        (let [head (nth head-lexemes (rand-int (.size head-lexemes)))]
          (= (not (nil? head)))
          (let [lexical-comps
                (seq (search/query-with-lexicon (set lexicon)
                       (list (fs/get-in head '(:subcat)))))
                lexical-comp
                (nth lexical-comps (rand-int (.size lexical-comps)))]
            (let [np (generate-np np-1-rules lexicon lexical-comp)
                  unified
                  (fs/unify
                   (fs/copy rule) {:head head :comp np})]
              (is (= (read-off-italian unified) '("fare" ("il" "compito"))))
              (printfs (list rule head np
                             (merge
                              {:italian (join (flatten (read-off-italian unified)) " ")}
                              unified)) "vp-1.html"))))))))

(def sentence-rules
  (concat
   vp-1-rules
   (let [cat (ref :verb)
         comp (ref {:cat :top})
         head (ref {:cat cat
                    :subcat comp})]
     (list
      {:cat cat
       :subcat :nil!
       :head head
       :comp comp
       :a comp
       :b head}))))
   
(def sentence-lexicon
  (concat
   vp-1-lexicon
   (list {:cat :noun
          :human true
          :subcat :nil!
          :italian "io"
          :english "i"})))

(defn generate-sentence []
  "generate a sentence (subject+vp)"
  (let [rules sentence-rules
        lexicon sentence-lexicon]
    (let [rules (filter (fn [rule] (= (fs/get-in rule '(:subcat)) :nil!))
                              rules)
          rule (nth rules (.size rules))]
      (let [head-lexemes
            (seq (search/query-with-lexicon (set lexicon)
                   (list (fs/get-in rule '(:head)))))]
        (let [head (nth head-lexemes (rand-int (.size head-lexemes)))]
          (let [vp (generate-vp vp-1-rules vp-1-lexicon head)
                subjects
                (seq (search/query-with-lexicon (set lexicon)
                       (list (fs/get-in vp '(:subcat)))))]
            (let [subject (nth subjects (rand-int (.size subjects)))
                  unified (fs/unify
                           (fs/copy rule) {:head vp :comp subject})]
              unified)))))))

(deftest sentence-1
  "generate a sentence (subject+vp)"
  (let [rules sentence-rules
        lexicon sentence-lexicon]
    (let [rules (filter (fn [rule] (= (fs/get-in rule '(:subcat)) :nil!))
                              rules)
          rule (nth rules (rand-int (.size rules)))]
      (is (not (nil? rule)))
      (let [head-lexemes
            (seq (search/query-with-lexicon (set lexicon)
                   (list (fs/get-in rule '(:head)))))]
        (let [head (nth head-lexemes (rand-int (.size head-lexemes)))]
          (is (not (nil? head)))
          (let [vp (generate-vp vp-1-rules vp-1-lexicon head)
                subjects
                (seq (search/query-with-lexicon (set lexicon)
                       (list (fs/get-in vp '(:subcat)))))]
            (is (not (nil? subjects)))
            (is (> (.size subjects) 0))
            (let [subject (nth subjects (rand-int (.size subjects)))
                  unified (fs/unify
                           (fs/copy rule) {:head vp :comp subject})]
              (printfs (list rule head vp
                             (merge
                              {:italian (join (flatten (read-off-italian unified)) " ")}
                              unified)) "sentence-1.html"))))))))

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


          
              
        
