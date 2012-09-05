(ns italianverbs.test.generate
  (:use [clojure.test]
        [italianverbs.generate])
  (:require
   [somnium.congomongo :as mongo]
   [italianverbs.fs :as fs]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.search :as search]))

;; TODO:
;; 1. use fixtures and compose-fixtures rather than defining
;; per-test lexicons and grammars

;; 2. compose tests where appropriate rather than separate functions
;; e.g. (generate-np), (generate-vp), etc.
;;

;; see: http://richhickey.github.com/clojure/clojure.test-api.html
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

(defn read-off-italian [expression]
  (if (not (nil? (fs/get-in expression '(:italian))))
    (fs/get-in expression '(:italian))
    (map (fn [child]
           (read-off-italian child))
         (list (fs/get-in expression '(:a))
               (fs/get-in expression '(:b))))))

(defn random [members]
  "return a randomly-selected member of the set members."
  (nth members (rand-int (.size members))))

(defn get-rules [rules filters-of-path-value]
  (if (> (.size filters-of-path-value) 0)
    (let [filter-of-path-value (first filters-of-path-value)
          path (first filter-of-path-value)
          value (second filter-of-path-value)]
      (get-rules
       (filter (fn [rule]
                 (= (fs/get-in rule path) value))
               rules)
       (rest filters-of-path-value)))
    rules))

(defn random-rule [rules [& filters-of-path-value] ]
  "pick a rule at random from the subset of rules that matches the provided filters."
  (random (get-rules rules (list filters-of-path-value))))

(defn generate-np [rules lexicon head]
  "generate a noun phrase from the supplied rules, lexicon,
   and optional _head_:a filter on the lexicon to find the head noun."
  (let [rule (random-rule rules '((:head :cat) :noun))]
    (let [head (random
                (if (not (nil? head)) (list head)
                    (seq (search/query-with-lexicon lexicon {:subcat :top}))))
          comp (random (seq (search/query-with-lexicon lexicon
                              (fs/get-in head '(:subcat)))))]
      (fs/unify (fs/copy rule) (fs/copy {:comp comp :head head})))))

(def np-1-rules 
  (let [np ;; NP -> Comp Head
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

(deftest np-1
  "generate a noun phrase."
  (let [np (generate-np np-1-rules np-1-lexicon nil)]
    (= (read-off-italian np) '("il" "compito"))
    (printfs np "np-1.html")))

(def vp-1-rules
  (concat
   np-1-rules
   (let [cat (ref :verb)
         comp (ref {:cat :top})
         subj (ref {:cat :top})
         head (ref {:cat cat
                    :subcat comp
                    :subj subj})]
     (list
      {:cat cat ;; VP -> Head Comp
       :subcat subj ;; now that we have an object, subject is the new subcat.
       :head head
       :comp comp
       :a head
       :b comp}))))

(def vp-1-lexicon
  (concat
   np-1-lexicon
   (list
    (let [person (ref :top)
          number (ref :top)]
      {:cat :verb
       :italian "facio"
       :root {:italian "fare"}
       :english "do"
       :subcat {:cat :noun}
       :subj {:human true
              :person person
              :number number
              :cat :noun}}))))

(defn generate-vp [rules lexicon head]
  (let [rule (random-rule rules '((:head :cat) :verb))
        head
        (random ;; filter by both rule's :head and head param (either may be nil)
         (seq (apply
               search/query-with-lexicon
               (concat
                (list lexicon)
                (list (fs/get-in rule '(:head))
                      head)))))
        lexical-comps
        (seq (search/query-with-lexicon lexicon
               (fs/get-in head '(:subcat))))
        lexical-comp
        (nth lexical-comps (rand-int (.size lexical-comps)))
        np (generate-np vp-1-rules lexicon lexical-comp)]
    (fs/unify
     (fs/copy rule) {:head (fs/copy head) :comp np})))

(deftest vp-rules
  "find a suitable vp rule."
  (let [rule (random-rule vp-1-rules '((:head :cat) :verb))]
    (is (not (nil? rule)))))

(deftest vp-find-head
  "find a lexeme that can be the head of a verb-phrase."
  (let [rule (random-rule vp-1-rules '((:head :cat) :verb))]
    (let [head
          (random ;; filter by both rule's :head and head param (either may be nil)
           (seq (search/query-with-lexicon (set vp-1-lexicon)
                  (fs/get-in rule '(:head)))))]
      (is (not (nil? head))))))

(deftest vp-1
  "generate a vp (transitive verb+np)"
  (let [unified (generate-vp vp-1-rules vp-1-lexicon nil)]
    (is (= (read-off-italian unified) '("facio" ("il" "compito"))))
    (printfs
     (merge
      {:italian (join (flatten (read-off-italian unified)) " ")}
      unified)
     "vp-1.html")))

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
          :person :1st
          :number :sing
          :subcat :nil!
          :italian "io"})))

(defn generate-sentence [rules lexicon]
  "generate a sentence (subject+vp)"
  ;; sentential rule: one whose subcat is nil!: meaning it takes no
  ;; subcategorizations (arguments) because it's a complete sentence.
  (let [rule (random-rule rules '((:subcat) :nil!))]
    (let [head-lexemes
          (seq (search/query-with-lexicon lexicon
                 (fs/get-in rule '(:head))))]
      (let [head (random head-lexemes)]
        (let [vp (generate-vp vp-1-rules vp-1-lexicon head)
              subjects
              (seq (search/query-with-lexicon lexicon
                     (fs/get-in vp '(:subcat))))]
          (let [subject (nth subjects (rand-int (.size subjects)))
                unified (fs/unify
                         (fs/copy rule) {:head vp :comp subject})]
            unified))))))
                
(deftest sentence-1
  "generate a sentence (subject+vp)"
  (let [unified (generate-sentence sentence-rules sentence-lexicon)]
    (is (= (read-off-italian unified) '("io" ("facio" ("il" "compito")))))
    (printfs (merge
              {:italian (join (flatten (read-off-italian unified)) " ")}
              unified) "sentence-1.html")))

(def sentence-lexicon-with-exceptions
  (concat
   sentence-lexicon))

(deftest inflection-via-unification-exception
  "implement inflection via unification - exceptional case: e.g. 'io facio'"
  (let [lexicon sentence-lexicon-with-exceptions
        facio (first (search/query-with-lexicon lexicon
                      '{:italian "facio"}))]
    (is (= (fs/get-in facio '(:italian)) "facio"))
    (is (= (fs/get-in facio '(:root :italian)) "fare"))))

(deftest inflection-via-unification-regular
  "implement inflection via unification - exceptional case: e.g. 'io parlo'"
  (let [lexicon sentence-lexicon]
    (is true)))



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
        lexicon (let [facio {:infl :infinitive
                            :italian "facio"
                            :passato-prossimo-aux {:infl :infinitive
                                                   :italian "avere"}}]
                  (list
                   {:infl :infinitive
                    :italian "avere"}
                   facio
                   (let [ref (ref {:italian "fatto"})]
                     {:italian ref
                      :root (fs/merge
                             facio
                             {:passato-prossimo ref})})
                   {:infl :infinitive
                    :italian "lavorare"
                    :passato-prossimo-aux {:infl :infinitive
                                           :italian "avere"}}))]  ;; end of lexicon
    ;; 1. choose rule at random.
    (let [rule (nth rules (rand-int (.size rules)))
          search-a (seq (search/query-with-lexicon lexicon (fs/get-in rule '(:a))))
          search-b (seq (search/query-with-lexicon lexicon (fs/get-in rule '(:b :root))))
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

