(ns italianverbs.test.generate
  (:use [clojure.test]
        [italianverbs.generate])
  (:require
   [clojure.contrib.logging :as log]
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
  (let [debug (if false (println (str "read-off-italian: " expression)))]
    (if (not (nil? (fs/get-in expression '(:italian))))
      (fs/get-in expression '(:italian))
      (if (not (nil? (fs/get-in expression '(:a))))
        (map (fn [child]
               (read-off-italian child))
             (list (fs/get-in expression '(:a))
                   (fs/get-in expression '(:b))))))))
  
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

(defn random-rule [rules & filters-of-path-value]
  "pick a rule at random from the subset of rules that matches the provided filters."
  (random (get-rules rules filters-of-path-value)))

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
              comp (ref :top)
              artifact (ref :top)
              number (ref :top)
              gender (ref :top)
              head (ref {:cat cat
                         :number number
                         :gender gender
                         :artifact artifact
                         :subcat comp})]
          {:head head
           :comp comp
           :cat cat
           :artifact artifact
           :number number
           :gender gender
           :a comp
           :b head})]
    (list np)))

(def np-1-lexicon
  (let [masc (ref :masc)
        sing (ref :sing)
        compito
        {:cat :noun
         :number sing
         :gender masc
         :artifact true
         :subcat {:cat :det
                  :number sing
                  :gender masc}
         :italian "compito"
         :english "homework"}
        il
        (let []
          {:cat :det
           :gender :masc
           :number :sing
           :italian "il"
           :english "the"})]
    (list compito il
          {:cat :noun
           :number :sing
           :gender :masc
           :human true
           :italian "uomo"
           :english "man"}
          )))

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
                    :infl :present
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
  (let [fare {:cat :verb
              :infl :infinitive
              :italian "fare"
              :english "to do"
              :subcat {:cat :noun
                       :artifact true}
              :subj {:human true
                     :cat :noun}}
        root-subcat (ref :top)
        root-subject (ref :top)
        root-cat (ref :top)
        inflected {:root {:cat root-cat
                          :subcat root-subcat
                          :subj root-subject}
                   :cat root-cat
                   :subcat root-subcat
                   :subj root-subject}
        present {:infl :present}]
    (concat
     np-1-lexicon
     (list
      fare
      ;; too much copying going on here: figure out minimal amount of copying needed.
      (fs/merge {:root (fs/copy fare)}
                (fs/copy inflected)
                (fs/copy present)
                (fs/copy {:italian "facio"
                          :english "do"
                          :subj {:person :1st
                                 :number :sing}}))
      (fs/merge {:root (fs/copy fare)}
                 inflected
                 present
                 {:italian "fai"
                  :english "do"
                  :subj {:person :2nd
                         :number :sing}})))))

(defn generate-vp [rules lexicon head]
  (let [rule (random-rule rules '((:head :cat) :verb) '((:head :infl) :present))
        debug (println (str "RULE:" rule))
        debug (println (str "HEAD:" (fs/get-in rule '(:head))))
        heads (apply  ;; filter by both rule's :head and head param (either may be nil)
               search/query-with-lexicon
               (concat
                (list lexicon)
                (list (fs/get-in rule '(:head)))))]
    (is (> (.size heads) 0))
    (let [head (random (seq heads))
          lexical-comps
          (seq (search/query-with-lexicon lexicon
                 (fs/get-in head '(:subcat))))
          lexical-comp
          (nth lexical-comps (rand-int (.size lexical-comps)))
          np (generate-np vp-1-rules lexicon lexical-comp)]
      (fs/unify
       (fs/copy rule) {:head (fs/copy head) :comp np}))))

(deftest vp-rules
  "find a suitable vp rule."
  (let [rule (random-rule vp-1-rules '((:head :cat) :verb))]
    (is (not (nil? rule)))))

(deftest vp-find-head
  "find a lexeme that can be the head of a verb-phrase."
  (let [rule (random-rule vp-1-rules '((:head :cat) :verb))]
    (let [head
          (random ;; filter by both rule's :head and head param (either may be nil)
           (seq (search/query-with-lexicon vp-1-lexicon
                  (fs/get-in rule '(:head)))))]
      (is (not (nil? head))))))

(deftest vp-1
  "generate a vp (transitive verb+np)"
  (let [trials (map (fn [num]
                      {:trial num
                       :result (generate-vp vp-1-rules vp-1-lexicon nil)})
                    (range 0 5))]
    (printfs vp-1-lexicon "vp-1-lexicon.html")
    (println
     (map (fn [unified]
            (let [num (:trial unified)
                  unified (:result unified)]
              (log/info (str "DOING NUM: " num))
              (is (or
                   (= (read-off-italian unified) '("facio" ("il" "compito")))
                   (= (read-off-italian unified) '("fai" ("il" "compito")))))
              (printfs
               (merge
                {:italian (join (flatten (read-off-italian unified)) " ")}
                unified)
               (str "vp-1-" num ".html"))))
          trials))))
    
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
          :italian "io"}
         {:cat :noun
          :human true
          :person :2nd
          :number :sing
          :subcat :nil!
          :italian "tu"})))

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
          (let [subject
                (if (and (not (nil? subjects)) (> (.size subjects) 0))
                  (nth subjects (rand-int (.size subjects)))
                  (do
                    (log/warn (str "no matching subjects found: searched for: " (fs/get-in vp '(:subcat))))
                    nil))
                unified (fs/unify
                         (fs/copy rule) {:head vp :comp subject})]
            unified))))))
                
(deftest sentence-1
  "generate a sentence (subject+vp)"
  (let [trials (map (fn [num]
                      {:trial num
                       :result (generate-sentence sentence-rules sentence-lexicon)})
                    (range 0 5))]
    (printfs sentence-lexicon "sentence-lexicon.html")
    (printfs sentence-rules "sentence-rules.html")
    (println
     (map (fn [trial]
            (let [num (:trial trial)
                  result (:result trial)]
              (println (str "sentence-1 result: " (seq (read-off-italian result))))
              (is
               (or (= (read-off-italian result) '("io" ("facio" ("il" "compito"))))
                   (= (read-off-italian result) '("tu" ("fai" ("il" "compito"))))))
              (printfs
               (merge
                {:italian (join (flatten (read-off-italian result)) " ")}
                result)
               (str "sentence-1-" num ".html"))))
          trials))
    (println
     (printfs
      (map (fn [trial]
             (let [num (:trial trial)
                   result (:result trial)]
               (merge
                {:italian (join (flatten (read-off-italian result)) " ")}
                result)))
           trials)
      (str "sentences.html")))))

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
