(ns italianverbs.test.generate
  (:use [clojure.test]
        [italianverbs.generate])
  (:require
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [somnium.congomongo :as mongo]
   ;; TODO: graduate italianverbs.fs to :use.
   [italianverbs.fs :as fs]
   [italianverbs.html :as html]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.search :as search]))

;; TODO:
;; 1. use fixtures and compose-fixtures rather than defining
;; per-test lexicons and grammars

;; 2. compose tests where appropriate rather than separate functions
;; e.g. (generate-np), (generate-vp), etc.
;;

(def numtrials 10)
(def numtrials-printable 2)

(defmacro deftest-ignore [str x y]
  (is true))

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
  (let [debug (if false (println (str "read-off-italian: " (type expression))))]
    (if (and (not (nil? expression)) (not (nil? (fs/get-in expression '(:italian)))))
      (let [retval (fs/get-in expression '(:italian))]
        (if false (println (str "<= " retval)))
        retval)
      (if (not (nil? (fs/get-in expression '(:a))))
        (map (fn [child]
               (read-off-italian child))
             (list (fs/get-in expression '(:a))
                   (fs/get-in expression '(:b))))
        ""))))
  
(defn random [members]
  "return a randomly-selected member of the set members."
  (if (nil? members) nil
      (nth (seq members) (rand-int (.size members)))))

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

(defn get-rules-that-match-head [rules head]
  (mapcat (fn [rule]
            (let [unified (fs/unify (fs/copy rule) head)]
              (if (not (= :fail (fs/get-in unified '(:head :subcat))))
                (list unified))))
          rules))

(defn random-rule [rules & filters-of-path-value]
  "pick a rule at random from the subset of rules that matches the provided filters."
  (random (get-rules rules filters-of-path-value)))

(defn generate-np [rules lexicon head-spec]
  "generate a noun phrase from the supplied rules, lexicon,
   and optional _head_:a filter on the lexicon to find the head noun."
  (let [rule (random (if (not (nil? head-spec))
                       (get-rules-that-match-head rules head-spec)
                       rules))
        debug (println (str "USING RULE: " rule))]
    (let [head (random
                (seq (search/query-with-lexicon lexicon (fs/get-in rule '(:head)))))
          debug (println (str "GOT HEAD FROM LEXICON: " head))
          comp ;; should return nil if using rule with no comp.
          (random (seq (search/query-with-lexicon lexicon
                         {:synsem (fs/get-in head '(:subcat :a))})))
          debug (println (str "GOT COMP FROM LEXICON: " comp))]
    (fs/unify
       (fs/copy rule)
       (if (not (nil? comp))
         (fs/copy {:comp comp :head head})
         (fs/copy {:head head}))))))

(def np-1-rules 
  (let [np-rule-1 ;; NP -> Comp Head
        (let [comp-synsem (ref {:cat :det})
              comp (ref {:synsem comp-synsem})
              head-synsem (ref {:cat :noun})
              head (ref {:synsem head-synsem
                         :subcat {:a comp-synsem}})]
          {:comment "np -> det noun"
           :head head
           :subcat :nil!
           :synsem head-synsem
           :comp comp
           :a comp
           :b head})]
    (list np-rule-1)))

(def np-1-lexicon
  (let [gender (ref :top)
        number (ref :top)
        agreement {:synsem {:gender gender
                            :number number}
                   :subcat {:a {:gender gender
                                :number number}}}]
  (list
   (fs/unify (fs/copy agreement)
             {:synsem {:cat :noun
                       :number :sing
                       :gender :masc
                       :artifact true
                       :person :3rd}
              :subcat {:a {:cat :det}}
              :italian "compito"
              :english "homework"})
   (fs/unify (fs/copy agreement)
             {:synsem {:cat :noun
                       :number :sing
                       :gender :masc
                       :edible true
                       :artifact true
                       :person :3rd}
              :subcat {:a {:cat :det}}
              :italian "pane"
              :english "bread"})
   (fs/unify (fs/copy agreement)
             {:synsem {:cat :noun
                       :number :sing
                       :gender :fem
                       :edible true
                       :artifact true
                       :person :3rd}
              :subcat {:a {:cat :det}}
              :italian "pasta"
              :english "pasta"})


   
   (fs/unify (fs/copy agreement)
             {:synsem {:cat :noun
                       :number :sing
                       :gender :masc
                       :artifact false
                       :human true
                       :person :3rd}
              :subcat {:a {:cat :det}}
              :italian "ragazzo"
              :english "guy"})

   (fs/unify (fs/copy agreement)
             {:synsem {:cat :noun
                       :number :sing
                       :gender :fem
                       :artifact false
                       :human true
                       :person :3rd}
              :subcat {:a {:cat :det}}
              :italian "ragazza"
              :english "girl"})

   {:synsem {:cat :det
             :gender :masc
             :number :sing}
    :italian "il"
    :english "the"}
   {:synsem {:cat :det
             :gender :fem
             :number :sing}
    :italian "la"
    :english "the"}
   {:synsem {:cat :det
             :gender :masc
             :number :plur}
    :italian "i"
    :english "the"}
   {:synsem {:cat :det
             :gender :fem
             :number :plur}
    :italian "le"
    :english "the"})))
   

(deftest get-rules-that-match-head-test
  "find the subset of _rules_ where each rule's head unifies with head."
  (let [rules np-1-rules
        head {:head {:subcat :nil!}}]
    (let [matching-rules
          (get-rules-that-match-head np-1-rules head)]
      (printfs matching-rules "matching-rules.html")
      (is (= (.size matching-rules) 1)))))

(deftest np-1
  "generate some random noun phrases."
  (let [trials (map (fn [num]
                      {:trial num
                       :result (generate-np np-1-rules np-1-lexicon nil)})
                    (range 0 numtrials))
        debug (println (str "TRIALS: " trials))]
    (println ;; seems to be the only way to get tests to run in slime.
     (map (fn [trial]
            (let [result (:result trial)
                  italian (string/trim (join (flatten (read-off-italian result)) " "))]
              (is
               (or
                (= italian "il compito")
                (= italian "il ragazzo")
                (= italian "la ragazza")
                (= italian "lei")))))
          trials))

    (println
     (printfs
      (map (fn [trial]
             (let [num (:trial trial)
                   result (:result trial)]
               {:trial num
                :italian (join (flatten (read-off-italian result)) " ")
                :result result}))
           (map (fn [num]
                  (nth trials num))
                (range 0 numtrials-printable)))
      "nps.html"))))


;  (let [np (generate-np np-1-rules np-1-lexicon nil)]
;    (= (read-off-italian np) '("il" "compito"))
;    (printfs np "np-1.html")))

(def vp-1-rules
  (concat
   np-1-rules
   (list
    (let [vp-rule-1
          (let [comp-synsem (ref {:cat :noun :case :acc})
                comp (ref {:synsem comp-synsem :subcat :nil!})
                subj (ref {:cat :noun :case :nom})
                head-synsem (ref {:cat :verb
                                  :infl {:not :infinitive}
                                  :subj subj
                                  :obj comp-synsem})
                head (ref {:synsem head-synsem
                           :subcat {:a comp-synsem
                                    :b subj}})]
            {:comment "vp -> head comp"
             :head head
             :subcat {:a subj}
             :synsem head-synsem
             :comp comp
             :a head
             :b comp})]
      vp-rule-1))))
  
(def vp-1-lexicon
  (let [verb-with-root
        (let [cat (ref :top)
              subcat (ref :top)]
          {:synsem {:cat cat}
           :subcat subcat
           :root {:subcat subcat
                  :synsem {:cat cat}}})
        transitive
        (let [subj (ref :top)
              obj (ref :top)]
          {:synsem {:subj subj
                    :obj obj}
           :subcat {:a obj
                    :b subj}
           :root {:synsem {:subj subj
                           :obj obj}}})
        finite
        (fs/unify
         (fs/copy verb-with-root)
         {:synsem {:infl :present}})]
    (concat
     np-1-lexicon
     (let [fare
           (let [subj {:cat :noun
                       :artifact false
                       :human true}
                 obj {:cat :noun
                      :human false
                      :artifact true}]
             (fs/unify
              transitive
              {:italian "fare"
               :english "to do"
               :synsem {:cat :verb
                        :subj subj
                        :obj obj
                        :infl :infinitive}
               :subcat {:a obj
                        :b subj}}))]
       (list fare
             (fs/unify
              (fs/copy finite)
              (fs/copy transitive)
              {:root (fs/copy fare)
               :italian "facio"
               :english "make" ;; regular: TODO: use regular english morphology for this and others below
               :subcat {:b {:person :1st
                            :number :sing}}})
             (fs/unify
              (fs/copy finite)
              (fs/copy transitive)
              {:root (fs/copy fare)
               :italian "fai"
               :subcat {:b {:person :2nd
                            :number :sing}}})
             (fs/unify
              (fs/copy finite)
              (fs/copy transitive)
              {:root (fs/copy fare)
               :italian "fa"
               :subcat {:b {:person :3rd
                            :number :sing}}})
             (fs/unify
              (fs/copy finite)
              (fs/copy transitive)
              {:root (fs/copy fare)
               :italian "facciamo"
               :subcat {:b {:person :1st
                            :number :plur}}})
             (fs/unify
              (fs/copy finite)
              (fs/copy transitive)
              {:root (fs/copy fare)
               :italian "fate"
               :subcat {:b {:person :2nd
                            :number :plur}}})
             (fs/unify
              (fs/copy finite)
              (fs/copy transitive)
              {:root (fs/copy fare)
               :italian "fanno"
               :subcat {:b {:person :3rd
                            :number :plur}}}))))))

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
          debug (println (str "LEXICAL-COMP: " lexical-comp))
          np (generate-np rules lexicon {:subcat (fs/get-in lexical-comp '(:subcat))})]
      (fs/unify
       (fs/copy rule) {:head (fs/copy head) :comp np}))))

(deftest-ignore vp-rules
  "find a suitable vp rule."
  (let [rule (random-rule vp-1-rules '((:head :cat) :verb))]
    (is (not (nil? rule)))))

(deftest-ignore vp-find-head
  "find a lexeme that can be the head of a verb-phrase."
  (let [rule (random-rule vp-1-rules '((:head :cat) :verb))]
    (let [head
          (random ;; filter by both rule's :head and head param (either may be nil)
           (seq (search/query-with-lexicon vp-1-lexicon
                  (fs/get-in rule '(:head)))))]
      (is (not (nil? head))))))

(deftest-ignore vp-1
  "generate a vp (transitive verb+np)"
  (let [trials (map (fn [num]
                      {:trial num
                       :result (generate-vp vp-1-rules vp-1-lexicon nil)})
                    (range 0 numtrials))]
    (printfs vp-1-lexicon "vp-1-lexicon.html")
    (println
     (map (fn [unified]
            (let [num (:trial unified)
                  unified (:result unified)]
              (log/info (str "DOING NUM: " num))
              (println (str "read-off-italian: " (seq (read-off-italian unified))))
              (is (or
                   (= (read-off-italian unified) '("facio" ("il" "compito")))
                   (= (read-off-italian unified) '("fa" ("il" "compito")))
                   (= (read-off-italian unified) '("fai" ("il" "compito")))))))
          trials))
    (println
     (printfs
      (map (fn [trial]
             (let [num (:trial trial)
                   result (:result trial)]
               {:trial num
                :italian (join (flatten (read-off-italian result)) " ")
                :result result}))
           (map (fn [num]
                  (nth trials num))
                (range 0 numtrials-printable)))
      "vps.html"))))
    
(def sentence-rules
  (concat
   vp-1-rules
   (let [subcatted (ref {:cat :noun})
         head-synsem (ref {:cat :verb
                           :subj subcatted
                           })
         comp (ref {:synsem subcatted :subcat :nil!})
         head (ref {:synsem head-synsem
                    :subcat {:a subcatted}})]
     (list
      {:comment "s -> np vp"
       :subcat :nil!
       :head head
       :comp comp
       :a comp
       :b head
       :synsem head-synsem
       }))))
   
(def sentence-lexicon
  (concat
   vp-1-lexicon
   (list {:synsem {:cat :noun
                   :case :nom
                   :human true
                   :artifact false ;; <- :human true => artifact :false
                   :person :1st
                   :number :sing}
          :subcat :nil!
          :italian "io"}
         {:synsem {:cat :noun
                   :case :nom
                   :human true
                   :artifact false ;; <- :human true => artifact :false
                   :person :2nd
                   :number :sing}
          :subcat :nil!
          :italian "tu"}
         {:synsem {:cat :noun
                   :case :nom
                   :human true
                   :artifact false ;; <- :human true => artifact :false
                   :person :3rd
                   :gender :masc
                   :number :sing}
          :subcat :nil!
          :italian "lui"}
         {:synsem {:cat :noun
                   :case :nom
                   :human true
                   :artifact false ;; <- :human true => artifact :false
                   :person :3rd
                   :gender :fem
                   :number :sing}
          :subcat :nil!
          :italian "lei"}
         {:synsem {:cat :noun
                   :case :nom
                   :human true
                   :artifact false ;; <- :human true => artifact :false
                   :person :1st
                   :number :plur}
          :subcat :nil!
          :italian "noi"}
         {:synsem {:cat :noun
                   :case :nom
                   :human true
                   :artifact false ;; <- :human true => artifact :false
                   :person :2nd
                   :number :plur}
          :subcat :nil!
          :italian "voi"}
         {:synsem {:cat :noun
                   :case :nom
                   :human true
                   :artifact false ;; <- :human true => artifact :false
                   :person :3rd
                   :number :plur}
          :subcat :nil!
          :italian "loro"})))

(defn fs? [fs]
  (or (= (type fs)
         clojure.lang.PersistentArrayMap)
      (= (type fs)
         clojure.lang.PersistentHashMap)))

(deftest get-sentence-rules
  "get a top-level sentence rule"
  (is (fs? (random-rule sentence-rules '((:subcat) :nil!)))))

(deftest get-verb-head
  "get a verb that can be the head of a vp."
  (printfs sentence-lexicon "sentence-lexicon.html")
  (printfs sentence-rules "sentence-rules.html")
  (let [verb
        (random (search/query-with-lexicon sentence-lexicon {:subcat :top
                                                             :synsem {:infl :present}}))]
    (is (fs? verb))))

(defn create-vp-step1 [head]
  "create a vp based on a head like in the test above."
  (let [head (if (nil? head)
               (random (search/query-with-lexicon sentence-lexicon {:subcat :top
                                                                    :synsem {:infl :present}})))]
    (let [vp
          (reduce
           (fn [result1 result2]
             (if (nil? result1) result2 result1))
           (map (fn [rule] (if (= (:comment rule) "vp -> head comp")
                             (fs/unify (fs/copy rule) (fs/copy {:head head})))) sentence-rules))]
      (printfs vp "vp-step1.html")
      vp)))

(deftest create-vp-test
  "create a vp based on a head like in the test above."
  (let [vp (create-vp-step1 nil)]
    (let [verb (fs/get-in vp '(:head))]
      (is (not (nil? vp)))
      (is (not (nil? verb))))))

(defn map-head-over-rules [head]
  "try all rules for generating a noun phrase from a head."
  (map (fn [rule]
         (fs/unify (fs/copy rule)
                   (fs/copy {:head head})))
       sentence-rules))

(defn set-rules-to-a-rules [rules lexicon]
  (if (> (.size rules) 0)
    (let [rule (first rules)]
      (assoc
          (set-rules-to-a-rules (rest rules) lexicon)
        rule
        (remove fs/fail?
                (map (fn [item]
                       (fs/unify (fs/copy rule)
                                 {:a (fs/copy item)}))
                     lexicon))))
    {}))

(def rules-to-a-rules (set-rules-to-a-rules sentence-rules sentence-lexicon))

(defn path-to-map [path child]
  "_path_ (:x :y :z),_child_ =>  {:x {:y {:z child}}}"
  (if (= (.size path) 0)
    child
    {(first path)
     (path-to-map (rest path) child)}))

(defn find-first-in [query collection]
  "find the first member of the collection that unifies with query successfully."
  (if (= (.size collection) 0)
    nil
    (let [result (fs/unify query (first collection))]
      (if (not (fs/fail? result))
        result
        (find-first-in query (rest collection))))))

(defn find-lex [query]
  (find-first-in query sentence-lexicon))

(defn find-rule [query]
  (find-first-in query sentence-rules))

(defn map-over-children [parent children path]
  (let [parent-cat (fs/get-in parent (concat path '(:synsem :cat)))]
    (mapcat (fn [child]
              (let [cat-of-child (fs/get-in child '(:synsem :cat))]
                (if (not (fs/fail? (fs/unify parent-cat cat-of-child)))
                  (list (fs/unify (fs/copy parent)
                                  (path-to-map path (fs/copy child)))))))
            children)))

(defn map-over-children-test []
  (let [rule (find-rule {:a {:cat :det} :b {:cat :noun}})]
    (map-over-children rule sentence-lexicon '(:a))))

(def rules-started-with-each-lexeme
  "create lookup table: rule => list (starts of rule with each lexeme)"
  (let [keys sentence-rules
        vals (map (fn [rule]
                    (map-over-children rule sentence-lexicon '(:a)))
                  sentence-rules)]
    (zipmap keys vals)))

(def rules-finished-with-each-lexeme
  "create lookup table: rule => list (starts of rule with each lexeme)"
  (let [keys sentence-rules
        vals (map (fn [rule]
                    (map-over-children rule sentence-lexicon '(:b)))
                  sentence-rules)]
    (zipmap keys vals)))


(defn combine [a-rules b-rules lexicon halt]
  (println "")
  (println (str "a-rules: " (if (not (nil? a-rules)) (.size a-rules) "")))
  (println (str "b-rules: " (if (not (nil? b-rules)) (.size b-rules) "")))
  (println (str "lexicon: " (if (not (nil? lexicon)) (.size lexicon) "")))

  (let [new-a-rules-with-fail (mapcat (fn [rule]
                                        (let [rule-cat (fs/get-in rule '(:a :synsem :cat))]
                                          (mapcat (fn [lexeme]
                                                    (let [cat-of-lexeme (fs/get-in lexeme '(:synsem :cat))]
                                                      (if (not (fs/fail? (fs/unify rule-cat cat-of-lexeme)))
                                                        (list (fs/unify (fs/copy rule)
                                                                        {:a (fs/copy lexeme)})))))
                                                  lexicon)))
                                      a-rules)
        new-a-rules (remove fs/fail? new-a-rules-with-fail)

        new-b-rules-with-fail (mapcat (fn [rule]
                                        (let [rule-cat (fs/get-in rule '(:b :synsem :cat))]
                                          (mapcat (fn [item]
                                                    (let [item-cat (fs/get-in item '(:synsem :cat))]
                                                      (if (not (fs/fail? (fs/unify rule-cat item-cat)))
                                                        (list (fs/unify (fs/copy rule)
                                                                        {:b (fs/copy item)})))))
                                                  lexicon)))
                                      b-rules)
        new-b-rules (remove fs/fail? new-b-rules-with-fail)
        new-lexicon (concat sentence-lexicon new-b-rules)
        nil-b-rules (nil? b-rules)
        cond1 (not nil-b-rules)
        cond2 (= (.size lexicon) (.size new-lexicon))
        debug (do
                (println (str "a succeed ratio: " (.size new-a-rules) "/" (.size new-a-rules-with-fail)))
                (println (str "b succeed ratio: " (.size new-b-rules) "/" (.size new-b-rules-with-fail)))
                (println (str "new-a-rules: " (.size new-a-rules))))
        ]
    (if (and halt cond1 cond2)
      {:a-rules new-a-rules
       :b-rules new-b-rules}
      (combine a-rules new-a-rules new-lexicon
               (and cond1 cond2)))))

(defn generate-all [filename]
  (let [result (combine sentence-rules
                        nil
                        sentence-lexicon
                        false)]
    (printfs
     (html/tablize (:b-rules result))
                                        ;     {:a-rules (html/tablize (:a-rules result))
                                        ;      :b-rules (html/tablize (:b-rules result))}
     filename)
    result))

(defn demo [] (generate-all "demo.html"))

(defn print-first [rule html]
  (printfs rule html))

(deftest print-with-complex-keys
  (html/tablize rules-finished-with-each-lexeme))

(defn workbook [head filename]
  (let [result (combine sentence-rules
                        nil
                        sentence-lexicon
                        false)]
    (printfs
     (html/tablize (:b-rules result))
                                        ;     {:a-rules (html/tablize (:a-rules result))
                                        ;      :b-rules (html/tablize (:b-rules result))}
     filename)
    result))

(deftest map-rules-and-lexicon-test-fare-sentence
  (workbook '{:synsem {:cat :verb :root {:italian "fare"}}} "workbook4.html"))

(deftest map-rules-and-lexicon-test-verb-third-plural
  (workbook
   {:synsem
    {:infl :present
     :cat :verb
     :obj {:cat :noun}
     :subj {:number :plur
            :person :3rd}}}
   "workbook2.html"))

(deftest map-rules-and-lexicon-test-subj-third-plural
  (workbook
   {:synsem
    {:artifact false
     :case :nom
     :cat :noun
     :human true
     :number :plur
     :person :3rd}}
  "subj.html"))

(deftest map-rules-and-lexicon-test-noun-third-sing
  (workbook
   {:synsem
    {:number :sing
     :person :3rd
     :cat :noun
     :human true}}
   "workbook3.html"))

(defn map-rules-and-lexicon [head]
  (mapcat (fn [rule]
            (mapcat (fn [lexeme]
                      (let [result
                            (fs/unify {:head (fs/copy lexeme)}
                                      (fs/copy rule))]
                        (if (not (fs/fail? result))
                          (list result))))
                    sentence-lexicon))
          (map-head-over-rules head)))

(defn np-step1 [noun-head]
  (random (map-rules-and-lexicon noun-head)))

(deftest create-np-test
  "create a vp based on a head like in the test above."
  (let [vp-step1 (create-vp-step1 nil)]
    (printfs vp-step1 "vp-step1.html")
    (is (fs? vp-step1))
    (let [subcat-criteria
          (fs/get-in vp-step1 '(:comp))
          noun-head
          (random (search/query-with-lexicon sentence-lexicon subcat-criteria))]
      (printfs subcat-criteria "subcat-criteria.html")
      (is (fs? noun-head))
      (printfs noun-head "noun-head.html")
      (let [np-step1 (np-step1 noun-head)]
        (printfs np-step1 "np-step1.html")
        (let [det (random (search/query-with-lexicon sentence-lexicon (fs/get-in np-step1 '(:comp))))]
          (is (fs? det))
          (printfs det "det.html")
          (let [np-step2
                (fs/unify (fs/copy np-step1) (fs/copy {:comp det}))]
            (printfs np-step2 "np-step2.html")))))))

(defn create-np [noun-head]
  (let [np-step1 (np-step1 noun-head)]
    (let [det (random (search/query-with-lexicon sentence-lexicon (fs/get-in np-step1 '(:comp))))]
      (fs/unify (fs/copy np-step1) (fs/copy {:comp det})))))

(defn create-vp [verb-head]
  (let [vp-step1 (create-vp-step1 verb-head)]
    (is (not (nil? vp-step1)))
    (let [subcat-criteria (fs/get-in vp-step1 '(:comp))
          noun-head (random (search/query-with-lexicon sentence-lexicon subcat-criteria))]
      (let [np-step2 (create-np noun-head)]
        (fs/unify (fs/copy vp-step1) {:comp (fs/copy np-step2)})))))

(deftest create-vp-step2
  "create a vp based on a head like in create-vp-step1 and complement like create-np above."
  (let [vp-step2 (create-vp nil)]
    (is (not (nil? vp-step2)))
    (printfs vp-step2 "vp-step2.html")))

(deftest create-sentence
  "create a sentence"
  (let [vp (create-vp nil)]
    (printfs vp "vps.html")
    (let [subj-criteria {:synsem (fs/get-in vp '(:subcat))}]
      (printfs subj-criteria "subj-criteria.html")
      (is (fs? subj-criteria))
      (let [subj-head
            (create-np subj-criteria)]
        (is (fs? subj-head))
        (printfs subj-head "subj.html")
        (let [sentence-step1
              (reduce
               (fn [result1 result2]
                 (if (nil? result1) result2 result1))
               (map (fn [rule] (if (= (:comment rule) "s -> np vp")
                                 (fs/unify (fs/copy rule) (fs/copy {:comp subj-head}))))
                    sentence-rules))]
          (is (fs? sentence-step1))
          (printfs sentence-step1 "sentence-step1.html")
          (let [sentence
                (fs/unify (fs/copy sentence-step1) {:head (fs/copy vp)})]
            (is (fs? sentence))
            ;; subject/verb agreement
            (is (= (fs/get-in sentence '(:head :subcat))
                   (fs/get-in sentence '(:comp :synsem))))
            (printfs sentence "sentence.html")
            (printfs (list vp subj-head sentence) "sentence-derivation.html")))))))

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
                
(deftest-ignore sentence-1
  "generate a sentence (subject+vp)"
  (let [trials (map (fn [num]
                      {:trial num
                       :result (generate-sentence sentence-rules sentence-lexicon)})
                    (range 0 numtrials))]
    (println
     (map (fn [trial]
            (let [result (:result trial)
                  italian (join (flatten (read-off-italian result)) " ")]
              (println (str "testing output: " italian))
              (is
               (or
                (= italian "io facio il compito")
                (= italian "lei fa il compito")
                (= italian "tu fai il compito")))))
          trials))

    (printfs sentence-lexicon "sentence-lexicon.html")
    (printfs sentence-rules "sentence-rules.html")

    (println
     (printfs
      (map (fn [trial]
             (let [num (:trial trial)
                   result (:result trial)]
               {:trial num
                :italian (join (flatten (read-off-italian result)) " ")
                :result result}))
           (map (fn [num]
                  (nth trials num))
                (range 0 numtrials-printable)))
      "sentences.html"))))
  
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
