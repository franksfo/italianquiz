(ns italianverbs.test.search
  (:use [clojure.test]
        [italianverbs.search]
        [clojure.set])
  (:require
   [italianverbs.fs :as fs]
   [italianverbs.lexiconfn :as lexfn]))

(deftest pv-match-test
  (let [entry
        (do (lexfn/clear!)
            (lexfn/add-lexeme {:cat :verb :italian "leggere"})
            (lexfn/fetch-one))]
    (not (nil? entry))
    (let [deserialized (fs/deserialize (:entry entry))]
      (not (nil? (pv-matches deserialized '(:italian) "leggere"))))))

(deftest lazy-query-verb
  (let [add-verb
        (do (lexfn/clear!)
            (lexfn/add-lexeme {:cat :verb :italian "leggere"}))
        get-fetch (lexfn/fetch)]
    (is (> (.size get-fetch) 0))
    (let [get-verbs
          (take 1 (lazy-query {:cat :verb}))]
      (is (> (.size get-verbs) 0)))))

(deftest lazy-query-noun
  (let [add-noun
        (lexfn/add-lexeme {:cat :noun :italian "cane"})]
    (let [result
          (take 1 (lazy-query {:cat :noun}))]
      (is (> (.size result) 0)))))

(deftest lazy-query-noun-nom
  (let [add-io
        (lexfn/add-lexeme {:cat :noun :italian "io" :case :nom})]
  (let [result
        (take 1 (lazy-query {:cat :noun :case :nom}))]
    (is (> (.size result) 0)))))

;;The intersection of mutually-exclusive queries is the null set
;;(since a noun can't be both nominative and non-nominative).
(deftest lookup-contradictory-features
  (let [result (intersection (query {:cat :noun :case :nom}) (query {:cat :noun :case {:not :nom}}))]
    (is (= (.size result) 0))))

(if false ;; exclude this test for now: no special serialization-specific features such as :ref or :refmap.
  (deftest lookup-ignores-some-features ;; Features that should be ignored for lookup purposes are in fact ignored.
    (let [result
          (take 1 (lazy-query {:cat :noun :case :nom :ref '("foo" "bar") :refmap {:baz 42}}))]
      (is (> (.size result) 0)))))

;; There's at least one verb that takes an edible object (a nested query works).
(deftest verb-with-edible-object
  (lexfn/add-lexeme {:cat :verb :obj {:edible true}})
  (let [result (take 1 (lazy-query {:cat :verb :obj {:edible true}}))]
    (is (> (.size result) 0))))

;; Looking up an irregular verb inflection by its root works.
;; (i.e. Look up a word by its root: find a verb whose root is 'fare (to make)' (e.g. 'facio (i make)'))
(deftest lookup-irregular-inflection-by-root
  (lexfn/add-lexeme {:italian "fare" :infl :infinitive :cat :verb})
  (lexfn/add-lexeme {:italian "facio" :root (first (search {:italian "fare"}))})
  (let [result (lazy-query {:root {:italian "fare"}})]
    (is (not (= result nil)))
    (is (> (.size result) 0))))

(deftest lookup-irregular-infinitive
  (let [result (search {:root (first (search {:italian "fare" :cat :verb :infl :infinitive}))})]
    (is (not (= result nil)))
    (is (> (.size result) 0))))

;; Search should take multiple parameters and merge them together.
(deftest search-with-multiple-arguments
  (lexfn/add-lexeme {:italian "ragazza" :cat :noun :gender :fem :number :singular})
  (let [result (search {:cat :noun} {:gender :fem} {:number :singular})]
    (is (not (= result nil)))
    (is (> (.size result) 0))))



   
;; "pathifying" a map means flattening a tree into a
;; list of path-value pairs.
;;
;;for example:
;;
;; {:root {:gender :masc
;;         :human true
;;         :det {:cat :det
;;               :def true}
;;         :animate true
;;         :morph "morph-noun"
;;         :common true
;;         :cat :noun
;;         :italian "uomo"}
;;  :italian "uomini"
;;  :number :plural}
;;
;; => ({(:root :gender) :masc}
;;     {(:root :human) true}
;;     {(:root :det :cat) :det}
;;     {(:root :det :def) true}
;;     {(:root :animate) true}
;;     {(:root :morph) "morph-noun"}
;;     {(:root :common) true}
;;     {(:root :cat) :noun}
;;     {(:root :italian) "uomo"}
;;     {(:italian) "uomini"}
;;     {(:number) :plural})

;; Pathify a large map.
(deftest pathify-map
  (let [result 
        (pathify
         {:root {:gender :masc
                 :human true
                 :det {:cat :det
                       :def true}
                 :animate true
             :morph "morph-noun"
                 :common true
                 :cat :noun
                 :italian "uomo"}
          :italian "uomini"
          :number :plural})]
    ;; TODO: write better test with more detailed tests.
    (is (= (count result) 11)))) 

(deftest pathify-2 ;; Pathify another map.
  (let [result 
        (pathify {:root
                  {:gender
                   :masc
                   :human true
                   :det {:cat :det}
                   :animate true
                   :morph "morph-noun"
                   :common true :cat
                   :noun :italian "uomo"
                   :person :3rd}})]
    (is (= (count result) 9)))) ;; TODO: write better test as preceding.

(deftest pathify-no-refs
  (let [result (pathify {:a 42})]
    (is (= (get (first result) (list :a)) 42))))

(deftest pathify-with-ref
  (let [result (pathify {:a (ref 42)})]
    (is (= (get (first result) (list :a)) 42))))

;; {:a {:b 42}} => {(:a :b) 42}
(deftest pathify-inner-map-no-refs
  (let [result
        (pathify {:a {:b 42}})]
    (is (= (first (keys (first result))) (list :a :b)))
    (is (= (first (vals (first result))) 42))))

;; {:a (ref {:b 42})} => {(:a :b) 42} (same as with no refs)
(deftest pathify-inner-map-with-refs
  (let [result
        (pathify {:a (ref {:b 42})})]
    (is (= (first (keys (first result))) (list :a :b)))
    (is (= (first (vals (first result))) 42))))

(if false
  (do
;; TODO: move some of these lexically-related tests to lexicon.clj (e.g. the 'fare' (to do) test).
  
(deftest verb-agreement-via-unify-1
  (let [vp
        (let [avere (lookup "avere")
              subject (random-lexeme (:subj avere))]

          (fs/unify avere {:subj subject}))]
    (is (not (nil? vp)))
    (is (= (type (get-in vp '(:number))) clojure.lang.Ref))
    (is (not (= @(get-in vp '(:number)) :fail)))
    (is (= (get-in vp '(:number)) (get-in vp '(:subj :number))))
    (is (= (type (get-in vp '(:person))) clojure.lang.Ref))
    (is (= (get-in vp '(:person)) (get-in vp '(:subj :person))))))

(deftest verb-agreement-via-unify-2
  (let [result
        (let [hanno (lookup "hanno")
              subject (random-lexeme (get-in hanno '(:subj)))]
          (fs/unify hanno {:subj subject}))]
    (is (not (nil? result)))
    (is (= (type (get-in result '(:number))) clojure.lang.Ref))
    (is (not (= @(get-in result '(:number)) :fail)))
    (is (or (= @(get-in result '(:number)) "plural") (= @(get-in result '(:number)) :plural)))))

(deftest subjects-based-on-verb-constraints
  (let [unified
        (let [hanno (lookup "hanno")]
          (fs/unify hanno {:subj (random-lexeme (:subj hanno))}))]
    (is (not (nil? unified)))))

;           (= (type (get-in hanno '(:person))) clojure.lang.Ref)
;           (or (= @(get-in hanno '(:person)) "3rd")
;               (= @(get-in hanno '(:person)) :3rd))

;           (= (get-in hanno '(:person))
;              (get-in hanno '(:comp :person)))
;           (= (get-in hanno '(:gender))
;              (get-in hanno '(:comp :gender)))
;           (= (get-in hanno '(:number))
;              (get-in hanno '(:comp :number))))))



))

(deftest test-query-with-lexicon
  (let [lexicon (list {:a 42 :b 43} {:a 99 :b {:c 100} :d :foo})]
    (is (= 1
           (.size (query-with-lexicon lexicon
                    {:a 42}))))
    (is (= 0
           (.size (query-with-lexicon lexicon
                    {:a 43}))))

    ;; search with two maps
    (is (= 1
           (.size (query-with-lexicon lexicon
                    {:a 99} {:b {:c 100}}))))

    ;; same semantics as above, but in only one map
    (is (= 1
           (.size (query-with-lexicon lexicon
                    {:a 99 :b {:c 100}}))))

    ))


;; TODO
(deftest test-query-with-lexicon-with-query-with-refs
  (is (= true true)))
