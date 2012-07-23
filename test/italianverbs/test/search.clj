(ns italianverbs.test.search
  (:use [clojure.test]
        [rdutest]
        [italianverbs.search]
        [clojure.set])
  (:require
   [italianverbs.fs :as fs]
   [italianverbs.lexiconfn :as lexfn]))

;; TODO: move some of these lexically-related tests to lexicon.clj (e.g. the 'fare' (to do) test).
(deftest lazy-query-verb
  (let [result
        (take 1 (lazy-query {:cat :verb}))]
    (is (> (.size result) 0))))

(deftest lazy-query-noun
  (let [result
        (take 1 (lazy-query {:cat :noun}))]
    (is (> (.size result) 0))))

(deftest lazy-query-noun-nom
  (let [result
        (take 1 (lazy-query {:cat :noun :case :nom}))]
    (is (> (.size result) 0))))

(deftest lookup-ignores-some-features ;; Features that should be ignored for lookup purposes are in fact ignored.
  (let [result
        (take 1 (lazy-query {:cat :noun :case :nom :ref '("foo" "bar") :refmap {:baz 42}}))]
    (is (> (.size result) 0))))

;;The intersection of mutually-exclusive queries is the null set
;;(since a noun can't be both nominative and non-nominative).
(deftest lookup-contradictory-features
  (let [result (intersection (query {:cat :noun :case :nom}) (query {:cat :noun :case {:not :nom}}))]
    (is (= (.size result) 0))))

;; There's at least one verb that takes an edible object (a nested query works).
(deftest verb-with-edible-object
  (let [result (take 1 (lazy-query {:cat :verb :obj {:edible true}}))]
    (is (> (.size result) 0))))

;; Looking up an irregular verb inflection by its root works.
;; (i.e. Look up a word by its root: find a verb whose root is 'fare (to make)' (e.g. 'facio (i make)'))
(deftest lookup-irregular-inflection-by-root
  (let [result (search {:root {:italian "fare"}})]
    (is (not (= result nil)))
    (is (> (.size result) 0))))

(deftest lookup-irregular-infinitive
 (let [result (search {:root (first (search {:italian "fare" :cat :verb :infl :infinitive}))})]
   (is (not (= result nil)))
   (is (> (.size result) 0))))

;; Search should take multiple parameters and merge them together.
(deftest search-with-multiple-arguments
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

(deftest noun-agreement-via-unify-1
  (let [np
    (let [cane (lexfn/lookup "cane")
          determiner (random-lexeme (fs/unify {:cat :det}
                                              (get-in cane '(:comp))))]
      (fs/unify cane {:comp determiner}))]
    (is (not (nil? np)))
    (is (not (= (get-in np '(:comp)) :fail)))
    (is (or (= (get-in np '(:comp :italian)) "il")
            (= (get-in np '(:comp :italian)) "un")))

    (is (= (type (get-in np '(:number))) clojure.lang.Ref))
    (is (or (= @(get-in np '(:number)) "singular")
            (= @(get-in np '(:number)) :singular)))

    (is (= (type (get-in np '(:gender))) clojure.lang.Ref))

    (is (or (= @(get-in np '(:gender)) "masc")
            (= @(get-in np '(:gender)) :masc)))

    (is (= (get-in np '(:gender))
           (get-in np '(:comp :gender))))
    (is (= (get-in np '(:number))
           (get-in np '(:comp :number))))))

(def tests
  (list

   (rdutest
    "verb-agreement via unify (1)."
    (let [avere (lexfn/lookup "avere")
          subject (random-lexeme (:subj avere))]
      (fs/unify avere {:subj subject}))
    (fn [result]
      (and (not (nil? result))
           (= (type (get-in result '(:number))) clojure.lang.Ref)
           (not (= @(get-in result '(:number)) :fail))
           (= (get-in result '(:number))
              (get-in result '(:subj :number)))
           (= (type (get-in result '(:person))) clojure.lang.Ref)
           (= (get-in result '(:person))
              (get-in result '(:subj :person))))))

   (rdutest
    "verb-agreement via unify (2)."
    (let [hanno (lexfn/lookup "hanno")
          subject (random-lexeme (get-in hanno '(:subj)))]
      (fs/unify hanno {:subj subject}))
    (fn [result]
      (and (not (nil? result))
           (= (type (get-in result '(:number))) clojure.lang.Ref)
           (not (= @(get-in result '(:number)) :fail))
           (or (= @(get-in result '(:number)) "plural")
               (= @(get-in result '(:number)) :plural)))))

   (rdutest
    "lookup subjects based on verb constraints."
    (let [hanno (lexfn/lookup "hanno")]
      (fs/unify hanno {:subj (random-lexeme (:subj hanno))}))
    (fn [unified]
      (not (nil? unified))))

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



