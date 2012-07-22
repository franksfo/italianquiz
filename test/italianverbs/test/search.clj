(ns italianverbs.test.search
  (:use [clojure.test]
        [rdutest]
        [italianverbs.search]
        [clojure.set])
  (:require
   [italianverbs.fs :as fs]
   [italianverbs.lexiconfn :as lexfn]))

(deftest test-lazy-query
  (let [result
        (take 1 (lazy-query {:cat :verb}))]
    (> (.size result) 0)))
  
;; TODO: move some of these lexically-related tests to lexicon.clj (e.g. the 'fare' (to do) test).
(def tests
  (list

   (rdutest
    "At least one verb is in the lexicon."
    (take 1 (lazy-query {:cat :verb}))
    #(> (.size %) 0)
    :one-verb)
   
   (rdutest
    "At least one noun is in the lexicon."
    (take 1 (lazy-query {:cat :noun}))
    #(> (.size %) 0)
    :one-noun)
   
   (rdutest
    "At least one nominative noun is in the lexicon (and that the conjunction of more than one predicate works."
    (take 1 (lazy-query {:cat :noun :case :nom}))
    #(> (.size %) 0)
    :one-nom-noun)

   (rdutest
    "Features that should be ignored for lookup purposes are in fact ignored."
    (take 1 (lazy-query {:cat :noun :case :nom :ref '("foo" "bar") :refmap {:baz 42}}))
    #(> (.size %) 0)
    :ignore-non-lookup-features)

   
   (rdutest
    "The intersection of mutually-exclusive queries is the null set (since a noun can't be both nominative and non-nominative)."
    (intersection (query {:cat :noun :case :nom}) (query {:cat :noun :case {:not :nom}}))
    #(= (.size %) 0)
    :null-set)
   
   (rdutest
    "There's at least one verb that takes an edible object (a nested query works)."
    (take 1 (lazy-query {:cat :verb :obj {:edible true}}))
    #(> (.size %) 0)
    :verb-with-edible-object)
   
   (rdutest
    "Looking up an irregular verb inflection by its root works."
    (search {:root {:italian "fare"}})
    #(and (not (= % nil)) (> (.size %) 0))
    :lookup-roots)

   (rdutest
    "Look up a word by its root: find a verb whose root is 'fare (to make)' (e.g. 'facio (i make)')."
    (search {:root (first (search {:italian "fare" :cat :verb :infl :infinitive}))})
    #(and (not (= % nil)) (> (.size %) 0))
    :lookup-complex-root)
   
   (rdutest
    "Search should take multiple parameters and merge them together."
    (search {:cat :noun} {:gender :fem} {:number :singular})
    #(and (not (= % nil)) (> (.size %) 0))
    :search-multiple-maps)
   
   ;; "pathifying" a map means (by example)
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

   (rdutest
    "Pathify a large map."
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
      :number :plural})
    (fn [paths] (= (count paths) 11)) ;; TODO: write better test.
    :pathify-long-map-1)
   
   (rdutest
    "Pathify another map."
    (pathify {:root {:gender :masc :human true :det {:cat :det} :animate true :morph "morph-noun" :common true :cat :noun :italian "uomo" :person :3rd}})
    (fn [paths] (= (count paths) 9)) ;; TODO: write better test.
    :pathify-long-map-2)

   (rdutest
    "Pathify, no reference."
    (pathify {:a 42})
    (fn [paths]
      (= (get (first paths) (list :a)) 42))
    :pathify-no-reference)

   (rdutest
    "Pathify with reference."
    (pathify {:a (ref 42)})
    (fn [paths]
      (= (get (first paths) (list :a)) 42))
    :pathify-with-reference)

   ;; {:a {:b 42}} => {(:a :b) 42}
   (rdutest
    "Pathify with inner map, no reference."
    (pathify {:a {:b 42}})
    (fn [paths]
      (and (= (first (keys (first paths))) (list :a :b))
           (= (first (vals (first paths))) 42)))
    :pathify-with-inner-map)

   ;; {:a (ref {:b 42})} => {(:a :b) 42}
   (rdutest
    "Pathify with inner map with reference."
    (pathify {:a (ref {:b 42})})
    (fn [paths]
      (and (= (first (keys (first paths))) (list :a :b))
           (= (first (vals (first paths))) 42)))
    :pathify-with-inner-map-with-reference)

   (rdutest
    "noun-agreement via unify (1)."
    (let [cane (lexfn/lookup "cane")
          determiner (random-lexeme (fs/unify {:cat :det}
                                              (get-in cane '(:comp))))]
      (fs/unify cane {:comp determiner}))
    (fn [np]
      (and (not (nil? np))
           (not (= (get-in np '(:comp)) :fail))
           (or (= (get-in np '(:comp :italian)) "il")
               (= (get-in np '(:comp :italian)) "un"))
           (= (type (get-in np '(:number))) clojure.lang.Ref)
           (or (= @(get-in np '(:number)) "singular")
               (= @(get-in np '(:number)) :singular))
           (= (type (get-in np '(:gender))) clojure.lang.Ref)
           (or (= @(get-in np '(:gender)) "masc")
               (= @(get-in np '(:gender)) :masc))
           (= (get-in np '(:gender))
              (get-in np '(:comp :gender)))
           (= (get-in np '(:number))
              (get-in np '(:comp :number))))))

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



