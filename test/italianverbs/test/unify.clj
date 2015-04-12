(ns italianverbs.test.unify
  (:refer-clojure :exclude [get-in merge resolve find])
  (:use [italianverbs.unify])
  (:use [clojure.test]))

;; TODO: add more tests for (isomorphic?)

(deftest simple-merge-test
  (let [result (merge {:foo 99} {:bar 42})]
    (is (= (:foo result) 99))
    (is (= (:bar result) 42))))

(deftest simple-unify-test
  (let [result (unify {:foo 99} {:bar 42})]
    (is (= (:foo result) 99))
    (is (= (:bar result) 42))))

(deftest recursive-merge-of-3-maps
  (let [result
        (let [map1 {:foo {:bar 99}}
              map2 {:foo {:baz 42}}
              map3 {:biff 12}]
          (merge map1 map2 map3))]
    ;; test that result looks like:
    ;; {:foo {:bar 99
    ;;        :baz 42}
    ;;  :biff 12}}
    (is (= (:bar (:foo result)) 99))
    (is (= (:baz (:foo result)) 42))
    (is (= (:biff result) 12))
    (is (= (get-in result '(:foo :bar)) 99))
    (is (= (get-in result '(:foo :baz)) 42))
    (is (= (get-in result '(:biff)) 12))))

(deftest unify-unequal-atomic-values
  "Testing that unify(v1,v2)=fail if v1 != v2."
  (let [result (unify {:foo 42} {:foo 43})]
    (is (fail? result))))

(deftest merge-unequal-atomic-values
  "Testing that merge(v1,v2)=v2 (overriding)."
  (let [result (merge {:foo 42} {:foo 43})]
    (is (= (:foo result) 43))))

(deftest ignore-nils-in-values
  "Ignore nils in values (true,nil)."
  (let [result (merge {:foo true} {:foo nil})]
    (is (= (:foo result) true))))

(deftest merge-emptymap
  "emptymap (unlike with nil) overrides true in merge."
  (let [result (merge {:foo true} {:foo {}})]
    (is (= (:foo result) {}))))

(deftest merge-nil
  (let [result (merge {:foo nil} {:foo nil})]
    (is (= result {:foo nil}))))

(deftest unify-atomic-values-with-references
  (let [myref (ref :top)
        val 42
        result (unify myref val)]
    (is (= (type result) clojure.lang.Ref))
    (is (= @result 42))))
      
;; {:a [1] :top
;;  :b [1]     } ,
;; {:a 42}
;;        =>
;; {:a [1] 42
;;  :b [1] }
(deftest merging-references-with-top-and-references
  (let [myref (ref :top)
        fs1 {:a myref :b myref}
        fs2 {:a 42}
        result (merge fs1 fs2)]
    (is (= (type (:a result)) clojure.lang.Ref))
    (is (= @(:a result) 42))
    (is (= @(:b result) 42))
    (is (= (:a result) (:b result)))))

(deftest merging-with-references-with-top-and-references-2
  (let [myref (ref :top)
        fs1 {:a myref}
        fs2 {:a :foo}
        result (merge fs1 fs2)]
    (is (= (type (:a result)) clojure.lang.Ref))
    (is (= @(:a result) :foo))))

(deftest merging-with-inner-reference-keyset
  (let [fs1 {:b (ref :top)}
        fs2 {:b 42}
        maps (list fs1 fs2)
        result (seq (set (mapcat #'keys maps)))] ;; mapcat->set->seq removes duplicates.
    (is (= result '(:b)))))

      ;; [b [1] :top], [b 42] => [b [1] 42]
(deftest merging-with-reference
  "merging with reference"
  (let [fs1 {:b (ref :top)}
        fs2 {:b 42}
        result (merge fs1 fs2)]
    (is (= (type (:b result)) clojure.lang.Ref))
    (is (= @(:b result)) 42)))

;; [a [b [1] :top]], [a [b 42]] => [a [b [1] 42]]
(deftest merging-with-inner-reference
  "merging with inner reference"
  (let [fs1 {:a {:b (ref :top)}}
        fs2 {:a {:b 42}}
        result (merge fs1 fs2)]
    (is (= (type (:b (:a result))) clojure.lang.Ref))
    (is (= @(:b (:a result))) 42)))

;; [a [b [1] top]], [a [b 42]] => [a [b [1] 42]]
(deftest merging-with-inner-reference-second-position
  (let [fs1 {:a {:b 42}}
        fs2 {:a {:b (ref :top)}}
        result (merge fs1 fs2)]
    (is (= (type (:b (:a result))) clojure.lang.Ref))
    (is (= @(:b (:a result))) 42)))

(deftest merging-with-reference-second-position
  "merging with reference, second position"
  (let [fs1 {:a 42}
        fs2 {:a (ref :top)}
        result (merge fs1 fs2)]
    (is (= (type (:a result)) clojure.lang.Ref))
    (is (= @(:a result) 42))))

(deftest merging-fail-with-not-1
  "test atom merging with ':not' (special feature) (first in list; fail)"
  (let [result (merge {:not 42} 42)]
    (is (= result :fail))))

(deftest merging-succeed-with-not
  "test atom merging with ':not' (special feature) (first in list; fail)"
  (let [result (merge 42 {:not 43})]
    (is (= result 42))))

(deftest merging-fail-with-not-2
  "test atom merging with ':not' (special feature) (second in list; fail)"
  (let [result (merge 42 {:not 42})]
    (is (= result :fail))))

(deftest unify-with-not
  (let [result (unify {:foo 42} {:foo {:not 43}})]
    (is (= result {:foo 42}))))

(deftest unify-fail-with-not
  "test atom unifying with ':not' (special feature) (first in list; fail)"
  (let [result (unify {:not 42} 42)]
    (is (= result :fail))))

(deftest unify-succeed-with-not
  "test atom unifying with ':not' (special feature) (second in list; succeed)"
  (let [result (unify 42 {:not 43})]
    (is (= result 42))))

(deftest unify-fail-with-not-2
  "test atom unifying with ':not' (special feature) (second in list; fail)"
  (let [result (unify 42 {:not 42})]
    (is (= result :fail))))

(deftest unify-nested-not
  "test unifying with ':not' (special feature)"
  (let [result (unify {:foo 42} {:foo {:not 43}})]
    (is (= result {:foo 42}))))

(deftest unify-with-not-and-top1
  "unifying {:not X} with :top should return {:not X} if X != top."
  (let [result (unify {:not 42} :top)]
    (is (= result {:not 42}))))

(deftest unify-with-not-and-top2
  "(reversed argument order as preceding): unifying :top with {:not X} should return {:not X} if X != top."
  (let [result (unify :top {:not 42})]
    (is (= result {:not 42}))))

(deftest keywords-and-strings-equiv
  "keywords and strings are equivalent for unification (due to accomodating mongo serialization), but canonicalize to keyword."
  (let [result (unify :foo "foo")]
    (is (= result :foo))))
      
(deftest complicated-merge
  (let [mycon (list {:comp {:number :singular, :cat :det}} {:gender :masc} {:comp {:def {:not :indef}}, :mass true} {:comp {}, :sport true})
        result (apply merge mycon)]
    (is (= (get-in result '(:comp :number)) :singular))))

(deftest merge-atomic-vals
  (let [result (merge 5 5)]
    (is (= result 5))))

(deftest unify-atomic-vals
  (let [result (unify 5 5)]
    (is (= result 5))))

(deftest unify-atomic-vals-fail
  (let [result (unify 5 4)]
    (is (= result :fail))))

(deftest maps-merge
  (let [result (merge '{:a 42} '{:b 43})]
    (is (= (:a result) 42)
        (= (:b result) 43))))

(deftest maps-unify
  (let [result (unify '{:a 42} '{:b 43})]
    (is (= (:a result) 42)
        (= (:b result) 43))))

(deftest merge-override
  (let [result (merge '{:a 42} '{:a 43})]
    (is (= (:a result) 43))))

(deftest unify-override
  (let [result (unify '{:a 42} '{:a 43})]
    (is (fail? result))))

      
;;      (deftest
;;       "merge should union :not-values"
;;       (merge {:not 41} {:not 42})
;;       (fn [result]
;;         (= (set (:not result)) (set 41 42)))

;;      
;;      (deftest
;;       "unify should union :not-values"
;;       (unify {:not 41} {:not 42})
;;       (fn [result]
;;         (= (set (:not result)) (set 41 42)))

(deftest unify-with-top
  "'top' and :top are equivalent when unifying with keyword."
  (let [result (unify "top" :foo)]
    (is (= result :foo))))

(deftest top-string-and-keyword-equiv
  "'top' and :top are equivalent when unifying with reference to keyword."
  (let [result (unify "top" (ref :foo))]
    (is (= (type result) clojure.lang.Ref))
    (is (= @result :foo))))

(deftest top-ref
  "@'top' and @:top are equivalent when unifying to a keyword."
  (let [result (unify (ref "top") :foo)]
    (is (= (type result) clojure.lang.Ref))
    (is (= @result :foo))))

(deftest top-ref-2
  "@'top' and @:top are equivalent when unifying to a string."
  (let [result (unify (ref "top") "foo")]
    (is (= (type result) clojure.lang.Ref))
    (is (= @result "foo"))))

(deftest map-with-reference
  (let [fs1 {:a (ref 42)}
        fs2 {:b (get fs1 :a)}
        result (unify fs1 fs2)]
    (is (= (type (:a result)) clojure.lang.Ref))
    (is (= (type (:b result)) clojure.lang.Ref))
    (is (= (:a result) (:b result)))))

(deftest unify-two-maps-one-with-references
  "unifying two maps, one with references."
  (let [fs1 {:a (ref :top)}
        fs2 {:b (get fs1 :a)}
        fs3 (unify fs1 fs2)
        fs4 {:a 42}
        result (unify fs3 fs4)]
    (is (= (type (:a result)) clojure.lang.Ref))
    (is (= (type (:b result)) clojure.lang.Ref))
    (is (= (:a result) (:b result)))
    (is (= @(:a result) 42))))

(deftest unify-two-maps-with-references
  "unifying two maps, both with references, same features"
  (let [fs1 {:a (ref 42)}
        fs2 {:b (get fs1 :a)}
        fs3 (unify fs1 fs2)
        fs4 {:a (ref 42)}
        fs5 {:b (get fs4 :a)}
        fs6 (unify fs4 fs5)
        result (unify fs3 fs6)]
    (is (= (type (:a result)) clojure.lang.Ref))
    (is (= (type (:b result)) clojure.lang.Ref))
    (is (= (:a result) (:b result)))
    (is (= @(:a result) 42))))

(deftest detect-unify-fail
  "test that, (fail? fs) <=> true if at least one of fs's path's value is :fail."
  (let [fs1 {:a 42}
        fs2 {:a :fail}]
    (is (= (fail? fs1) false))
    (is (= (fail? fs2) true))
    (is (= (fail? {:a (ref :fail)}) true))
    (is (= (fail? {:a (ref 42)}) false)))
    (is (= (fail? {:a (ref {:b :fail})}) true)))


;(deftest pathify-no-references
;  "a simple test of pathify with no structure-sharing."
;  (let [mymap {:a {:c 42}, :b {:c 42}, :c 42}
;        pathify (pathify mymap)]
;    (is (= pathify '((:a :c) 42 (:b :c) 42 (:c) 42)))))

(deftest paths-to-values-1
  "test path-to-value, which returns a list of all ways of reaching
a given value in a given map."
  (let [ref1 (ref 42)
        mymap {:a ref1 :b ref1}
        ptf (paths-to-value mymap ref1 nil)]
    (is (= ptf '((:a)(:b))))))

(deftest paths-to-values-2
  "test path-to-value, which returns a list of all ways of reaching
a given value in a given map."
  (let [ref2 (ref 42)
        ref1 (ref {:c ref2})
        mymap {:a ref1
               :b ref1
               :d ref2}
        paths-to-ref1 (paths-to-value mymap ref1 nil)]
    (is (= paths-to-ref1 '((:a)(:b))))))

(deftest paths-to-values-3
  "test path-to-value, which returns a list of all ways of reaching
a given value in a given map."
  (let [ref2 (ref 42)
        ref1 (ref {:c ref2})
        mymap {:a ref1
               :b ref1
               :d ref2}
        paths-to-ref2 (paths-to-value mymap ref2 nil)]
    (is (= paths-to-ref2 '((:a :c)(:b :c)(:d))))))

(deftest all-refs1
  (let [ref1 (ref 42)
        mymap {:a ref1, :b ref1}
        refs (uniq (flatten (all-refs mymap)))]
    (is (= refs (list ref1)))))

(deftest all-refs2
  (let [ref1 (ref 42)
        ref2 (ref 43)
        mymap {:a ref1, :b ref2}
        refs (uniq (flatten (all-refs mymap)))]
    (is (or (= refs (list ref1 ref2))
            (= refs (list ref2 ref1))))))

(deftest all-refs3
  (let [ref1 (ref 42)
        ref2 (ref 43)
        mymap {:a ref1 :b {:c ref2}}
        refs (uniq (flatten (all-refs mymap)))]
    (is (or (= refs (list ref1 ref2))
            (= refs (list ref2 ref1))))))

(deftest all-refs4
  (let [ref1 (ref 42)
        mymap {:a ref1 :b {:c ref1}}
        refs (uniq (sort (all-refs mymap)))]
    (is (= refs (list ref1)))))

(deftest all-refs5
  (let [ref2 (ref 42)
        ref1 (ref {:c ref2})
        mymap {:a ref1 :b ref1 :d ref2}
        refs (uniq (sort (all-refs mymap)))]
    (is (or (= refs (list ref1 ref2))
            (= refs (list ref2 ref1))))))

(deftest skeletize-1
  (let [mymap {:a 42}]
    (is (= (skeletize mymap) mymap))))

(deftest skeletize-2
  (let [ref1 (ref 42)
        mymap {:a 42 :b ref1}]
    (is (= (skeletize mymap) {:a 42 :b :top}))))

(deftest skeletize-3
  (let [ref1 (ref 42)
        ref2 (ref 43)
        mymap {:a ref1 :b ref2}]
    (is (= (skeletize mymap) {:a :top :b :top}))))

(deftest ser-db-1
  (let [ref1 (ref 42)
        mymap {:a ref1, :b ref1}
        ser (ser-db mymap)]
    (is (= ser
           {
            {:ref ref1
             :skel 42} '((:a)(:b))}))))

;; TODO: this test is unnecessarily strict: see below for specifics
(deftest ser-db-2
  (let [ref2 (ref 42)
        ref1 (ref {:c ref2})
        mymap {:a ref1 :b ref1 :d ref2}
        ser (ser-db mymap)]
    (is (=
         ser
         {
          {:ref ref1
           ;; TODO: could also be '((:b)(:a)).
           :skel {:c :top}} '((:a)(:b))
          {:ref ref2
           ;; TODO: could also be '((:b :c)(:a :c)(:d))
           ;; (or other possible orderings).
           :skel 42} '((:a :c)(:b :c)(:d))
          }))))

(deftest serialize-1
  (let [ref1 (ref 42)
        mymap {:a ref1, :b ref1}
        ser (serialize mymap)]
    (is (= ser

           '((nil {:b :top :a :top})

             ;; TODO: could be '((:b)(:a))
             (((:a)(:b)) 42))))))

(deftest serialize-2
  (let [ref2 (ref 42)
        ref1 (ref {:c ref2})
        mymap {:a ref1, :b ref1 :d ref2}
        ser (serialize mymap)]
    (is (= ser

           '((nil {:d :top, :b :top, :a :top})

             ;; TODO: could be '((:b)(:a))
             (((:a) (:b)) {:c :top})

             ;; TODO: could be '((:b :c)(:a c)..etc
             (((:a :c) (:b :c) (:d)) 42))))))

(deftest serialize-3
  (let [mymap {:a 42 :b (ref 43)}]
    (is (not (nil? (serialize mymap))))))

(deftest serialize-4
  (let [ref3 (ref "avere")
        ref2 (ref {:italian "fatto"})
        ref1 (ref {:infl :infinitive
                   :italian ref3})
        vp {:a ref1
            :b {:italian ref2
                :root {:infl :infinitive
                       :pass-prossimo ref2
                       :pass-prossimo-aux ref1}}
            :italian {:a ref3
                      :b ref2}
            :infl :infinitive}
        serialized (serialize vp)
        ]
    (not (nil? vp))
    (not (nil? serialized))
    (= (.size serialized) 4)))

(deftest create-shared-values-1
  (let [ref2 (ref 42)
        ref1 (ref {:c ref2})
        mymap {:a ref1, :b ref1 :d ref2}
        my-ser (serialize mymap)
        create-shared-vals (create-shared-values my-ser)
        types (map (fn [val]
                     (type val))
                   create-shared-vals)
        derefs (map (fn [val]
                      @val)
                    create-shared-vals)]
    (is (= (first derefs)
           {:d :top
            :b :top
            :a :top}))
    (is (= (second derefs)
           {:c :top}))

    (is (= (nth derefs 2)
           42))
    
    (is (= types (list
                  clojure.lang.Ref
                  clojure.lang.Ref
                  clojure.lang.Ref)))))

(deftest create-path-in-1
  (let [path '(:a :b :c :d :e)
        val 43
        result (create-path-in path val)]
    (is (= (get-in result path) val))))

(deftest deser-with-ref
  (let [serialized [[nil {:a "PH"}] [[["a"]] 42]]
        deserialized (deserialize serialized)]
    (is (not (nil? deserialized)))
    (is (= (type (:a deserialized)) clojure.lang.Ref))
    (is (= @(:a deserialized) 42))))

;; deserialize a map's serialized form
(deftest deser-1
  (let [ref2 (ref 42)
        ref1 (ref {:c ref2})
        mymap {:a ref1, :b ref1 :d ref2}
        my-ser (serialize mymap)
        my-deser (deserialize my-ser)]
    (is (not (= my-ser nil)))
    (is (= (type (:a my-deser)) clojure.lang.Ref))
    (is (= (type (:a my-deser)) clojure.lang.Ref))

    ;; a)
    (is (= (type (get @(get my-deser :a) :c)) clojure.lang.Ref))
    (is (= (type (get @(get my-deser :b) :c)) clojure.lang.Ref))
    (is (= (type (:d my-deser)) clojure.lang.Ref))
    (is (= (:a my-deser) (:b my-deser)))
    (is (= (get @(get my-deser :a) :c)
           (get my-deser :d)))
    (is (= (get @(get my-deser :b) :c)
           (get my-deser :d)))
    (is (= @(get @(get my-deser :a) :c)
           42))

    ;; similar tests as a) above, but using fs/get-in
    (is (= (type (get-in my-deser '(:a :c))) java.lang.Long))
    (is (= (type (get-in my-deser '(:b :c))) java.lang.Long))
    (is (= (type (get-in my-deser '(:d))) java.lang.Long))
    (is (= (get-in my-deser '(:a :c))
           (get-in my-deser '(:d))))
    (is (= (get-in my-deser '(:b :c))
           (get-in my-deser '(:d))))
    (is (= (get-in my-deser '(:a :c))
           42))))

(deftest deser-2
  (let [ref3 (ref "avere")
        ref2 (ref {:italian "fatto"})
        ref1 (ref {:infl :infinitive
                   :italian ref3})
        vp {:a ref1
            :b {:italian ref2
                :root {:infl :infinitive
                       :pass-prossimo ref2
                       :pass-prossimo-aux ref1}}
            :italian {:a ref3
                      :b ref2}
            :infl :infinitive}
        myser (serialize vp)
        ]
    (not (nil? vp))
    (not (nil? myser))))

;(if false (deftest pathify-one-atomic-reference
;  "a map with one atom (42) shared"
;  (let [ref1 (ref 42)
;        mymap {:a ref1 :b ref1}
;        pathify (pathify mymap)]
;    (is (= pathify '((:a) 42 (:b) 42))))))

;;      (deftest
;;       "unifying two maps, both with references, overlapping features"
;;       (let [fs1 {:a (ref 42)}
;;             fs2 {:b (get fs1 :a)}
;;             fs3 (unify fs1 fs2)
;;             fs4 {:b (ref 42)}
;;             fs5 {:c (get fs4 :b)}
;;             fs6 (unify fs4 fs5)]
;;         (unify fs3 fs6))
;;       (fn [result]
;;         (and (= (type (:a result)) clojure.lang.Ref)
;;              (= (type (:b result)) clojure.lang.Ref)
;;              (= (type (:c result)) clojure.lang.Ref)
;;              (= (:a result) (:b result))
;;              (= (:b result) (:c result))
;;              (= @(:a result) 42))))

(deftest copy-with-not
  (let [fs1 {:a (ref {:not 42})}
        fs1-copy (copy fs1)]
    (is (not (fail? fs1-copy)))))


;; TODO: remove this test: use merge-with-keys instead, which does not use the variable 'strict'.
(deftest unify-string-and-map
  "This is to allow values of keys that are string-unifier-keys, like :english and :italian,
   whose values are strings to over-ride values that are maps (in which
   case they are specs of how to compute a string: agreement
   information such as gender and number."
  (is (or (= strict true) ;; the test will fail if unify/strict is true, so short-circuit this test if so.
          (= "foo"
             (unify "foo"
                    {:english "foo"})))))

(deftest overflow
  "merge has a problem: we hit StackOverflowError java.util.regex.Pattern$BmpCharProperty.match (Pattern.java:3366) when this test is run.
   Code works as expected if merge is replaced with unify. However, this test passes - the SOE seems to only happen
when run from a REPL."
  (unify
   (get-in (merge (let [head-cat (ref :top)
                                    head-is-pronoun (ref :top)
                                    head-sem (ref :top)
                                    head-infl (ref :top)]
                                {:synsem {:cat head-cat
                                          :pronoun head-is-pronoun
                                          :sem head-sem
                                          :infl head-infl}
                                 :head {:synsem {:cat head-cat
                                                 :pronoun head-is-pronoun
                                                 :infl head-infl
                                                 :sem head-sem}}})

                              (let [essere (ref :top)
                                    infl (ref :top)]
                                {:italian {:a {:infl infl}}
                                 :english {:a {:infl infl}}
                                 :synsem {:infl infl
                                          :essere essere}
                                 :head {:italian {:infl infl}
                                        :english {:infl infl}
                                        :synsem {:essere essere
                                                 :infl infl}}}))
                 '(:head))
   (unify
    {:italian {:foo 42}}
    (let [infl (ref :top)]
      {:italian {:infl infl}
       :english {:infl infl}
       :synsem {:infl infl}}))))

(deftest nil-and-top
  ;; ...should return emptylist.
  (is (= nil
         (unify nil :top))))

(deftest nil-and-anything-except-top
  ;; ...should return :fail.
  (is (fail?
       (unify nil {:foo 42}))))

(deftest emptylist-and-top
  ;; ...should return emptylist.
  (is (= '()
         (unify '() :top))))

(deftest emptylist-and-anything-except-top
  ;; ...should return :fail.
  (is (fail?
       (unify '() {:foo 42}))))

(deftest set-and-top
  (is (= (unify #{1 2 3} :top)
         #{1 2 3})))

(deftest set-and-set-is-intersection
  (is (= (unify #{1 2} #{2 3})
     #{2})))

(deftest serialized-set
  (let [result (serialize (set (list 1 2)))]
    (is (= result #{(list (list nil 1)) (list (list nil 2))}))))

(deftest refset2map-test
  (let [myref (ref #{1 2})
        input {:a myref
               :b #{{:c myref} {:d 3}}}
        result (refset2map input)]
    (is (map? result))
    (is (set? (get-in result '(:a))))
    (is (set? (get-in result '(:b))))
    (is (= myref (:ref (first (get-in result '(:a))))))
    (is (= myref (:ref (second (get-in result '(:a))))))
    (is (or (= 1 (:val (first (get-in result '(:a)))))
            (= 2 (:val (first (get-in result '(:a)))))))
    (is (= 2 (.size (get-in result '(:b)))))
))

(deftest step2-test
  (let [myref (ref #{1 2})
        input {:a myref
               :b #{{:c myref} {:d 3}}}
        result (refset2map input)
        step2-result (step2 result)]
    (is (set? step2-result))
    (is (= (.size step2-result) 6))))

(deftest test-final
  (let [input
        (let [myref (ref #{1 2})]
          {:a myref
           :b #{{:c myref} {:d 3}}})
        final (expand-disj input)]
    (= (.size final) 2)))

(def parent
  (let [catref (ref :top)]
    {:head {:cat catref}
     :cat catref}))

(def disj-cat #{{:cat :noun}
                {:cat :verb}})

(def parent-with-disj
  (let [catref (ref #{{:cat :noun}
                      {:cat :verb}})]
    {:head {:cat catref}
     :cat catref}))

(deftest category-disjunction
  (let [result (expand-disj parent-with-disj)]
    (is (= (.size result) 2))))

(deftest expand-constraints
  (let [constraints {:constraints #{{:synsem {:infl :futuro
                                              :sem {:tense :futuro}}}
                                    {:synsem {:infl :present
                                              :sem {:tense :present}}}}}
        constraints-expanded (expand-disj constraints)]
    (is (= (.size constraints-expanded) 2))))

(deftest unify-with-set-test
  (let [result (unify {:b 42}
                      {:c #{1 2}})]
    (is (set? result))
    (is (= 2 (.size result)))
    (is (or (= (get-in (first result) '(:c)) 1)
            (= (get-in (first result) '(:c)) 2)))))

(deftest unify-with-set-test2
  (let [result (unify {:b 42}
                      {:c #{1 2}})]
    (is (set? result))))

(deftest unify-with-set-and-ref
  (let [result (unifyc {:a (ref :top)} {:a {:b #{1 2}}})]
    (is (= (.size result) 2))
    (is (not (fail? (first result))))
    (is (not (fail? (second result))))))



(deftest cycle-is-fail
  "unification that would lead to a cycle results in fail, and avoids a StackOverflowError."
  (let [ref1 (ref :top)
        ref2 (ref :top)
        foo1 {:a ref1 :b ref1}
        foo2 {:a ref2 :b {:a ref2}}]
    ;; Test both copy and non-copy - first two are both copying before unification, so should be equivalent,
    ;; but might as well test more variants.
    (is (fail? (unifyc foo1 foo2)))
    (is (fail? (unify (copy foo1) (copy foo2))))
    (is (fail? (unify foo1 foo2)))))

(deftest isomorphic-true1
  (is (= true (isomorphic? {:a 42 :b 43 :c 44} {:a 42 :b 43 :c 44}))))

(deftest isomorphic-false1
  (is (= false (isomorphic? {:a 42 :b 43 :c 45} {:a 42 :b 43 :c 44}))))

(deftest isomorphic-false2
  (is (= false (isomorphic? {:a 42 :b 43} {:a 42 :b 43 :c 44}))))

(deftest isomorphic-false3
  (is (= false (isomorphic? {:a 42 :b 43 :c 44} {:a 42 :b 43}))))

(deftest unify-with-string2
  (let [arg1 {:italiano "gatto"}
        arg2 {:italiano {:initial true}}
        result (unify arg1 arg2)]
  (is (not (fail? result)))
  (is (= result
         {:italiano {:initial true
                     :italiano "gatto"}}))))

(deftest unify-with-string3
  (let [arg1 {:italiano "gatto"}
        arg2 {:italiano "gatto"}
        result (unify arg1 arg2)]
    (is (not (fail? result)))
    (is (= result
           {:italiano "gatto"}))))

(deftest unify-with-string4
  (let [arg1 {:italiano "gatto"}
        arg2 {:italiano "cane"}
        result (unify arg1 arg2)]
    (is (fail? result))))



