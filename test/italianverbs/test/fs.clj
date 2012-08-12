(ns italianverbs.test.fs
  (:use [italianverbs.fs])
  (:use [clojure.test]))

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
    (is (= (:foo result) :fail))))

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

;; test map inversion
;; path (:a :b) points to a reference, whose value is an integer, 42.
;; path (:c) also points to the same reference.
;;
;;[ :a  [:b [1] 42]
;;  :c  [1]]
;;
;; =>
;;
;;key    | value
;;-------+------                               
;;42     | (a b),(c)
;;
;;(actually ref-invert returns an array of kv pairs,
;; but illustrating as returning a map makes it
;; more clear hopefully).
;; TODO: it should actually return a map.
(if false
(deftest ref-invert-1
  (let [myref (ref 42)
        fs {:a {:b myref}
            :c myref}
        result (ref-invert fs)]
    (is (= (.size result) 1))
    (is (= (first (first result))
            42))
    (is (let [paths (second (first result))]
          (and (= (.size paths) 2)
               (or (= (first paths) '(:a :b))
                   (= (first paths) '(:c)))
               (or (= (second paths) '(:a :b))
                   (= (second paths) '(:c)))))))))

;; {:a [2] {:c [1] 42}
;;  :b [2]
;;  :d [1] }
;;
;; =>
;;
;; key    | value
;; -------+------
;;{:c 42} | (a),(b)
;;  42    | (a c),(c)
        
(if false (deftest rev-invert-2
  (let [ref1 (ref 42)
        ref2 (ref {:c ref1})
        mymap {:a ref2
               :b ref2
               :c ref1}
        result (ref-invert mymap)
        ;; see TODO in fs.clj:(ref-invert)
        resultmap (zipmap (vec (map (fn [x] (first x)) result))
                          (vec (map (fn [x] (second x)) result)))]
    (is (not (nil? resultmap)))
    (is (not (nil? (get resultmap 42))))
    (is (not (nil? (get resultmap {:c 42})))))))
    
;; {:a [2] {:c {:e [1] 42} }
;;  :b [2]
;;  :d [1] }
;;
;; =>
;;
;; key          | value
;; -------------+------
;;{:c {:e 42} } | (a),(d)
;;  42          | (a c),(d)

(if false        
(deftest ref-invert-3
  (let [ref1 (ref 42)
        ref2 (ref {:c {:e ref1}})
        mymap {:a ref2
               :b ref2
               :d ref1}
        result (ref-invert mymap)
        ;; see TODO in fs.clj:(ref-invert)
        resultmap (zipmap (vec (map (fn [x] (first x)) result))
                          (vec (map (fn [x] (second x)) result)))]
    (is (not (nil? resultmap)))
    (is (not (nil? (get resultmap 42))))
    (is (not (nil? (get resultmap {:c {:e 42}})))))))


;(deftest serialization
;  (let [myref (ref 42)
;        fs {:a {:b myref}
;            :c myref}
;        result (serialize fs)]
;    (is (= (get-in result '(:a :b)) 42))
;    (is (= (get-in result '(:c)) 42))
;    (is (not (nil? (:refs result))))
;    (is (= (.size (:refs result)) 1))
;    (is (= (first (first (:refs result))) 42))
;    (let [paths (second (first (:refs result)))]
;      (is (= (.size paths) 2))
;      (is (or (= (first paths) '(:a :b))
;              (= (first paths) '(:c))))
;      (is (or (= (second paths) '(:a :b))
;              (= (second paths) '(:c)))))))

;(deftest serialization2
;  (let [fs {:a (ref {:b (ref 42)})}
;        result (serialize fs)]
;    (is (not (nil? (:refs result))))))

;(deftest deserialization
;  (let [myref (ref 42)
;        fs {:a {:b myref}
;            :c myref}
;        serialized (serialize fs)
;        result (list fs (deserialize serialized))]
;    (fn [result] ;; fs and our deserialized fs should be isomorphic.
;      ;; TODO: also test to make sure fs original and copy are distinct as well (not ref-equal)
;      (let [original (first result)
;            copy (second result)]
;        (is (= (get-in original '(:a :b))
;               (get-in original '(:c))))
;        (is (= (get-in copy '(:a :b))
;               (get-in copy '(:c))))))))

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
    (is (= (:a result) :fail))))

      
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

;(deftest pathify-no-references
;  "a simple test of pathify with no structure-sharing."
;  (let [mymap {:a {:c 42}, :b {:c 42}, :c 42}
;        pathify (pathify mymap)]
;    (is (= pathify '((:a :c) 42 (:b :c) 42 (:c) 42)))))

(deftest get-vals
  (let [ref1 (ref 42)
        mymap {:a ref1 :b ref1}
        get-vals (uniq (vals-r mymap))]
    (is (= get-vals (list ref1)))))

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

(deftest ref-to-rfv-1
  "a simple test of mapping references to reference-free-values (i.e. skeletons)"
  ;; 1.':ph' means 'PlaceHolder'
  ;; 2. nil  simply maps to the outermost skeleton of the map.
  ;; {:a [1] 42, :b [1] } => 
  ;;       {[1] 42 => ((a)(b)), nil => {:a :ph, :b :ph}}
  (let [ref1 (ref 42)
        mymap {:a ref1, :b ref1}
        rfv (rfv mymap)]
    (is (not (nil? rfv)))
    (is (= rfv
           {ref1 '((:a)(:b)), nil {:a :ph :b :ph}}))))

(deftest get-refs1
  (let [ref1 (ref 42)
        mymap {:a ref1, :b ref1}
        refs (uniq (flatten (all-refs mymap)))]
    (is (= refs (list ref1)))))

(deftest get-refs2
  (let [ref1 (ref 42)
        ref2 (ref 43)
        mymap {:a ref1, :b ref2}
        refs (uniq (flatten (all-refs mymap)))]
    (is (or (= refs (list ref1 ref2))
            (= refs (list ref2 ref1))))))

(deftest get-refs3
  (let [ref1 (ref 42)
        ref2 (ref 43)
        mymap {:a ref1 :b {:c ref2}}
        refs (uniq (flatten (all-refs mymap)))]
    (is (or (= refs (list ref1 ref2))
            (= refs (list ref2 ref1))))))

(deftest get-refs4
  (let [ref1 (ref 42)
        mymap {:a ref1 :b {:c ref1}}
        refs (uniq (flatten (all-refs mymap)))]
    (is (= refs (list ref1)))))

(deftest get-refs5
  (let [ref2 (ref 42)
        ref1 (ref {:c ref2})
        mymap {:a ref1 :b ref1 :d ref2}
        refs (uniq (flatten (all-refs mymap)))]
    (is (or (= refs (list ref1 ref2))
            (= refs (list ref2 ref1))))))

(deftest skeletize-1
  (let [mymap {:a 42}]
    (is (= (skeletize mymap) mymap))))

(deftest skeletize-2
  (let [ref1 (ref 42)
        mymap {:a 42 :b ref1}]
    (is (= (skeletize mymap) {:a 42 :b :PH}))))

(deftest skeletize-3
  (let [ref1 (ref 42)
        ref2 (ref 43)
        mymap {:a ref1 :b ref2}]
    (is (= (skeletize mymap) {:a :PH :b :PH}))))

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
           :skel {:c :PH}} '((:a)(:b))
          {:ref ref2
           ;; TODO: could also be '((:b :c)(:a :c)(:d))
           ;; (or other possible orderings).
           :skel 42} '((:a :c)(:b :c)(:d))
          }))))

(deftest ser-1
  (let [ref1 (ref 42)
        mymap {:a ref1, :b ref1}
        ser (ser mymap)]
    (is (= ser
           {
            '((:a) (:b))             42,
            nil                      {:b :PH, :a :PH}}))))

(deftest ser-2
  (let [ref2 (ref 42)
        ref1 (ref {:c ref2})
        mymap {:a ref1, :b ref1 :d ref2}
        ser (ser mymap)]
    (is (= ser
           {
            nil                      {:d :PH, :b :PH, :a :PH}
            '((:a) (:b))             {:c :PH}
            '((:a :c) (:b :c) (:d))  42}))))

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

      
