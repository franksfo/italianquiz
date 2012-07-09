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
;; => {#<Ref: 42> => {(:a :b) (:c)}
;;                               
(deftest map-inversion
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
                   (= (second paths) '(:c))))))))

(deftest serialization
  (let [myref (ref 42)
        fs {:a {:b myref}
            :c myref}
        result (serialize fs)]
    (is (= (get-in result '(:a :b)) 42))
    (is (= (get-in result '(:c)) 42))
    (is (not (nil? (:refs result))))
    (is (= (.size (:refs result)) 1))
    (is (= (first (first (:refs result))) 42))
    (let [paths (second (first (:refs result)))]
      (is (= (.size paths) 2))
      (is (or (= (first paths) '(:a :b))
              (= (first paths) '(:c))))
      (is (or (= (second paths) '(:a :b))
              (= (second paths) '(:c)))))))

(deftest deserialization
  (let [myref (ref 42)
        fs {:a {:b myref}
            :c myref}
        serialized (serialize fs)
        result (list fs (deserialize serialized))]
    (fn [result] ;; fs and our deserialized fs should be isomorphic.
      ;; TODO: also test to make sure fs original and copy are distinct as well (not ref-equal)
      (let [original (first result)
            copy (second result)]
        (is (= (get-in original '(:a :b))
               (get-in original '(:c))))
        (is (= (get-in copy '(:a :b))
               (get-in copy '(:c))))))))

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

;      (deftest
;       "merging with inner reference:keyset"
;       (let [fs1 {:b (ref :top)}
;             fs2 {:b 42}
;            maps (list fs1 fs2)]
;         (seq (set (mapcat #'keys maps)))) ;; mapcat->set->seq removes duplicates.
;       (fn [result]
;         (= result '(:b))))

      ;; [b [1] :top], [b 42] => [b [1] 42]
;      (deftest
;       "merging with reference"
;       (let [fs1 {:b (ref :top)}
;             fs2 {:b 42}]
;         (fs/merge fs1 fs2))
;       (fn [result]
;         (and (= (type (:b result)) clojure.lang.Ref)
;              (= @(:b result)) 42)))

      ;; [a [b [1] :top]], [a [b 42]] => [a [b [1] 42]]
;      (deftest
;       "merging with inner reference"
;       (let [fs1 {:a {:b (ref :top)}}
;             fs2 {:a {:b 42}}]
;         (fs/merge fs1 fs2))
;       (fn [result]
;         (and (= (type (:b (:a result))) clojure.lang.Ref)
;              (= @(:b (:a result))) 42)))

      ;; [a [b [1] top]], [a [b 42]] => [a [b [1] 42]]
;      (deftest
;       "merging with inner reference, second position"
;       (let [fs1 {:a {:b 42}}
;             fs2 {:a {:b (ref :top)}}]
;         (fs/merge fs1 fs2))
;       (fn [result]
;         (and (= (type (:b (:a result))) clojure.lang.Ref)
;              (= @(:b (:a result))) 42)))

;      (deftest
;       "merging with reference, second position"
;       (let [fs1 {:a 42}
;             fs2 {:a (ref :top)}]
;         (fs/merge fs1 fs2))
;      (fn [result]
;         (and (= (type (:a result)) clojure.lang.Ref)
;              (= @(:a result) 42))))

;      (deftest
;       "merging with reference, second position"
;       (let [fs1 {:a 42}
;             fs2 {:a (ref :top)}]
;         (fs/merge fs1 fs2))
;       (fn [result]
;         (and (= (type (:a result)) clojure.lang.Ref)
;              (= @(:a result) 42))))

;      (deftest
;       "test atom merging with ':not' (special feature) (first in list; fail)"
;       (merge {:not 42} 42)
;       (fn [result]
;         (= result :fail)))

;      (deftest
;       "test atom merging with ':not' (special feature) (second in list; succeed)"
;      (merge 42 {:not 43})
;       (fn [result]
;         (= result 42)))

;      (deftest
;       "test atom merging with ':not' (special feature) (second in list; fail)"
;       (merge (list 42 {:not 42}))
;       (fn [result]
;         :fail))

;      (deftest
;       "test merging with ':not' (special feature)"
;       (unify {:foo 42} {:foo {:not 43}})
;       (fn [result]
;         (= result {:foo 42})))

;      (deftest
;       "test atom unifying with ':not' (special feature) (first in list; fail)"
;       (unify {:not 42} 42)
;       (fn [result]
;         (= result :fail)))

;      (deftest
;       "test atom unifying with ':not' (special feature) (second in list; succeed)"
;       (unify 42 {:not 43})
;       (fn [result]
;         (= result 42)))
;
;      (deftest
;       "test atom unifying with ':not' (special feature) (second in list; fail)"
;       (unify (list 42 {:not 42}))
;       (fn [result]
;         :fail))

;      (deftest
;       "test unifying with ':not' (special feature)"
;       (unify {:foo 42} {:foo {:not 43}})
;       (fn [result]
;         (= result {:foo 42})))

;      (deftest
;       "keywords and strings are equivalent for unification (due to mongo serialization), but canonicalize to keyword."
;       (unify :foo "foo")
;       (fn [result]
;        (= result :foo)))
      
;      (deftest
;       "complicated merge."
;       (let [mycon (list {:comp {:number :singular, :cat :det}} {:gender :masc} {:comp {:def {:not :indef}}, :mass true} {:comp {}, :sport true})]
;         (apply merge mycon))
;       (fn [result] true))

;      (deftest
;       "atomic vals: merge"
;       (merge 5 5)
;       (fn [result] (= 5 5)))

;      (deftest
;       "atomic vals: unify"
;       (unify 5 5)
;       (fn [result] (= 5 5)))

;      (deftest
;       "atomic vals: unify fail"
;       (unify 5 4)
;       (fn [result] (= result :fail)))

;      (deftest
;       "maps: merge"
;       (merge '{:a 42} '{:b 43})
;       (fn [result]
;         (and (= (:a result) 42)
;              (= (:b result) 43))))

;      (deftest
;       "maps: unify"
;       (unify '{:a 42} '{:b 43})
;       (fn [result]
;         (and (= (:a result) 42)
;              (= (:b result) 43))))

;      (deftest
;       "maps: merge (override)"
;       (merge '{:a 42} '{:a 43})
;       (fn [result]
;         (= (:a result) 43)))
      
;      (deftest
;       "maps: unify fail"
;       (unify '{:a 42} '{:a 43})
;       (fn [result]
;         (= (:a result) :fail)))
      
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

;      (deftest
;       "'top' and :top are equivalent when unifying with keyword."
;       (unify "top" :foo)
;       (fn [result]
;         (= result :foo)))

;      (deftest
;       "'top' and :top are equivalent when unifying with reference to keyword."
;       (unify "top" (ref :foo))
;       (fn [result]
;         (and (= (type result) clojure.lang.Ref)
;              (= @result :foo))))

;      (deftest
;       "@'top' and @:top are equivalent when unifying to a keyword."
;       (unify (ref "top") :foo)
;       (fn [result]
;         (and (= (type result) clojure.lang.Ref)
;             (= @result :foo))))

;      (deftest
;       "@'top' and @:top are equivalent when unifying to a string."
;       (unify (ref "top") "foo")
;       (fn [result]
;         (and (= (type result) clojure.lang.Ref)
;              (= @result "foo"))))

;      (deftest
;       "create a map with a reference."
;       (let [fs1 {:a (ref 42)}
 ;            fs2 {:b (get fs1 :a)}]
 ;        (unify fs1 fs2))
 ;      (fn [result]
;         (and (= (type (:a result)) clojure.lang.Ref)
;              (= (type (:b result)) clojure.lang.Ref)
;              (= (:a result) (:b result)))))

;      (deftest
;       "unifying two maps, one with references."
;       (let [fs1 {:a (ref :top)}
;             fs2 {:b (get fs1 :a)}
;             fs3 (fs/unify fs1 fs2)
;             fs4 {:a 42}]
;         (unify fs3 fs4))
;       (fn [result]
;         (and (= (type (:a result)) clojure.lang.Ref)
;              (= (type (:b result)) clojure.lang.Ref)
;              (= (:a result) (:b result))
;              (= @(:a result) 42))))

;      (deftest
;       "unifying two maps, both with references, same features"
;       (let [fs1 {:a (ref 42)}
;             fs2 {:b (get fs1 :a)}
;             fs3 (unify fs1 fs2)
;             fs4 {:a (ref 42)}
;             fs5 {:b (get fs4 :a)}
;             fs6 (unify fs4 fs5)]
;         (unify fs3 fs6))
;       (fn [result]
;         (and
;          (= (type (:a result)) clojure.lang.Ref)
;          (= (type (:b result)) clojure.lang.Ref)
;          (= (:a result) (:b result))
;          (= @(:a result) 42))))

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

      
