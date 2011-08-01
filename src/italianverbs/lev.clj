(ns italianverbs.lev
  (:require
   [clojure.contrib.string :as string]
   [italianverbs.grammar :as gram]))


(defn explode [string]
  "abc => (\"a\" \"b\" \"c\")"
  (rest (string/split (java.util.regex.Pattern/compile "") string)))

(defn get-min [matrix x y]
  (let [diag 0] ;; depends on whether char = char (0 if ==, 1 otherwise)
    (min (if (> x 0)
           (+ 1 (get matrix (list (- x 1) y)))
           Float/POSITIVE_INFINITY)
         (if (> y 0)
           (+ 1 (get matrix (list x (- y 1))))
           Float/POSITIVE_INFINITY)
         (if (and (> x 0) (> y 0))
           (+ diag
              (get matrix (list (- x 1) (- y 1))))
           Float/POSITIVE_INFINITY))))

(defn testi1 [matrix i y]
  (if (get matrix (list i (- y 1)))
    (testi1
     (merge
      matrix
      {(list i y) (get-min matrix i y)})
     (+ 1 i) y)
    matrix))

(defn test [matrix j y]
  (if (<= j y)
    (let [new-matrix
          (merge
           matrix
           (testi1 matrix 0 j))]
      (test new-matrix
            (+ 1 j)
            y))
    matrix))

(defn create-initial-row [charlist i]
  (if (> (.size charlist) 0)
    (merge
     {(list i 0) i}
     (create-initial-row (rest charlist)
                         (+ i 1)))
    {}))

(defn testi [string]
  (create-initial-row (explode string) 0))

(defn matrix []
  {:italian "matrice"
   :matrix (.toString (test (testi "foobar") 1 5))})

