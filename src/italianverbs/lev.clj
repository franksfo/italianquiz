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
  "adds value [i y] to the matrix for all i:[0,end]"
  (if (get matrix (list i (- y 1)))
    (testi1
     (merge
      matrix
      {(list i y) (get-min matrix i y)})
     (+ 1 i) y)
    matrix))

(defn test [matrix j y]
  "adds one row at a time to matrix for all rows up to y."
  ;; TODO: for consistency, do similar to testi1 does:
  ;;  (check for existence of (get matrix [0,j])).
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

(defn tablize-row [matrix i j]
  (if (get matrix (list i j))
    (str
     "<td>"
     (get matrix (list i j))
     "</td>"
     (tablize-row matrix (+ i 1) j))
    ""))

(defn matrix-header [charlist]
  (if (> (.size charlist) 0)
    (str "<th>" (first charlist) "</th>"
         (matrix-header (rest charlist)))))

(defn tablize [matrix vertical-char-list j]
  (if (get matrix (list 0 j))
    (str
     "<tr>"
     "<th>"
     j
     "</th>"
     "<th>"
     ;; j'th element of vertical string.
     (nth vertical-char-list j)
     "</th>"
     (tablize-row matrix 0 j)
     "</tr>"
     (tablize matrix vertical-char-list (+ 1 j)))
    ""))

(defn matrix []
  {:italian "matrice"
   :test (str "<table>"

              "<tr>"
              "<th colspan='2'> </th>"
              (matrix-header (explode "0123456"))
              "</tr>"

              "<tr>"
              "<th colspan='2'> </th>"
              (matrix-header (explode "matrice"))
              "</tr>"
              (tablize (test (testi "matrice") 1 5)
                       (explode "matrix") 0)
              "</table>")})


