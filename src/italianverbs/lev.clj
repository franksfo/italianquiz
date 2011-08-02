(ns italianverbs.lev
  (:require
   [clojure.contrib.string :as string]
   [italianverbs.grammar :as gram]))


(defn explode [string]
  "abc => (\"a\" \"b\" \"c\")"
  (rest (string/split (java.util.regex.Pattern/compile "") string)))

(defn get-min [matrix x y char-x char-y]
  (let [diag
        (if (= char-x char-y) 0 1)]
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

(defn add-one-row [matrix x y horiz-char-list vert-char-list]
  "adds values for one whole row ([x:[0,end], y]) to the matrix."
  (if (get matrix (list x (- y 1)))
    (add-one-row
     (merge
      matrix
      {(list x y) (get-min matrix
                           x y
                           (nth horiz-char-list x)
                           (nth vert-char-list y)
                           )})
     (+ 1 x) y
     horiz-char-list vert-char-list)
    matrix))

(defn create-matrix [matrix j y horiz-char-list vert-char-list]
  "adds one row at a time to matrix for all rows up to y.
   assumes that matrix already has one row created with
   (create-initial-row)"
  ;; TODO: for consistency, do similar to add-one-row does:
  ;;  (check for existence of (get matrix [0,j])).
  (if (<= j y)
    (let [new-matrix
          (merge
           matrix
           (add-one-row matrix 0 j
                        horiz-char-list
                        vert-char-list))]
      (create-matrix new-matrix
                     (+ 1 j)
                     y
                     horiz-char-list
                     vert-char-list))
    matrix))

(defn create-initial-row [char-list-horiz char-list-vert
                          i
                          current-val]
  "i is an index into char-list-horiz"
  (if (> (.size char-list-horiz) 0)
    (merge
     {(list i 0) current-val}
     (create-initial-row (rest char-list-horiz)
                         char-list-vert
                         (+ i 1)
                         (+ current-val 1)
                         ))
    {}))

(defn tablize-row [matrix i j horiz-char-list vert-char-list]
  (if (get matrix (list i j))
    (str
     "<td>"
     (get matrix (list i j))
     "</td>"
     (tablize-row matrix (+ i 1) j horiz-char-list vert-char-list))
    ""))

(defn matrix-header [charlist]
  (if (> (.size charlist) 0)
    (str "<th>" (first charlist) "</th>"
         (matrix-header (rest charlist)))))

(defn tablize [matrix horiz-char-list vert-char-list j]
  (if (get matrix (list 0 j))
    (str
     "<tr>"
     "<th>"
     j
     "</th>"
     "<th>"
     ;; j'th element of vertical string.
     (nth vert-char-list j)
     "</th>"
     (tablize-row matrix 0 j horiz-char-list vert-char-list)
     "</tr>"
     (tablize matrix horiz-char-list vert-char-list (+ 1 j)))
    ""))

(defn matrix []
  (let [word1 "a large dog sits"
        word2 "a large cat eats"]
  {:italian word1
   :test (str "<table>"
              "<tr>"
              "<th colspan='2'> </th>"
              (matrix-header (range 0 (.size (explode word1))))
              "</tr>"
              "<tr>"
              "<th colspan='2'> </th>"
              (matrix-header (explode word1))
              "</tr>"
              (tablize (create-matrix
                        (create-initial-row
                         (explode word1) (explode word2) 0
                         (if (= (first (explode word1))
                                (first (explode word2)))
                           0
                           1))
                        1 (- (.size (explode word2)) 1)
                        (explode word1)
                        (explode word2))
                       (explode word1)
                       (explode word2) 0)
              "</table>")}))


