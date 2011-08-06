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


(defn matrix-header [charlist]
  (if (> (.size charlist) 0)
    (str "<th>" (first charlist) "</th>"
         (matrix-header (rest charlist)))))

(defn tablize [matrix horiz-char-list vert-char-list j]
  (let [tablize-row
        (fn [matrix i j horiz-char-list vert-char-list]
          (if (get matrix (list i j))
            (str
             "<td>"
             (get matrix (list i j))
             "</td>"
             (tablize-row matrix (+ i 1) j horiz-char-list vert-char-list))
            ""))]
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
      "")))

(defn interpret-op [op curr diag horiz-char-list vert-char-list x y]
  (if (= op nil)
    (if (= (nth horiz-char-list 0)
           (nth vert-char-list 0))
      (list '/ (nth horiz-char-list 0))
      (list 'x (nth horiz-char-list 0)
           (nth vert-char-list 0) ))
    (if (= op "/")
      (list '/ (nth horiz-char-list x) )
      (if (= op "x")
        (list 'x
             (nth horiz-char-list x)
             (nth vert-char-list y)
             )
        (if (= op "|")
          (list '|
               (nth vert-char-list y)
               )
          (if (= op "-")
            (list '-
                 (nth horiz-char-list x)
                 )
            "??"))))))

(defn find-min-path-up [matrix horiz-char-list vert-char-list]
  (let [x (- (.size horiz-char-list) 1)
        y (- (.size vert-char-list) 1)]
    (if (or (> x 0)
            (> y 0))
      (let [curr (get matrix (list x y))
            up (if (> y 0)
                 (get matrix (list x y)))
            left (if (> x 0)
                   (get matrix (list x y)))
            diag (if (and (> x 0)
                          (> y 0))
                   (get matrix (list (- x 1) (- y 1))))]
        (if (and diag left up
                 (< diag left)
                 (< diag up))
          (cons
           (list '/
                 (nth horiz-char-list (- x 1))
                 (nth vert-char-list (- y 1)))
           (find-min-path-up matrix
                             (butlast horiz-char-list)
                             (butlast horiz-char-list)))
          (list 'done diag left up x y
                (< diag left)
                (< diag up)))))))

(defn find-minimum-path [matrix x y horiz-char-list vert-char-list op] 
  (let [curr (get matrix (list x y))
        down (get matrix (list x (+ y 1)))
        right (get matrix (list (+ x 1) y))
        diag (get matrix (list (+ x 1) (+ y 1)))
        interpret-op
        (interpret-op op
                      curr
                      diag
                      horiz-char-list
                      vert-char-list x y)
        ]
    ;; handle nulls first.
    (if (and (= nil down)
             (= nil right))
      (list interpret-op)
      (if (= nil down)
        (cons interpret-op
              (find-minimum-path matrix
                                 (+ x 1)
                                 y
                                 horiz-char-list
                                 vert-char-list
                                 "-"))
        (if (= nil right)
          (cons interpret-op
                (find-minimum-path matrix
                                  x
                                  (+ y 1)
                                  horiz-char-list
                                  vert-char-list
                                  "|"
                                  ))
          ;; neither down nor right are null; so
          ;; neither is diag.
          (if (and (< diag right)
                   (< diag down))
            (cons
             interpret-op
             (find-minimum-path matrix
                                (+ x 1)
                                (+ y 1)
                                horiz-char-list
                                vert-char-list
                                (if (= curr diag)
                                  "/"
                                  "x")))
            (if (< down right)
              (cons interpret-op
                    (find-minimum-path matrix
                                      x
                                      (+ y 1)
                                      horiz-char-list
                                      vert-char-list
                                      "|"
                                      ))
              (if (< right down)
                (cons interpret-op
                      (find-minimum-path matrix
                                         (+ x 1)
                                         y
                                         horiz-char-list
                                         vert-char-list
                                         "-"
                                         ))
                (if (<= diag right)
                  (cons interpret-op
                        (find-minimum-path matrix
                                           (+ x 1)
                                           (+ y 1)
                                           horiz-char-list
                                           vert-char-list
                                           (if (= curr diag)
                                             "/"
                                             "x")))
                  (cons interpret-op
                        (find-minimum-path matrix
                                           (+ x 1)
                                           y
                                           horiz-char-list
                                           vert-char-list
                                           "-")))))))))))

(defn score [path]
  (if (first path)
    (let [elem (first path)
          elem-score 
          (if (= (first elem) '/)
            0
            1)]
    (+
     elem-score
     (score (rest path))))
    0))

(defn matrix [word1 word2]
  (let [wordlist1 (explode word1)
        wordlist2 (explode word2)
        matrix
        (create-matrix
         (create-initial-row
          wordlist1 wordlist2 0
          (if (= (first wordlist1)
                 (first wordlist2))
            0
            1))
         1 (- (.size wordlist2) 1)
         wordlist1
         wordlist2)]
    {:italian word1
     :path (find-min-path-up matrix wordlist1 wordlist2)
     :test (str "<table class='matrix'>"
                "<tr>"
                "<th colspan='2'> </th>"
                (matrix-header (range 0 (.size wordlist1)))
                "</tr>"
                "<tr>"
                "<th colspan='2'> </th>"
                (matrix-header wordlist1)
                "</tr>"
                (tablize
                 matrix
                 wordlist1
                 wordlist2 0)
                "</table>")}))

(defn test []
  (matrix "abcdxyz"
          "xyzabcd"))

