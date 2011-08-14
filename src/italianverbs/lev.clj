(ns italianverbs.lev
  (:require
   [clojure.contrib.string :as string]))

(defn create-matrix [matrix j y horiz-char-list vert-char-list]
  "adds one row at a time to matrix for all rows up to y.
   assumes that matrix already has one row created with
   (create-initial-row)"
  ;; TODO: for consistency, do similar to add-one-row does:
  ;;  (check for existence of (get matrix [0,j])).
  (let [add-one-row
        (fn ! [matrix x y horiz-char-list vert-char-list]
          "adds values for one whole row ([x:[0,end], y]) to the matrix."
          ;; TODO: use recur:
          ;; see http://clojure.org/functional_programming#Functional Programming--Recursive Looping
          (let [get-min (fn [matrix x y char-x char-y]
                          (let [diag
                                (if (= char-x char-y) 0 2)]
                            (min (if (> x 0)
                                   (+ 1 (get matrix (list (- x 1) y)))
                                   Float/POSITIVE_INFINITY)
                                 (if (> y 0)
                                   (+ 1 (get matrix (list x (- y 1))))
                           Float/POSITIVE_INFINITY)
                                 (if (and (> x 0) (> y 0))
                                   (+ diag
                                      (get matrix (list (- x 1) (- y 1))))
                                   Float/POSITIVE_INFINITY))))]
            (if (get matrix (list x (- y 1)))
              (!
               (merge
                matrix
                {(list x y) (get-min matrix
                                     x y
                                     (nth horiz-char-list x)
                                     (nth vert-char-list y)
                                     )})
               (+ 1 x) y
               horiz-char-list vert-char-list)
              matrix)))]
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
      matrix)))

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

(defn path-in-order-table [path]
  (if (> (.size path) 0)
    (str "<tr>" "<td>" (first path) "</td>" "</tr>"
         (path-in-order-table (rest path)))))

;        :print (if (and
;                    (= delete true)
;                    (= insert true))
;                 (str "[" (nth truth x) "/" (nth test y) "]")
;                 (if (= delete true)
;                   (str "[" (nth truth x) "/" "]")
;                   (if (= insert true)
;                     (str "[/" (nth test y) "]")
;                     (str (nth truth x)))))

(defn path-in-order [path pair truth test]
  (if pair
    (let [path-step (get path pair)
          next-pair (get path-step :next)
          next-step (get path next-pair)
          score (get path-step :score)
          x (first pair)
          y (second pair)
          next-x (first next-pair)
          next-y (second next-pair)
          incr (and
                next-pair
                (> (get (get path next-pair) :score)
                   score))
          delete (and (= incr true)
                      (> next-x x))
          insert (and (= incr true)
                      (> next-y y))
          ]
      (cons
       {:pair pair
        :score score
        :action (if (and
                     (= delete true)
                     (= insert true))
                  "subst"
                  (if (= delete true)
                    "delete"
                    (if (= insert true)
                      "insert"
                      "match")))
        :truth (nth truth x)
        :test (nth test y)
        }
       (path-in-order path next-pair truth test)))))

(defn tablize [matrix horiz-char-list vert-char-list j path]
  (let [tablize-row (fn ! [matrix i j horiz-char-list vert-char-list path]
                      (if (get matrix (list i j))
                        (str
                         "<td"
                         (if (get path (list i j))
                           (if (and
                                (=
                                 (nth horiz-char-list i)
                                 (nth vert-char-list j))

                                (or
                                 (and
                                  (= i 0)
                                  (= j 0))
                                 (get path
                                      (list (- i 1)
                                            (- j 1)))))

                             " class='corrent'"
                             " class='path'"))
                         ">"
                         (get matrix (list i j))
                         "</td>"
                         (! matrix (+ i 1) j horiz-char-list vert-char-list path))
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
       (tablize-row matrix 0 j horiz-char-list vert-char-list path)
       "</tr>"
       (tablize matrix horiz-char-list vert-char-list (+ 1 j) path))
      "")))

(defn next-candidates [min-key]
  (let [key (nth (first min-key) 0)
        x (first key)
        y (second key)]
    (if (and (> x 0)
             (> y 0))
      (list 

       (list (- x 1)
             (- y 1))


       (list x
             (- y 1))



       (list (- x 1)
             y))
      (if (> x 0)
        (list 
         (list (- x 1)
               y))
        (if (> y 0)
          (list
           (list x
                 (- y 1))))))))

(defn green [path current wordlist1 wordlist2]
  (if (get path current)
    (if (and
         (=
          (nth wordlist1 (first current))
          (nth wordlist2 (second current)))
         (or 
          (and 
           (= (first current) 0)
           (= (second current) 0))
          (get path (list (- (first current) 1)
                          (- (second current) 1)))))
    (cons (second current) ;; (second current) is index into wordlist2.
          (green path (get (get path current) :next) wordlist1 wordlist2))
    (green path (get (get path current) :next) wordlist1 wordlist2))))

(defn explode [string]
  "abc => (\"a\" \"b\" \"c\")"
  (rest (string/split (java.util.regex.Pattern/compile "") string)))
  
(defn matrix [word1 word2]
  (let [

        wordlist1 (explode word1)

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
         wordlist2)

        find-path
        (fn ! [matrix min-in-outer-set candidates]
          (let [find-min-in-set
                (fn ! [candidates min-key min-value matrix in-outer-set]
                  ;; initially: [*candidates* nil Float/POSITIVE_INFINITY *matrix* nil]
                  (if (> (.size candidates) 0)
                    (let [candidate (get matrix (first candidates))
                          current-min-x (first min-key)
                          current-min-y (second min-key)
                          candidate-x (first (first candidates))
                          candidate-y (second (first candidates))]
                      ;; replace current min if it's better.
                      ;; 'candidate' is the (potentially smaller) value we are
                      ;; testing against current minimum 'min-value'.
                      ;; 'candidate''s 'x' value must be smaller than current min-value's
                      ;; 'x' value.
                      (if
                          (or 
                           (> min-value candidate)
                           false)
                        
                        ;; true: replace current minimum value with current candidate;
                        ;; continue testing other candidates with new current minimum.
                        (! (rest candidates) (first candidates) candidate matrix in-outer-set)
                        
                        ;; false: reject current candidate; continue with existing minimum.
                        (! (rest candidates) min-key min-value matrix in-outer-set)))
                    
                    {min-key {:next (nth (first in-outer-set) 0)
                              :score min-value}}))]
            (if (and
                 candidates
                 (> (.size candidates) 0))
              (let [
                    min-in-this-set
                    (find-min-in-set candidates nil Float/POSITIVE_INFINITY matrix min-in-outer-set)
                    ]
                (merge
                 min-in-this-set
                 (! matrix min-in-this-set (next-candidates min-in-this-set)))))))
          
        path (find-path matrix nil
                        (list (list (- (.size wordlist1) 1) (- (.size wordlist2) 1))))

        ]

    {:italian word1 ;; not necessarily :italian, but :italian is displayed at top of feature structure.
     :green (green path (list 0 0) wordlist1 wordlist2)
     :path-table (str "<table>"
                      (path-in-order-table (path-in-order path (list 0 0) wordlist1 wordlist2))
                      "</table>")
     :path (path-in-order path (list 0 0) wordlist1 wordlist2)
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
                 wordlist2 0 path)
                "</table>")}))

(defn get-green [word1 word2]
  "convenient function for external usage."
  (let [matrix
        (matrix word1 word2)]
    (get matrix :green)))

(defn get-green2 [word1 word2]
  "convenient function for external usage."
  (let [matrix
        (matrix word1 word2)]
    (get matrix :path)))

;(defn test []
;  (matrix "un'uomo va in Roma"
;          "gli uomini vanno a Roma"))

(defn test []
  (matrix "voi accendete"
          "voi acete noi"))
