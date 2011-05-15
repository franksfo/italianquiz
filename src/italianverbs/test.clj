 (ns italianverbs.test
    (:use 
     [hiccup core page-helpers]
     [somnium.congomongo])
    (:require
     [italianverbs.html :as html]
     [italianverbs.lexiconfn :as lexfn]
     [italianverbs.grammar :as gram]
     [italianverbs.generate :as gen]
     [clojure.string :as string]
     [italianverbs.quiz :as quiz]))


;; <test definitions>

;; each testN must specify a testN-fn, a testN-head, and testN-comp.
;; 2 choices for testN-fn.
;; gram/choose-lexeme : choose a random lexeme based on choose-head which is a feature structure.
;;                      choose-comp-lexeme is ignored.
;; gram/np : generate a np with head based on choose-head,
;;           det based on choose-comp-lexeme.


;; test1: singular NPs without determiners. (e.g. "Italia (Italy)")
(def test1-fn gram/np)
(def test1-head {:cat :noun
                 :det nil
                 :pronoun {:$ne true}
                 :number :singular})
(def test1-comp {:number :singular
                 :cat :det})

;; test2: plural NPs with determiners. (e.g. "i cani (the dogs)")
(def test2-fn gram/np)
(def test2-head {:cat :noun
                 :pronoun {:$ne true}
                 :number :plural})
(def test2-comp {:number :plural
                 :cat :det})

;; test3: months of the year
(def test3-fn gram/choose-lexeme)
(def test3-head {:month true})
(def test3-comp nil)

;; test4 : furniture.
(def test4-fn gram/np)
(def test4-head {:cat :noun
                 :furniture true
                 :pronoun {:$ne true}
                 })
(def test4-comp nil);{:cat :det})

;; test5 : furniture np-as-complement-for-prep-phrase
(def test5-fn gram/np)
(def test5-head {:cat :noun
                 :furniture true})
(def test5-comp {:def :def})

;; def test5 lets us use the results of test5 in subsequent tests, e.g. test6.
(def test5
  (merge {:test "plural NPs that are :def"} 
         (apply test5-fn (list test5-head test5-comp))))

;; test6 : prepositional phrases about furniture.
(def test6-fn gram/pp)

(def test6-head
  (merge
   {:already-looked-up true}
   (gram/choose-lexeme
    {
     :cat :prep
     ;; this does not seem to work: (nested selection path obj->furniture.)
                                        ;     :obj {:furniture true}})
                                        ; so using the following instead (as workaround).
     :furniture-prep true})))
                 
(def test6-comp 
  (gram/np-with-post-conditions 
    (get test6-head :obj)
    (defn fn [fs]
      (= (get fs :def) "def"))))

(def test6
  (merge {:test "furniture PPs"}
         (apply test6-fn (list test6-head test6-comp))))

(def test7-fn gram/vp-pp)

(def test7-head
  (gram/choose-lexeme
   {:cat :verb
    :italian "essere"}))

(def test7-comp test6)

(def test7
  (merge {:test "furniture VPs"}
         (apply test7-fn (list test7-head test7-comp))))
  
(def test8
   (let [fn gram/sv
         head
         (let [fn gram/vp-pp
               head (gram/choose-lexeme
                     {:cat :verb
                      :italian "essere"})
               comp
               (let [fn gram/pp
                     head (merge
                           {:already-looked-up true}
                           (gram/choose-lexeme
                            {:cat :prep
                             :furniture-prep true}))
                     comp (gram/np-with-post-conditions 
                            (get head :obj)
                            (defn fn [fs]
                              (= (get fs :def) "def")))]
                 (apply fn (list head comp)))]
           (apply fn (list head comp)))
         comp
         (gram/np-with-post-conditions 
           {:furniture true}
           (defn fn [fs]
             (= (get fs :def) "def")))]
     (merge {:test "furniture sentences"}
            (apply fn (list head comp)))))

;;  "possessive NPs"
(def test9
  (let [fn gram/n-bar
        head (gram/choose-lexeme
              {:cat :noun
               :common true
               :number :singular})
        comp (gram/choose-lexeme
              {:cat :adj
               :gender (get head :gender)
               :possessive true})]
    (merge {:test "possessive NPs"}
           (apply fn (list head comp)))))

(def test10
  (let [fn gram/np-det-n-bar
        head
        (let [fn gram/n-bar
              head (gram/choose-lexeme
                    {:cat :noun
                     :common true
                     :number :singular})
              comp (gram/choose-lexeme
                    {:cat :adj
                     :gender (get head :gender)
                     :possessive true})]
          (merge {:test "possessive NPs"}
                 (apply fn (list head comp))))
        comp (gram/choose-lexeme
              {:cat :det
               :gender (get head :gender)
               :number (get head :number)
               :def :def})]
    (merge {:test "det-n-bar"}
           (apply fn (list head comp)))))
            
(def test11
  (let [hour (+ 1 (rand-int 1))   ;; {1,2,3,...12}
        minute (* (rand-int 5) 5) ;; {0,5,10,...55}
        ampm (if (= (rand-int 2) 0)
               "am"
               "pm")
        hour (if (= hour 0) 12 hour)] ;; or do mod 12 + 1.
    {:test "che-ora"
     :english (gram/english-time hour minute ampm)
     :italian (gram/italian-time hour minute ampm)
     :hour hour
     :minute minute}))

(def test12
  (merge 
   (gen/random-passato-prossimo)
   {:test "passivo partato"}))

;; apply library functions: will move elsewhere after testing.
(defn show-answer [question] (get question :answer))
(defn wrap-div [string]
  (str "<div class='test'>" string "</div>"))

(defn correct []
  (str "correct guesses: " (count (mapcat quiz/each-correct (fetch :question)))
       " out of : " (count (fetch :question))))

;; fixme: change name to "compose-sv-sentence" or something.
(defn conjugate [pronoun infinitive]
    (gram/combine infinitive pronoun 'right))

(defn io-pranzo []
  (gram/combine (lexfn/lookup "pranzare")
           (lexfn/lookup "io" {:case {:$ne :acc}}) 'right))

(defn lui-scrivo-il-libro []
  (let [subject (lexfn/lookup "lui" {:case {:$ne :acc}})
        object (gram/combine
                (lexfn/lookup "libro")
                (lexfn/lookup "il") 'right gram/det-n)
        verb-phrase (gram/combine (lexfn/lookup "scrivere") object 'left gram/vo)]
    (gram/combine verb-phrase subject 'right)))

(def in-italia
  (let [prep (lexfn/lookup "in")
	noun (lexfn/lookup "Italia")]
    (gram/combine
     prep noun 'left)))

(def andare-in-italia
  (gram/combine (lexfn/lookup "andare")
           in-italia 'left))

(defn lui-vado-in-italia []
  (gram/combine
   (gram/combine
    (lexfn/lookup "andare") in-italia 'left)
   (lexfn/lookup "lui" {:case {:$ne :acc}}) 'right))

(defn io-mangio-il-pane []
  (let [subject (lexfn/lookup "io" {:case {:$ne :acc}})
        object (gram/combine
                (lexfn/lookup "pane")
                (lexfn/lookup "il") 'right)
        verb-phrase (gram/combine (lexfn/lookup "mangiare")
                             object 'left)]
    (gram/combine verb-phrase subject 'right)))

(defn lui-mangio-la-pasta-in-italia []
  (let [subject (lexfn/lookup "lui" {:case {:$ne :acc}})
        object (gram/combine
                (lexfn/lookup "pasta")
                (lexfn/lookup "la") 'right)
        verb-phrase (gram/combine (lexfn/lookup "mangiare")
                             object
                             'left)]
    (gram/combine 
     (gram/combine verb-phrase subject 'right)
     in-italia 'left)))

(defn io-scrivo-il-libro []
  (let [subject (lexfn/lookup "io")
	object (gram/combine
		(lexfn/lookup "libro")
		(lexfn/lookup "il"))
	verb-phrase (gram/combine (lexfn/lookup "scrivere")
			     object)]
    (gram/combine verb-phrase subject)))

(defn reload-button []
  (str "<form action='/test/' method='post'><input type='submit' value='Reload'/>  </form> "))

(defn bugs []
  nil)

(defn conjugation [verb] ;; verb should be the infinitive form of a verb.
  (str
   "<div class='conjugation'>"
   (html/tablize verb)
   "<table class='fs conjugation'>"
   "<tr>"
   "<th>io</th>"
   "<td>"
   (get (conjugate (lexfn/lookup "io") verb) :italian)
   "</td>"
   "</tr>"
   "<tr>"
       "<th>tu</th>"
       "<td>"
       (get (conjugate (lexfn/lookup "tu") verb) :italian)
       "</td>"
       "</tr>"
       "<tr>"
       "<th>lui/lei</th>"
       "<td>"
       (get (conjugate (lexfn/lookup "lui") verb) :italian)
       "</td>"
       "</tr>"
       "<tr>"
       "<th>noi</th>"
       "<td>"
       (get (conjugate (lexfn/lookup "noi") verb) :italian)
       "</td>"
       "</tr>"
       "<tr>"
       "<th>voi</th>"
       "<td>"
       (get (conjugate (lexfn/lookup "voi") verb) :italian)
       "</td>"
       "</tr>"
       "<tr>"
       "<th>loro</th>"
       "<td>"
       (get (conjugate (lexfn/lookup "loro") verb) :italian)
       "</td>"
       "</tr>"
       "</table>"
       "</div>"
       ))

(defn conjugations []
  (list 
   "<div class='section'> <h2>conjugations</h2></div>"
   (conjugation (lexfn/lookup "andare"))
   (conjugation (lexfn/lookup "volare"))
   (conjugation (lexfn/lookup "fare"))
   (conjugation (lexfn/lookup "venire"))
   (conjugation (lexfn/lookup "dire"))))

(defn random-sentences-1 [num generate-fn head comp]
  (if (> num 0)
    (cons

     (html/tablize (apply generate-fn (list head
                                            (if (and (get comp :type) (= (get comp :type) "combine()d"))
                                              comp
                                              (gram/choose-lexeme comp nil)))))
     (random-sentences-1 (- num 1) generate-fn head comp))))

(defn random-sentences [num generate-fn head comp]
  (list
   (random-sentences-1 num generate-fn head comp)))

(def tests
  (list
;   (reload-button) ; reload button does not work yet (results are still cached)

   ;(bugs)

   ;(conjugations)

   (html/tablize
    (if false
      (merge
       {:test "sentence"}
       (gram/sentence))))

   (if false
     (html/tablize
      (merge 
       (gen/random-present)
       {:test "present"})))

   (html/tablize test12)
;   (html/tablize test11)
;   (html/tablize test10)
;   (html/tablize test8)
;   (html/tablize test7)
;   (html/tablize test6)
;   (html/tablize test5)
   ))
