;; NO RESTARTING OF RING REQUIRED FOR CHANGES TO THIS FILE. (must reload browser 2x though).
(ns italianverbs.grammar
  (:use [somnium.congomongo])
  (:require
   [italianverbs.morphology :as morph]
   [clojure.string :as string]))

(defn right [head comp]
  {:head head
   :comp comp
   :english (string/join " "
                         (list 
                          (get comp :english)
                          (get head :english)))
   :italian (string/join " "
                         (list 
                          (get comp :italian)
                          (get head :italian)))
   :children (list comp head)})

(defn left [head comp]
  {:head head
   :comp comp
   :english (string/join " "
                         (list 
                          (get head :english)
                          (get comp :english)))

   :italian (string/join " "
                         (list 
                          (get head :italian)
                          (get comp :italian)))
   :children (list head comp)})

(defn combine-error [head comp]
  {:cat :error
   :notes "no function found to combine head and comp."
   :children (list head comp)})

;; head-position is 'left or 'right.
(defn combine [head comp fn]
  (merge
   (apply fn (list head comp))
   {:head head
    :comp comp
    :def (get comp :def)
    }
   ;; following are all features copied from head to parent: {det,number,gender}
   (if (get head :det)
     {:det (get head :det)})
   (if (get head :number)
     {:number (get head :number)})
   (if (get head :gender)
     {:gender (get head :gender)})

   ;; following is all features copied from complement to parent..
   (if (get comp :def)
     {:def (get comp :def)})))



;; TODO: use (morph/get-head) instead.
(defn gramhead [sign]
  (if (get sign :head)
    (get sign :head)
    sign))

(defn unify-np [head arg]
  (if (and
       (= (get (gramhead head) :gender)
          (get (gramhead arg) :gender))
       (= (get (gramhead head) :number)
          (get (gramhead arg) :number)))
    {
     :head head
     }
    {
     :cat :fail
     ;; TODO: rewrite as (defn diagnosis [head arg])
     :note (str (get head :gender) " != " (get arg :gender)
                " or "
                (get head :number) " != " (get arg :number))
     }))

(defn noun-fn [head arg]  ;; e.g. "il libro"
  (merge
   (unify-np head arg)
   {:english
    (morph/conjugate-en head arg)
    :italian
    (string/join " "
                 (list (get arg :italian)
                       (morph/conjugate-it head)))}))

(defn choose-lexeme [struct & [dummy]]
  "Choose a random lexeme from the set of lexemes
   that match search criteria.
   dummy: ignored for compatibility with gram/np"
  ;; do a query based on the given struct,
  ;; and choose a random element that satisfies the query.
  (let [results (fetch :lexicon :where struct)]
    (if (= (count results) 0)
      {:english "??" :italian "??"
       :cat :error :note (str "<tt>(choose-lexeme)</tt>: no results found. <p/>See <tt>:choose</tt> feature below for query.")
       :choose struct
       }
      (nth results (rand-int (count results))))))

"find a function which might really be a function, or might be a string that
 needs to be converted to a function whose name is that string."
(defn find-fn [fn]
  (cond
   (nil? fn)
   {:cat :error :note
    (str "function is null")}
   (string? fn)
   (symbol fn)
   true fn))

  
(defn np [ & [fs determiner]]
  "'fs' puts pre-conditions on noun (head of the np)"
  (let [chosen-determiner determiner
        noun (choose-lexeme (merge fs {:cat :noun
                                       }))
        determiner-search
        (if (not (= (get noun :det) nil))
          (merge
           (get noun :det)
           {:cat :det
            :gender (get noun :gender)
            :number (get noun :number)}))
        determiner (if (not (= (get noun :det) nil))
                     (choose-lexeme determiner-search))]
    (if determiner
      (merge 
;       {:choose-comp determiner-search}
       (combine noun determiner right)
       {:italian (morph/italian-article determiner noun)})
      noun)))

(defn n-bar [noun adjective]
  {:italian (string/join " "
                         (list 
                          (get adjective :italian)
                          (get noun :italian)))
   :english (string/join " "
                         (list 
                          (get adjective :english)
                          (get noun :english)))
   :head noun
   :gender (get noun :gender)
   :number (get noun :number)
   :comp adjective
   })

(defn np-det-n-bar [n-bar det]
  {:italian (string/join " "
                         (list 
                          (get det :italian)
                          (get n-bar :italian)))
   :english (string/join " "
                         (list 
                          (get n-bar :english)))
   :gender (get n-bar :gender)
   :number (get n-bar :number)
   :head n-bar
   :comp det
   })

  
(def np-with-common-noun-and-definite-pronoun
  (fn [candidate]
    (and (not (= (get candidate :pronoun) true)) ;; e.g. "noi (us)"
         (not (= (get candidate :det) nil)) ;; e.g. "Italia (Italy)"
         (= (get candidate :def) "def"))))

(defn np-with-post-conditions [ & [head-conditions post-conditions keep-trying]]
  ;; forgot how to pass default params, so doing this (let) instead.
  (let [default-limit 10
        keep-trying (if (not (= keep-trying nil))
                      keep-trying
                      default-limit)]
    (let [candidate (np head-conditions)]
      (if (apply post-conditions
                 (list candidate))
        candidate
        (if (= keep-trying 0)
          {:cat :error
           :note (str "gave up trying to generate after " default-limit " attempts.")
           :notefs candidate
           }
          (np-with-post-conditions head-conditions post-conditions (- keep-trying 1)))))))

(defn verb-sv [head comp]  ;; e.g. "i [sleep]","he [writes a book]"
  (cond
   ;; unfortunately we have to check
   ;; for either the :-form or the quoted-string below:
   (or (= (get (morph/get-head comp) :cat) :noun)
       (= (get (morph/get-head comp) :cat) "noun")
       (= (get (morph/get-head comp) :cat) :pronoun)
       (= (get (morph/get-head comp) :cat) "pronoun"))

   {:fn "verb-sv"
    :english
    (string/join " "
		 (list 
		  (get comp :english)
		  (morph/conjugate-english-verb (morph/get-head head) comp)
		  (get (get head :comp) :english)))
    :italian
    (string/join " "
		 (list
		  (get comp :italian)
		  (morph/conjugate-italian-verb head comp)
          (get (get head :comp) :italian)))}
   (= (get (morph/get-head comp) :cat) "prep")
   {:fn "verb-sv"
    :head head
    :comp comp
    :italian
    (str
     (get head :italian)
     " "
     (get comp :italian))
     :english
    (str
     (get head :english)
     " "
     (get comp :english))}
   true
   {:cat :error
    :note (str
           "<tt><i>error: verb does not know what to do with this argument.</i>(<b>verb-sv</b> "
           "'" (get head :english) "','" (get comp :english) "'"
           ")</i>."
           "<p>get-head comp :cat=" (get (morph/get-head comp) :cat) "</p>"
           "</tt>")}))

(defn pp [ & [fs obj]]
  "generate a prepositional phrase.
   fs adds restrictions on prep.
   obj is simply an object for the preposition."
  (let [prep (if (get fs :already-looked-up)
               fs
               (choose-lexeme (merge fs {:cat :prep})))]
    (let [np (if obj obj
                 (np (get prep :obj)))]
      (merge 
;       {:choose-head prep}
;       {:choose-comp np}
;       {:given-an-obj (if obj true false)}
; ^^for debugging : comment-out if not needed.
       (combine prep np left)
       {:italian (morph/conjugate-italian-prep prep np)}))))

(defn sv [head comp]
  (merge
   (right head comp)
   {:english (string/join " "
                          (list (get comp :english)
                                (morph/conjugate-english-verb head comp)))
    :italian (string/join " "
                          (list (get comp :italian)
                                (morph/conjugate-italian-verb head comp)))}))

(defn vo [head comp]
  (left head comp))

(defn vp-pp [head comp]
  (left head comp))

(defn det-n [head comp]
  (right head comp))

(defn choose-iobject [verb]
  (pp (get verb :iobj)))

(defn vp [ & [fs]]
  (let [verb-fs (merge
                 fs
                 {:cat :verb
                  :infl :infinitive})
        verb (nth (fetch :lexicon :where verb-fs)
                  (rand-int (count (fetch :lexicon :where verb-fs))))
        object
        (cond
         (= (get (get verb :obj) :cat) "noun")
         (np (merge {:case {:$ne :nom}}
                    (get verb :obj)))
         (= (get (get verb :obj) :cat) "verb")
         (vp (get verb :obj))
         true nil)
        verb-with-object (if object
                           (combine verb object vo)
                           verb)
        verb-with-iobject (if (get verb :iobj)
                            (combine verb-with-object (choose-iobject verb) vo)
                            verb-with-object)]
    verb-with-iobject))

(defn vp-with-adjunct-pp [ & [fs]]
  (let [vp (vp fs)]
    (combine vp
             (pp (get (morph/get-head vp) :adjunct))
             vp-pp)))

(defn sentence []
  (let [vp (vp-with-adjunct-pp)]
    (let [subject
          (np
           (merge
            {:case {:$ne :acc}}
            (get (morph/get-root-head vp) :subj)))]
      (if vp
        (combine vp subject sv)
        {:cat :error
         :error "vp-with-adjunct-pp returned null."}))))

;; TODO: move to lexicon (maybe).
(defn italian-number [number]
  (cond
   (= number 1) "una"
   (= number 2) "due"
   (= number 3) "tre"
   (= number 4) "quattro"
   (= number 5) "cinque"
   (= number 6) "sei"
   (= number 7) "sette"
   (= number 8) "otto"
   (= number 9) "nove"
   (= number 10) "dieci"
   (= number 11) "undici"
   (= number 12) "dodici"
   (= number 13) "tredici"
   (= number 14) "quattordici"
   (= number 15) "quindici"
   (= number 16) "sedici"
   (= number 17) "diciassette"
   (= number 18) "diciotto"
   (= number 19) "diciannove"
   
   ;; ...
   (= number 20) "venti"
   (< number 30) (str (italian-number 20) (italian-number (- number 20)))
   (= number 30) "trenta"
   (< number 40) (str (italian-number 30) (italian-number (- number 30)))
   true "??"))

(defn italian-time [hour minute ampm]
  (let [print-hour
        (if (<= minute 30)
          (italian-number hour)
          (italian-number
           (if (= hour 12)
             1
             (+ hour 1))))]
    (str
     (cond
      (and (= print-hour 12)
           (= ampm "am"))
      "mezzogiorno"
      (and (= print-hour 12)
           (= ampm "pm"))
      "mezzonotte"
      true (morph/italian-article {:italian "le" :def :def} {:number :singular :italian print-hour :numerical true}))
     (cond
      (= minute 0) ""
      (= minute 15) " e un quarto"
      (= minute 30) " e mezzo"
      (= minute 45) " meno un quarto"
      (<= minute 30)
      (str " e " (italian-number minute))
      true (str " meno "(italian-number (- 60 minute)))))))

(defn english-time [hour minute ampm]
  (str hour ":" (if (< minute 10) (str "0" minute) minute) " " (if (= hour 12) (if (= ampm "am") " after midnight" " after noon") "")))

(defn random-present []
  (let [verb-inf (choose-lexeme {:cat :verb :infl :infinitive})
        ;; TODO: more complicated matching: i.e. {:root verb-inf}
        verb (choose-lexeme {:cat :verb :infl :present
                             :root.italian (get verb-inf :italian)
                             })
        subject (cond
                 (or (= (get verb :person) "1st")
                     (= (get verb :person) "2nd"))
                 (choose-lexeme
                  (merge {:case {:$ne :acc}
                          :cat :noun
                          :person (get verb :person)
                          :number (get verb :number)}
                         (get (get verb :root) :subj)))
                 true
                 (np
                  (merge
                   {:case {:$ne :acc}}
                   {:number (get verb :number)}
                   {:person (get verb :person)}
                   (get (get verb :root) :subj))))]
    (merge
     {:verb-inf verb-inf
      :verb verb
      :subject subject
      :english (str (get subject :english) " " (morph/conjugate-english-verb verb-inf subject))
      :italian (str (get subject :italian) " " (get verb :italian))}
    {:type-is-fs (set '(:verb :subject :verb-inf))})))
  
(defn random-passato-prossimo []
  (let [
        verb-past (choose-lexeme {:root.cat :verb :infl :passato-prossimo})
        verb-inf (choose-lexeme {:cat :verb :infl :infinitive :italian (get verb-past :aux)})
        ;; TODO: more complicated matching: i.e. {:root verb-inf}
        verb-aux (choose-lexeme {:cat :verb :infl :present
                                 :root.italian (get verb-inf :italian)
                                 })
        subj-constraints
        (merge
         {:cat :noun
          :case {:$ne :acc}}
         (get (get verb-past :root) :subj)
         (get verb-inf :subj)
         {:person (get verb-aux :person)
          :number (get verb-aux :number)})
        subject (cond
                 (or (= (get verb-aux :person) "1st")
                     (= (get verb-aux :person) "2nd"))
                 (choose-lexeme subj-constraints)
                 true
                 (np subj-constraints))]
    (merge
     {:verb-inf verb-inf
      :verb-aux verb-aux
      :verb-past verb-past
      :subject subject
      :subj-constraints subj-constraints
      :english (str (get subject :english) " "
                    ;(morph/conjugate-english-verb verb-inf subject) " "
                    (get verb-past :english))
      :italian (str (get subject :italian) " " (get verb-aux :italian) " " (get verb-past :italian))}
    {:type-is-fs (set '(:verb-past :subject :verb-inf :subj-constraints :verb-aux))})))
  