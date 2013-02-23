(ns italianverbs.grammar
  (:require
   [italianverbs.fs :as fs]
   [italianverbs.morphology :as morph]
   [italianverbs.lexiconfn :as lexfn]
   [clojure.string :as string]))

;;    [1]
;;   /   \
;;  /     \
;; H[1]    C
(def head-principle
  (let [head-cat (ref :top)
        head-sem (ref :top)
        head-infl (ref :top)]
    {:synsem {:cat head-cat
              :sem head-sem
              :infl head-infl}
     :head {:synsem {:cat head-cat
                     :infl head-infl
                     :sem head-sem}}}))

;;     subcat<>
;;     /      \
;;    /        \
;; H subcat<1>  C[1]
(def subcat-1-principle
  (let [comp-synsem (ref :top)]
    {:subcat '()
     :head {:synsem {:subcat {:1 comp-synsem}}}
     :comp {:synsem comp-synsem}}))

;;     subcat<1>
;;     /      \
;;    /        \
;; H subcat<1,2>  C[2]
(def subcat-2-principle
  (let [comp-synsem (ref :top)
        parent-subcat (ref :top)]
    {:synsem {:subcat {:1 parent-subcat}}
     :head {:synsem {:subcat {:1 parent-subcat
                              :2 comp-synsem}}}
     :comp {:synsem comp-synsem}}))


;; a language's morphological inflection is
;; identical to its head's SYNSEM|INFL value.
(def verb-inflection-morphology
  (let [infl (ref :top)]
    {:italian {:infl infl}
     :english {:infl infl}
     :head {:synsem {:infl infl}}}))

(def vp-rules
  (let [head (ref :top)
        comp (ref :top)]

    (def vp-past
      (fs/unifyc head-principle
                 subcat-2-principle
                 verb-inflection-morphology
                 {:head {:synsem {:cat :verb
                                  :infl :past}}}
                 {:comment "vp[past] &#x2192; head comp"
                  :head head
                  :comp comp
                  :1 head
                  :2 comp
                  :extend {:a {:head 'past-transitive-verbs
                               :comp 'np}
                           :b {:head 'past-intransitive-verbs}}}))

    (def vp-present
      (fs/unifyc head-principle
                 subcat-2-principle
                 verb-inflection-morphology
                 {:head {:synsem {:cat :verb
                                  :infl :present}}}
                 {:comment "vp[present] &#x2192; head comp"
                  :head head
                  :comp comp
                  :1 head
                  :2 comp
                  :extend {
                           :a {:head 'present-transitive-verbs
                               :comp 'np}
                           :b {:head 'present-aux-verbs
                               :comp 'vp-past}
                           }}))


    (def vp-future
      (fs/unifyc head-principle
                 subcat-2-principle
                 verb-inflection-morphology
                 {:head {:synsem {:cat :verb
                                  :infl :futuro}}}
                 {:comment "vp[future] &#x2192; head comp"
                  :head head
                  :comp comp
                  :1 head
                  :2 comp
                  :extend {
                           :a {:head 'future-transitive-verbs
                               :comp 'np}}}))

    (list vp-present vp-past vp-future)))

(def sentence-rules
  (let [subj-sem (ref :top)
        subcatted (ref {:cat :noun
                        :subcat '()
                        :sem subj-sem})
        infl (ref :top)
        present (ref :present)
        future (ref :futuro)
        comp (ref {:synsem subcatted})
        head (ref {:synsem {:cat :verb
                            :sem {:subj subj-sem}
                            :subcat {:1 subcatted
                                     :2 '()}}})]
    (list
     ;; present
     (fs/unifyc head-principle subcat-1-principle
                {:synsem {:infl present}}
                {:comment "sentence (present) (4 subrules)"
                 :head head
                 :comp comp
                 :1 comp
                 :2 head
                 :extend {
                          :a {:comp 'np
                              :head 'vp-present}
                          :b {:comp 'pronouns
                              :head 'vp-present}
                          :c {:comp 'np
                              :head 'present-intransitive-verbs}
                          :d {:comp 'pronouns
                              :head 'present-intransitive-verbs}
                          }})
     ;; future
     (fs/unifyc head-principle subcat-1-principle
                {:synsem {:infl future}}
                {:comment "sentence (future) (4 subrules)"
                 :head head
                 :comp comp
                 :1 comp
                 :2 head
                 :extend {
                          :a {:comp 'np
                              :head 'future-intransitive-verbs}
                          :b {:comp 'pronouns
                              :head 'future-intransitive-verbs}
                          :c {:comp 'np
                              :head 'vp-future}
                          :d {:comp 'pronouns
                              :head 'vp-future}
                          }}))))


(def adj-rules
  (let [sem (ref :top)
        head (ref :top)
        comp (ref :top)
        comp-sem-pred (ref :top)
        subcat (ref :top)
        gender (ref :top)
        number (ref :top)
        agr (ref :top)]
    (def nbar
      (fs/unify
       head-principle
       {:1 head}
       {:2 comp}
       {:synsem {:subcat subcat}}
       {:head {:synsem {:subcat subcat}}}
       {:synsem {:sem sem}}
       {:synsem {:sem {:mod comp-sem-pred}}}
       {:head head
        :comp comp}
       {:head {:synsem {:cat :noun
                        :agr agr}}
        :comp {:italian {:agr agr}
               :english {:agr agr}}
        :synsem {:agr agr}}
       {:comp {:synsem {:cat :adjective
                        :sem {:pred comp-sem-pred}}}}
       {:comp {:synsem {:cat :adjective
                        :sem {:mod sem}}}}
       {:comment "n&#x0305; &#x2192; adj noun"
        :extend {:a {:head 'nouns
                     :comp 'adjectives}}}))
    (list nbar)))

(def np-rules 
  (let [head (ref :top)
        comp (ref :top)]
    (def np1
      (fs/unifyc head-principle subcat-1-principle ;; NP -> Comp Head
                 (let [agr (ref :top)]
                   (fs/unifyc
                    {:head {:synsem {:cat :noun
                                     :agr agr}}
                     :synsem {:agr agr}}
                    {:comment "np &#x2192; det noun"
                     :synsem {:agr agr}
                     :head head
                     :comp comp
                     :1 comp
                     :2 head
                     :extend {
                              :a {:comp 'determiners
                                  :head 'nouns}
                              :b {:comp 'determiners
                                  :head 'nbar}}}))))
    (list np1)))

(def prep-phrase
  (let [head (ref {:synsem {:cat :prep}})
        comp (ref :top)]
    (fs/unifyc head-principle
               subcat-1-principle
               {:head head
                :comp comp
                :1 head
                :2 comp
                :extend {:a {:head 'lexicon ;; should be propositions.
                             :comp 'np}}})))

(def rules (concat np-rules vp-rules sentence-rules))

(def np (nth np-rules 0))
(def vp-present (nth vp-rules 0))
(def vp-past (nth vp-rules 1))
(def vp-future (nth vp-rules 2))
(def s1 (nth sentence-rules 0))
(def s2 (nth sentence-rules 1))

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
  (string/trim (str hour ":" (if (< minute 10) (str "0" minute) minute) " " (if (= hour 12) (if (= ampm "am") " after midnight" " after noon") ""))))
