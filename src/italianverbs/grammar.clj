;; NO RESTARTING OF RING REQUIRED FOR CHANGES TO THIS FILE. (must reload browser 2x though).
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
        head-sem (ref :top)]
    {:synsem {:cat head-cat
              :sem head-sem}
     :head {:synsem {:cat head-cat
                     :sem head-sem}}}))

;;     subcat<>
;;     /      \
;;    /        \
;; H subcat<1>  C[1]
(def subcat-1-principle
  (let [comp-synsem (ref :top)]
    {:head {:synsem {:subcat {:1 comp-synsem}}}
     :comp {:synsem comp-synsem}}))

(def vp-rules
  (list
   (let [obj-sem (ref :top)
         obj-synsem (ref {:sem obj-sem})
         obj (ref {:synsem obj-synsem})
         subj-sem (ref :top)
         subj-synsem (ref {:sem subj-sem})
         head-synsem (ref {:cat :verb
                           :infl {:not :infinitive}
                           :sem {:subj subj-sem
                                 :obj obj-sem}
                           :subcat {:1 subj-synsem
                                    :2 obj-synsem}})
         head (ref {:synsem head-synsem})]
     (fs/unifyc head-principle
                {:comment "vp -> head comp"
                 :head head
                 :synsem {:subcat {:1 subj-synsem}}
                 :comp obj
                 :1 head
                 :2 obj}))))

(def sentence-rules
  (let [subj-sem (ref :top)
        subcatted (ref {:cat :noun
                        :sem subj-sem})
        head-synsem (ref {:cat :verb
                          :infl {:not :infinitive}
                          :sem {:subj subj-sem}
                          :subcat {:1 subcatted}})
        comp (ref {:synsem subcatted})
        head (ref {:synsem head-synsem})]
    (list
     (fs/unifyc head-principle subcat-1-principle
               {:comment "s -> np vp"
                :head head
                :comp comp
                :1 comp
                :2 head}))))

(def np-rules 
  (list
   (fs/unifyc head-principle subcat-1-principle ;; NP -> Comp Head
              (let [case (ref :top)
                    comp (ref {:synsem {:cat :det}})
                    person (ref :top)
                    number (ref :top)
                    gender (ref :top)
                    agr (ref :top)
                    head (ref {:synsem {:cat :noun
                                        :agr agr}})]
                {:comment "np -> det noun"
                 :synsem {:agr agr}
                 :head head
                 :comp comp
                 :1 comp
                 :2 head}))))

(def prep-phrase
  (let [head (ref {:synsem {:cat :prep}})
        comp (ref :top)]
    (fs/unifyc head-principle
               subcat-1-principle
               {:head head
                :comp comp
                :1 head
                :2 comp})))
             

(def rules (concat np-rules vp-rules sentence-rules))

(def np (nth rules 0))
(def vp (nth rules 1))
(def s (nth rules 2))

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


