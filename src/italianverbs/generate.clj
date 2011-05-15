;; (not known if) RESTARTING OF RING REQUIRED FOR CHANGES TO THIS FILE. (must reload browser 2x though).
(ns italianverbs.generate
  (:use [somnium.congomongo])
  (:require
   [italianverbs.morphology :as morph]
   [italianverbs.grammar :as gram]
   [clojure.string :as string]))

(defn mobili []
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
    (merge {:question-type :mobili}
           (apply fn (list head comp)))))

;; move these random generation functions to a separate file: generate.clj
(defn random-present []
  (let [verb-inf (gram/choose-lexeme {:cat :verb :infl :infinitive})
        ;; TODO: find a way to do more complicated matching: i.e. {:root verb-inf}
        verb (gram/choose-lexeme {:cat :verb :infl :present
                             :root.italian (get verb-inf :italian)
                             })
        subject (cond
                 (or (= (get verb :person) "1st")
                     (= (get verb :person) "2nd"))
                 (gram/choose-lexeme
                  (merge {:case {:$ne :acc}
                          :cat :noun
                          :person (get verb :person)
                          :number (get verb :number)}
                         (get (get verb :root) :subj)))
                 true
                 (gram/np
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
        ;; choose a random verb in the passato-prossimo form.
        verb-past (gram/choose-lexeme {:root.cat :verb :infl :passato-prossimo})

        ;; find the infinitive for this form.
        verb-inf (gram/choose-lexeme {:cat :verb :infl :infinitive :italian (get verb-past :aux)})

        ;; get the appropriate auxilliary for that verb.
        ;; TODO: more complicated matching: i.e. {:root verb-inf}
        verb-aux (gram/choose-lexeme {:infl :present
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
                 (gram/choose-lexeme subj-constraints)
                 true
                 (gram/np subj-constraints))]
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
  