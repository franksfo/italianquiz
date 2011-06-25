;; RESTARTING OF RING REQUIRED FOR CHANGES TO THIS FILE. (purtroppo)
(ns italianverbs.generate
  (:use [somnium.congomongo])
  (:require
   [italianverbs.morphology :as morph]
   [italianverbs.grammar :as gram]
   [italianverbs.config :as config]
   [clojure.string :as string]))

(defn mobili []
  (let [fn gram/sv
        head
        (merge
         {:infl :present}
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
           (apply fn (list head comp))))
        comp
        (gram/np-with-post-conditions 
          {:furniture true}
          (defn fn [fs]
            (= (get fs :def) "def")))]
    (merge {:question-type :mobili}
           (apply fn (list head comp)))))


;; TODO : factor out commonalities between random-present and random-passato-prossimo.
(defn random-present []
  (let [;; choose a random verb in the infinitive form.
        verb-inf (gram/choose-lexeme
                  (merge {:cat :verb
                          :infl :infinitive}
                         config/random-present-inf))


        verb-present-constraints
        (merge 
         {:root.cat :verb :infl :present}
         {:root.italian (get verb-inf :italian)})

        verb-present
        (let [lexical
              (gram/choose-lexeme verb-present-constraints)]
          (if (not (= (get lexical :cat) :error))
            lexical
              ;; else, use a pronoun as a source of a random person/number pair,
              ;; and generate a present verb.
            (let [pronoun (gram/choose-lexeme
                           (merge
                            {:pronoun true}
                            config/random-present-subj))
                  number (get pronoun :number)
                  person (get pronoun :person)]
              {:italian (morph/conjugate-italian-verb-regular verb-inf
                                                        {:person person
                                                         :number number})

                                                
               :english (morph/conjugate-english-verb verb-inf
                                                {:person person
                                                 :number number}
                                                {:infl :present})
                                               
               :root verb-inf
               :person person
               :number number})))

        subj-constraints
        (merge
         {:cat :noun
          :case {:$ne :acc}}
         (get (get verb-present :root) :subj)
         (get verb-inf :subj)
         {:person (get verb-present :person)
          :number (get verb-present :number)})
        subject (cond
                 (or (= (get verb-present :person) "1st")
                     (= (get verb-present :person) "2nd"))
                 (gram/choose-lexeme subj-constraints)
                 true
                 (gram/np subj-constraints))]
    (merge
     {:verb-inf verb-inf
      :verb-present verb-present
      :subject subject
      :verb-constraints verb-present-constraints
      :subj-constraints subj-constraints
      :english (str (get subject :english) " "
                    (if (get verb-present :english)
                      (get verb-present :english)
                      (morph/conjugate-english-verb verb-inf subject {:infl :present})))
      :italian (str (get subject :italian) " " (get verb-present :italian))}
    {:type-is-fs (set '(:verb-present :subject :verb-inf :subj-constraints :verb-constraints))})))

(defn random-infinitivo []
  (gram/choose-lexeme
   (merge {:cat :verb
           :infl :infinitive}
          config/random-infinitivo)))

(defn espressioni []
  (gram/choose-lexeme {:cat :espressioni}))

(defn random-passato-prossimo []
  (let [
        ;; choose a random verb in the passato-prossimo form.
        verb-past (gram/choose-lexeme
                   (merge
                    (if (get config/random-passato-prossimo-subj :person)
                      {:person (get config/random-passato-prossimo-subj :person)}
                      {})
                    (if (get config/random-passato-prossimo-subj :number)
                      {:number (get config/random-passato-prossimo-subj :number)}
                      {})
                    {:root.cat :verb :infl :passato-prossimo}
                    config/random-passato-prossimo-verb-past))

        ;; find the infinitive for this form.
        verb-inf (gram/choose-lexeme {:cat :verb
                                      :infl :infinitive
                                      :italian (get verb-past :aux)})

        ;; get the appropriate auxiliary for that verb.
        ;; TODO: more complicated matching: i.e. {:root verb-inf}
        verb-aux (gram/choose-lexeme
                  (merge
                   {:infl :present
                    :root.italian (get verb-inf :italian)}
                   (if (get verb-past :person)
                     {:person (get verb-past :person)}
                     {})
                   (if (get verb-past :number)
                     {:number (get verb-past :number)}
                     {})))
        subj-constraints
        (merge
         {:cat :noun
          :case {:$ne :acc}}
         (get verb-inf :subj)
         (get (get verb-past :root) :subj)
         (get verb-inf :subj)
         config/random-passato-prossimo-subj
         {:person (get verb-aux :person)
          :number (get verb-aux :number)}
         )
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
                    (morph/conjugate-english-verb verb-past subject) " ")
                    ;(get verb-past :english))
      :italian (str (get subject :italian) " " (get verb-aux :italian) " "
                    (morph/conjugate-italian-verb verb-past subject))}

     {:type-is-fs (set '(:verb-past :subject :verb-inf :subj-constraints :verb-aux))}
     {:save-to-db (set '(:verb-past :subject))}
     )))
  