(ns italianverbs.generate)

;; usage:
;;(let [mypresent (gpresent)] (list (:english mypresent) (:italian mypresent)))

(defn gpresent []
  (let [root-verb (search/random {:cat :verb :infl :infinitive})
        object (conjugate-np (search/random (:obj root-verb)))
        subject (conjugate-np (search/random (search/random (:subj root-verb))))
        verb-phrase (conjugate-vp (fs/merge root-verb {:infl :present}) ; merge because we want :present to override :infinitive.
                                  subject
                                  object)]
    {:subj subject
     :object object
     :root-verb root-verb
     :verb-phrase verb-phrase
     :italian (str (:italian subject) " " 
                   (:italian verb-phrase))
     :english (str (:english subject) " " 
                   (:english verb-phrase))}))
     
(defn gpast []
  "generate a random passato-prossimo sentence"
  (let [root-verb (search/random {:cat :verb :infl :infinitive})
        object (conjugate-np (search/random (:obj root-verb)))
        subject (conjugate-np (search/random (:subj root-verb)))
        unified-with-subject (fs/unify root-verb {:subj subject})
        aux-verb (search/random (:passato-aux unified-with-subject))
        ;; search+regular inflection rules
        past-verb-search (fs/merge unified-with-subject {:infl :passato-prossimo})
        past-verb "(cyclic dependency)" ;(morph/conjugate past-verb-search)
        ]
    {:subject subject
     :object object
     :unified-with-subject unified-with-subject
     :root-verb root-verb
     :aux-verb aux-verb
     :italian (str (:italian subject) " "
                   (:italian aux-verb) " "
                   (:italian past-verb) " "
                   (:italian object))
     :english (str (:english subject) " "
                   (:english aux-verb) " "
                   (:english past-verb) " " 
                   (:english object))}))




