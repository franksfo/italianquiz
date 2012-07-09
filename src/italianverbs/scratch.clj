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

     
(defn gpass []
  (let [root-verb (search/random {:cat :verb :infl :infinitive})
        object (conjugate-np (search/random (:obj root-verb)))
        subject (conjugate-np (search/random (search/random (:subj root-verb))))
        verb-phrase (conjugate-vp (fs/merge root-verb {:infl :passato-prossimo}) ; merge because we want :present to override :infinitive.
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


