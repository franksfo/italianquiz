;;verbs
(unify
      transitive
      avere-common
      {:synsem {:sem {:pred :avere
                      :activity false
                      :discrete false
                      :subj {:human true}
                      :obj {:buyable true}}}})

;; bere
     (unify
      (:transitive verb)
      {:italian {:infinitive "bere"
                 :irregular {:passato "bevuto"
                             :imperfetto {:1sing "bevevo"
                                          :2sing "bevevi"
                                          :3sing "beveva"
                                          :1plur "bevevamo"
                                          :2plur "bevevate"
                                          :3plur "bevevano"}
                             :present {:1sing "bevo"
                                       :2sing "bevi"
                                       :3sing "beve"
                                       :1plur "beviamo"
                                       :2plur "bevete"
                                       :3plur "bevano"}}}
       :english {:infinitive "to drink"
                 :irregular {:past "drank"}}
       :synsem {:essere false
                :sem {:pred :bere
                      :subj {:animate true}
                      :obj {:drinkable true}}}})

      ;; cercare
      (unify
       (:transitive verb)
       {:italian {:infinitive "cercare"}
        :english {:infinitive "to look for"
                  :irregular {:past "looked for"
                              :imperfetto-suffix "looking for"
                              :past-participle "looked for"
                              :present {:1sing "look for"
                                        :2sing "look for"
                                        :3sing "looks for"
                                        :1plur "look for"
                                        :2plur "look for"
                                        :3plur "look for"}}}
        :synsem {:essere false
                 :sem {:pred :cercare
                       :activity true
                       :discrete false
                       :subj {:human true}
                       :obj {:physical-object true}}}})

