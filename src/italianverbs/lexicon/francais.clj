(ns italianverbs.lexicon.francais
  (:require
   [italianverbs.lexiconfn :refer (unify)]))

(def lexicon-source 
  
"abandonar" {:synsem {:cat :verb
                                           :sem {:pred :abandon}}}
                     "terminer" {:synsem {:cat :verb
                                        :sem {:pred :finish}}}
                     "accepter" {:synsem {:cat :verb
                                         :sem {:pred :accept}}}
                     "accompagner" {:synsem {:cat :verb
                                           :sem {:pred :accompany}}}
                     "anoncier" {:synsem {:cat :verb
                                          :sem {:pred :announce}}}
                     "soutenir" {:synsem {:cat :verb
                                        :sem {:pred :support}}}
                     "apprendre" {:synsem {:cat :verb
                                          :sem {:pred :imparare}}}

                     "profiter (de)" [
                                   {:synsem {:cat :verb
                                             :sem {:pred :take-advantage-of}}}
                                   

                     "assurer" [{:synsem {:cat :verb
                                           :sem {:pred :assure}}}
                                 {:synsem {:cat :verb
                                           :sem {:pred :insure}}}]

                     "augmenter" {:synsem {:cat :verb
                                          :sem {:pred :increase}}}

                     "aider" {:synsem {:cat :verb
                                        :sem {:pred :aiutare}}}

                     "baisser" {:synsem {:cat :verb
                                       :sem {:pred :lower}}}

                     "cambiar" {:synsem {:cat :verb
                                         :sem {:pred :cambiare}}}

                     "commenter" {:synsem {:cat :verb
                                          :sem {:pred :comment}}}

                     "manger" {:synsem {:cat :verb
                                       :sem {:pred :mangiare}}}

                     "partager" {:synsem {:cat :verb
                                           :sem {:pred :share}}}

                     "acheter" {:synsem {:cat :verb
                                         :sem {:pred :comprare}}}

                     "comprendre" {:synsem {:cat :verb
                                            :sem {:pred :understand}}}

                     "conserver" [{:synsem {:cat :verb
                                           :sem {:pred :conserve}}}
                                  {:synsem {:cat :verb
                                            :sem {:pred :preserve}}}]

                     "considérer" {:synsem {:cat :verb
                                            :sem {:pred :consider}}}

                     "répondre" {:synsem {:cat :verb
                                           :sem {:pred :answer}}}

                     "corrir" {:synsem {:cat :verb
                                        :sem {:pred :run}}}

                     "correspondre" {:synsem {:cat :verb
                                              :sem {:pred :correspond}}}

                     "couper" {:synsem {:cat :verb
                                        :sem {:pred :cut}}}

                     "créer" {:synsem {:cat :verb
                                       :sem {:pred :create}}}

   ;; TODO: clarify semantics of this in English 
   ;;                     "cumplir" {:synsem {:cat :verb
   ;;                                         :sem {:pred :turn-years}}}
   
   "devoir" {:synsem {:cat :verb
                     :sem {:pred :have-to}}}
   
   "décider" {:synsem {:cat :verb
                       :sem {:pred :decide}}}
   
   "laisser" {:synsem {:cat :verb
                     :sem {:pred :leave-behind}}}
   
   "développer" {:synsem {:cat :verb
                           :sem {:pred :develop}}}
   
   "desirer" {:synsem {:cat :verb
                      :sem {:pred :desire}}}
   
   "dormir" {:synsem {:cat :verb
                      :sem {:pred :dormire}}}
   
   "jeter" {:synsem {:cat :verb
                     :sem {:pred :throw-out}}}
   
   "le"
   (unify determiner
          {:synsem {:cat :det
                    :def :def
                    :gender :masc
                    :number :sing}})
   
   "il" [{:synsem {:cat cat-of-pronoun
                   :pronoun true
                   :case :nom
                   :agr {:person :3rd
                         :gender :masc
                         :number :sing}
                   :sem {:human true
                         :gender :masc
                         :pred :lui}
                   :subcat '()}}
         {:synsem {:cat cat-of-pronoun
                   :pronoun true
                   :case :nom
                   :agr {:person :3rd
                         :gender :masc
                         :number :sing}
                   :sem {:human false
                         :gender :masc
                         :pred :lui}
                   :subcat '()}}]
   
   "elle"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :fem
                   :number :sing}
             :sem {:human true
                   :gender :fem
                   :pred :lei}
             :subcat '()}}
   
   "ils"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :masc
                   :number :plur}
             :sem {:human true
                   :gender :masc
                   :pred :loro}
             :subcat '()}}
   
   "elles"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :fem
                   :number :plur}
             :sem {:human true
                   :gender :fem
                   :pred :loro}
             :subcat '()}}
   
   "enseigner" [{:synsem {:cat :verb
                        :sem {:pred :show}}}
              {:synsem {:cat :verb
                        :sem {:pred :teach}}}]
   
   "entrer" {:synsem {:cat :verb
                      :sem {:pred :enter}}}
   
   "s'echapper" {:synsem {:cat :verb
                          :reflexive true
                          :sem {:pred :escape}}}
   
   "écouter" {:synsem {:cat :verb
                        :sem {:pred :listen-to}}}
   
   "attendre" {:synsem {:cat :verb
                        :sem {:pred :wait-for}}}
  
    "espérer" {:synsem {:cat :verb
                        :sem {:pred :hope}}}
   
   "étudier" {:synsem {:cat :verb
                        :sem {:pred :study}}}
   
   "éviter" {:synsem {:cat :verb
                      :sem {:pred :avoid}}}
   
   "exister" {:synsem {:cat :verb
                       :sem {:pred :exist}}}
   
   "exprimer" {:synsem {:cat :verb
                        :sem {:pred :express}}}
   
   "manquer" {:synsem {:cat :verb
                      :sem {:pred :to-be-missing}}}
   
   "former" {:synsem {:cat :verb
                      :sem {:pred :form}}}
   
   "fonctionner" {:synsem {:cat :verb
                         :sem {:subj {:human false}
                               :pred :work-nonhuman}}}
   
   "gagner" [{:synsem {:cat :verb
                      :sem {:pred :earn
                            :subj {:human true}
                            :obj {:human false}}}}
            {:synsem {:cat :verb
                      :sem {:pred :win
                            :subj {:human true}
                            :obj {:human false}}}}]
   
   ;; TODO: handle syntax/semantics mismatch between Italian/Spanish and English.
                                        ;                     "gustar" {:synsem {:cat :verb
                                        ;                                         :sem {:pred :??}}}
   
   "parler" [{:synsem {:cat :verb
                       :sem {:pred :speak
                             :subj {:human true}}}}
             {:synsem {:cat :verb
                       :sem {:pred :talk
                             :subj {:human true}}}}]
   
   "la"
   (unify determiner
          {:synsem {:cat :det
                    :def :def
                    :gender :fem
                    :number :sing}})
   
   "femme"
   (unify agreement-noun
          common-noun
          feminine-noun
          {:synsem {:sem {:pred :donna
                          :human true}
                    :subcat {:1 {:cat :det
                                 :number :sing
                                 :def :def}}}})
   
   
   
   "nous"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :number :plur}
             :sem {:human true
                   :pred :noi}
             :subcat '()}}
   
   "pain"
   (unify agreement-noun
          common-noun
          masculine-noun
          {:synsem {:sem {:pred :pane
                          :edible true
                          :artifact true}
                    :subcat {:1 {:cat :det
                                 :number :sing
                                 :def :def}}}})
   
   
   "tu" {:synsem {:cat :noun
                  :pronoun true
                  :case :nom
                  :agr {:person :2nd
                        :number :sing}
                  :sem {:human true
                        :pred :tu}
                  :subcat '()}}
   
   "vous"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :number :plur}
             :sem {:human true
                   :pred :voi}
             :subcat '()}}
   
   
   "je"       {:synsem {:cat :noun
                        :pronoun true
                        :case :nom
                        :agr {:gender :fem
                              :person :1st
                              :number :sing}
                        :sem {:human true
                              :pred :io}
                        :subcat '()}}
   
   })




