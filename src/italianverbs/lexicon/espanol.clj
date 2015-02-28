(ns italianverbs.lexicon.espanol
  (:refer-clojure :exclude [get-in]))

(require '[clojure.tools.logging :as log])
(require '[italianverbs.lexiconfn :refer (compile-lex map-function-on-map-vals unify)])
(require '[italianverbs.morphology.italiano :refer (agreement analyze exception-generator phonize italian-specific-rules)])
(require '[italianverbs.morphology.italiano :as m])
(require '[italianverbs.pos :refer (agreement-noun 
                                    cat-of-pronoun common-noun
                                    comparative
                                    countable-noun determiner
                                    drinkable-noun non-comparative-adjective noun
                                    pronoun-acc sentential-adverb
                                    verb verb-aux)])
;; TODO: consider factoring out Italian and Spanish commonalities into a single namespace.
(require '[italianverbs.pos.italiano :refer (adjective
                                             intransitive intransitive-unspecified-obj
                                             feminine-noun masculine-noun
                                             transitive verb-subjective)])
(require '[italianverbs.unify :refer (get-in)])
(require '[italianverbs.unify :as unify])

(def lexicon-source {

                     "abandonar" {:synsem {:cat :verb
                                           :sem {:pred :abandon}}}

                     "acabar" {:synsem {:cat :verb
                                        :sem {:pred :finish}}}
                     "aceptar" {:synsem {:cat :verb
                                         :sem {:pred :accept}}}
                     "acompañar" {:synsem {:cat :verb
                                           :sem {:pred :accompany}}}
                     "anunciar" {:synsem {:cat :verb
                                          :sem {:pred :announce}}}
                     "apoyar" {:synsem {:cat :verb
                                        :sem {:pred :support}}}
                     "aprender" {:synsem {:cat :verb
                                          :sem {:pred :imparare}}}

                     "aprovechar" [
                                   {:synsem {:cat :verb
                                             :sem {:pred :take-advantage-of}}}
                                   {:synsem {:cat :verb
                                             :sem {:pred :enjoy}}}]

                     "asegurar" [{:synsem {:cat :verb
                                           :sem {:pred :assure}}}
                                 {:synsem {:cat :verb
                                           :sem {:pred :insure}}}]

                     "aumentar" {:synsem {:cat :verb
                                          :sem {:pred :increase}}}

                     "ayudar" {:synsem {:cat :verb
                                        :sem {:pred :aiutare}}}

                     "bajar" {:synsem {:cat :verb
                                       :sem {:pred :lower}}}

                     "cambiar" {:synsem {:cat :verb
                                         :sem {:pred :cambiare}}}

                     "comentar" {:synsem {:cat :verb
                                          :sem {:pred :comment}}}

                     "comer" {:synsem {:cat :verb
                                       :sem {:pred :mangiare}}}

                     "compartir" {:synsem {:cat :verb
                                           :sem {:pred :share}}}

                     "comprar" {:synsem {:cat :verb
                                         :sem {:pred :comprare}}}

                     "comprender" {:synsem {:cat :verb
                                            :sem {:pred :understand}}}

                     "conservar" [{:synsem {:cat :verb
                                           :sem {:pred :conserve}}}
                                  {:synsem {:cat :verb
                                            :sem {:pred :preserve}}}]

                     "considerar" {:synsem {:cat :verb
                                            :sem {:pred :consider}}}

                     "contestar" {:synsem {:cat :verb
                                           :sem {:pred :answer}}}

                     "correr" {:synsem {:cat :verb
                                        :sem {:pred :run}}}

                     "corresponder" {:synsem {:cat :verb
                                              :sem {:pred :correspond}}}

                     "cortar" {:synsem {:cat :verb
                                        :sem {:pred :cut}}}

                     "crear" {:synsem {:cat :verb
                                       :sem {:pred :create}}}

;; TODO: clarify semantics of this in English 
;;                     "cumplir" {:synsem {:cat :verb
;;                                         :sem {:pred :turn-years}}}
                     
                     "deber" {:synsem {:cat :verb
                                       :sem {:pred :have-to}}}

                     "decidir" {:synsem {:cat :verb
                                         :sem {:pred :decide}}}

                     "dejar" {:synsem {:cat :verb
                                       :sem {:pred :leave-behind}}}

                     "desarrollar" {:synsem {:cat :verb
                                             :sem {:pred :develop}}}

                     "desear" {:synsem {:cat :verb
                                        :sem {:pred :desire}}}

                     "dormir" {:synsem {:cat :verb
                                        :sem {:pred :dormire}}}

                     "echar" {:synsem {:cat :verb
                                       :sem {:pred :throw-out}}}

                     "enseñar" [{:synsem {:cat :verb
                                          :sem {:pred :show}}}
                                {:synsem {:cat :verb
                                          :sem {:pred :teach}}}]

                     "entrar" {:synsem {:cat :verb
                                        :sem {:pred :enter}}}

                     "escapar" {:synsem {:cat :verb
                                         :sem {:pred :escape}}}

                     "escuchar" {:synsem {:cat :verb
                                          :sem {:pred :listen-to}}}

                     "esperar" [{:synsem {:cat :verb
                                          :sem {:pred :wait-for}}}
                                {:synsem {:cat :verb
                                          :sem {:pred :hope}}}]

                     "estudiar" {:synsem {:cat :verb
                                          :sem {:pred :study}}}

                     "evitar" {:synsem {:cat :verb
                                        :sem {:pred :avoid}}}

                     "existir" {:synsem {:cat :verb
                                         :sem {:pred :exist}}}

                     "expresar" {:synsem {:cat :verb
                                          :sem {:pred :express}}}

                     "faltar" {:synsem {:cat :verb
                                        :sem {:pred :to-be-missing}}}

                     "formar" {:synsem {:cat :verb
                                        :sem {:pred :form}}}

                     "funcionar" {:synsem {:cat :verb
                                           :sem {:pred :work-nonhuman}}}

                     "ganar" [{:synsem {:cat :verb
                                        :sem {:pred :earn}}}
                              {:synsem {:cat :verb
                                        :sem {:pred :win}}}]

;; TODO: handle syntax/semantics mismatch between Italian/Spanish and English.
;                     "gustar" {:synsem {:cat :verb
;                                         :sem {:pred :??}}}

                     "hablar" [{:synsem {:cat :verb
                                         :sem {:pred :speak}}}
                               {:synsem {:cat :verb
                                         :sem {:pred :talk}}}]

                     "imaginar" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "importar" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "iniciar" {:synsem {:cat :verb
                                         :sem {:pred :??}}}
                     "insistir" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "intentar" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "interesar" {:synsem {:cat :verb
                                           :sem {:pred :??}}}
                     "levantar" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "llamar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "llevar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "lograr" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "mandar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "matar" {:synsem {:cat :verb
                                       :sem {:pred :??}}}
                     "meter" {:synsem {:cat :verb
                                       :sem {:pred :??}}}
                     "mirar" {:synsem {:cat :verb
                                       :sem {:pred :??}}}
                     "necesitar" {:synsem {:cat :verb
                                           :sem {:pred :??}}}
                     "notar" {:synsem {:cat :verb
                                       :sem {:pred :??}}}
                     "observar" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "ocupar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "ocurrir" {:synsem {:cat :verb
                                         :sem {:pred :??}}}
                     "olvidar" {:synsem {:cat :verb
                                         :sem {:pred :??}}}
                     "participar" {:synsem {:cat :verb
                                            :sem {:pred :??}}}
                     "partir" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "pasar" {:synsem {:cat :verb
                                       :sem {:pred :??}}}
                     "permitir" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "preguntar" {:synsem {:cat :verb
                                           :sem {:pred :??}}}
                     "preocupar" {:synsem {:cat :verb
                                           :sem {:pred :??}}}
                     "preparar" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "presentar" {:synsem {:cat :verb
                                           :sem {:pred :??}}}
                     "prestar" {:synsem {:cat :verb
                                         :sem {:pred :??}}}
                     "pretender" {:synsem {:cat :verb
                                           :sem {:pred :??}}}
                     "quedar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "quitar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "recibir" {:synsem {:cat :verb
                                         :sem {:pred :??}}}
                     "representar" {:synsem {:cat :verb
                                             :sem {:pred :??}}}
                     "responder" {:synsem {:cat :verb
                                           :sem {:pred :??}}}
                     "resultar" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "salvar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "señalar" {:synsem {:cat :verb
                                         :sem {:pred :??}}}
                     "subir" {:synsem {:cat :verb
                                       :sem {:pred :??}}}
                     "suceder" {:synsem {:cat :verb
                                         :sem {:pred :??}}}
                     "sufrir" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "terminar" {:synsem {:cat :verb
                                          :sem {:pred :??}}}

                     "tirar" [{:synsem {:cat :verb
                                        :sem {:pred :throw-out}}}
                              {:synsem {:cat :verb
                                        :sem {:pred :throw}}}]
                               

                     "tomar" {:synsem {:cat :verb
                                       :sem {:pred :??}}}
                     "trabajar" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "tratar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "usar" {:synsem {:cat :verb
                                      :sem {:pred :??}}}
                     "vender" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "vivir" {:synsem {:cat :verb
                                       :sem {:pred :??}}}

                     "yo"       [{:synsem {:cat :noun
                                           :pronoun true
                                           :case :nom
                                           :agr {:gender :fem
                                                 :person :1st
                                                 :number :sing}
                                           :sem {:human true
                                                 :pred :io}
                                           :subcat '()}}

                                 {:synsem {:cat :noun
                                           :pronoun true
                                           :case :nom
                                           :agr {:gender :masc
                                                 :person :1st
                                                 :number :sing}
                                           :sem {:human true
                                                 :pred :io}
                                           :subcat '()}}]

                     })




