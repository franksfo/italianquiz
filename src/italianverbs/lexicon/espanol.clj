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
                                           :sem {:pred :??}}}
                     "acabar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "aceptar" {:synsem {:cat :verb
                                         :sem {:pred :??}}}
                     "acompañar" {:synsem {:cat :verb
                                           :sem {:pred :??}}}
                     "anunciar" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "apoyar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "aprender" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "aprovechar" {:synsem {:cat :verb
                                            :sem {:pred :??}}}
                     "asegurar" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "aumentar" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "ayudar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "bajar" {:synsem {:cat :verb
                                       :sem {:pred :??}}}
                     "cambiar" {:synsem {:cat :verb
                                         :sem {:pred :??}}}
                     "comentar" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "comer" {:synsem {:cat :verb
                                       :sem {:pred :??}}}
                     "compartir" {:synsem {:cat :verb
                                           :sem {:pred :??}}}
                     "comprar" {:synsem {:cat :verb
                                         :sem {:pred :??}}}
                     "comprender" {:synsem {:cat :verb
                                            :sem {:pred :??}}}
                     "conservar" {:synsem {:cat :verb
                                           :sem {:pred :??}}}
                     "considerar" {:synsem {:cat :verb
                                            :sem {:pred :??}}}
                     "consistir" {:synsem {:cat :verb
                                           :sem {:pred :??}}}
                     "contestar" {:synsem {:cat :verb
                                           :sem {:pred :??}}}
                     "correr" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "corresponder" {:synsem {:cat :verb
                                              :sem {:pred :??}}}
                     "cortar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "crear" {:synsem {:cat :verb
                                       :sem {:pred :??}}}
                     "cumplir" {:synsem {:cat :verb
                                         :sem {:pred :??}}}
                     "deber" {:synsem {:cat :verb
                                       :sem {:pred :??}}}
                     "decidir" {:synsem {:cat :verb
                                         :sem {:pred :??}}}
                     "dejar" {:synsem {:cat :verb
                                       :sem {:pred :??}}}
                     "depender" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "desarrollar" {:synsem {:cat :verb
                                             :sem {:pred :??}}}
                     "desear" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "echar" {:synsem {:cat :verb
                                       :sem {:pred :??}}}
                     "enseñar" {:synsem {:cat :verb
                                         :sem {:pred :??}}}
                     "entrar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "escapar" {:synsem {:cat :verb
                                         :sem {:pred :??}}}
                     "escuchar" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "esperar" {:synsem {:cat :verb
                   :sem {:pred :??}}}
                     "estudiar" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "evitar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "existir" {:synsem {:cat :verb
                                         :sem {:pred :??}}}
                     "expresar" {:synsem {:cat :verb
                                          :sem {:pred :??}}}
                     "faltar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "fijar" {:synsem {:cat :verb
                                       :sem {:pred :??}}}
                     "formar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "funcionar" {:synsem {:cat :verb
                                           :sem {:pred :??}}}
                     "ganar" {:synsem {:cat :verb
                                       :sem {:pred :??}}}
                     "guardar" {:synsem {:cat :verb
                                         :sem {:pred :??}}}
                     "gustar" {:synsem {:cat :verb
                                        :sem {:pred :??}}}
                     "hablar" {:synsem {:cat :verb
                                        :sem {:pred :parlare}}}
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
                     "tirar" {:synsem {:cat :verb
                                       :sem {:pred :??}}}
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
                                       :sem {:pred :??}}}})



