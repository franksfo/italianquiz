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
                                    pronoun-acc pronoun-noun sentential-adverb
                                    verb verb-aux)])
(require '[italianverbs.pos.italiano :refer (adjective
                                             intransitive intransitive-unspecified-obj
                                             feminine-noun masculine-noun
                                             transitive verb-subjective)])
(require '[italianverbs.unify :refer (get-in)])
(require '[italianverbs.unify :as unify])

(def lexicon-source {

"abandonar" {:synsem {:cat :verb}}
"acabar" {:synsem {:cat :verb}}
"aceptar" {:synsem {:cat :verb}}
"acompañar" {:synsem {:cat :verb}}
"anunciar" {:synsem {:cat :verb}}
"apoyar" {:synsem {:cat :verb}}
"aprender" {:synsem {:cat :verb}}
"aprovechar" {:synsem {:cat :verb}}
"asegurar" {:synsem {:cat :verb}}
"aumentar" {:synsem {:cat :verb}}
"ayudar" {:synsem {:cat :verb}}
"bajar" {:synsem {:cat :verb}}
"cambiar" {:synsem {:cat :verb}}
"comentar" {:synsem {:cat :verb}}
"comer" {:synsem {:cat :verb}}
"compartir" {:synsem {:cat :verb}}
"comprar" {:synsem {:cat :verb}}
"comprender" {:synsem {:cat :verb}}
"conservar" {:synsem {:cat :verb}}
"considerar" {:synsem {:cat :verb}}
"consistir" {:synsem {:cat :verb}}
"contestar" {:synsem {:cat :verb}}
"correr" {:synsem {:cat :verb}}
"corresponder" {:synsem {:cat :verb}}
"cortar" {:synsem {:cat :verb}}
"crear" {:synsem {:cat :verb}}
"cumplir" {:synsem {:cat :verb}}
"deber" {:synsem {:cat :verb}}
"decidir" {:synsem {:cat :verb}}
"dejar" {:synsem {:cat :verb}}
"depender" {:synsem {:cat :verb}}
"desarrollar" {:synsem {:cat :verb}}
"desear" {:synsem {:cat :verb}}
"echar" {:synsem {:cat :verb}}
"enseñar" {:synsem {:cat :verb}}
"entrar" {:synsem {:cat :verb}}
"escapar" {:synsem {:cat :verb}}
"escuchar" {:synsem {:cat :verb}}
"esperar" {:synsem {:cat :verb}}
"estudiar" {:synsem {:cat :verb}}
"evitar" {:synsem {:cat :verb}}
"existir" {:synsem {:cat :verb}}
"expresar" {:synsem {:cat :verb}}
"faltar" {:synsem {:cat :verb}}
"fijar" {:synsem {:cat :verb}}
"formar" {:synsem {:cat :verb}}
"funcionar" {:synsem {:cat :verb}}
"ganar" {:synsem {:cat :verb}}
"guardar" {:synsem {:cat :verb}}
"gustar" {:synsem {:cat :verb}}
"hablar" {:synsem {:cat :verb}}
"imaginar" {:synsem {:cat :verb}}
"importar" {:synsem {:cat :verb}}
"iniciar" {:synsem {:cat :verb}}
"insistir" {:synsem {:cat :verb}}
"intentar" {:synsem {:cat :verb}}
"interesar" {:synsem {:cat :verb}}
"levantar" {:synsem {:cat :verb}}
"llamar" {:synsem {:cat :verb}}
"llevar" {:synsem {:cat :verb}}
"lograr" {:synsem {:cat :verb}}
"mandar" {:synsem {:cat :verb}}
"matar" {:synsem {:cat :verb}}
"meter" {:synsem {:cat :verb}}
"mirar" {:synsem {:cat :verb}}
"necesitar" {:synsem {:cat :verb}}
"notar" {:synsem {:cat :verb}}
"observar" {:synsem {:cat :verb}}
"ocupar" {:synsem {:cat :verb}}
"ocurrir" {:synsem {:cat :verb}}
"olvidar" {:synsem {:cat :verb}}
"participar" {:synsem {:cat :verb}}
"partir" {:synsem {:cat :verb}}
"pasar" {:synsem {:cat :verb}}
"permitir" {:synsem {:cat :verb}}
"preguntar" {:synsem {:cat :verb}}
"preocupar" {:synsem {:cat :verb}}
"preparar" {:synsem {:cat :verb}}
"presentar" {:synsem {:cat :verb}}
"prestar" {:synsem {:cat :verb}}
"pretender" {:synsem {:cat :verb}}
"quedar" {:synsem {:cat :verb}}
"quitar" {:synsem {:cat :verb}}
"recibir" {:synsem {:cat :verb}}
"representar" {:synsem {:cat :verb}}
"responder" {:synsem {:cat :verb}}
"resultar" {:synsem {:cat :verb}}
"salvar" {:synsem {:cat :verb}}
"señalar" {:synsem {:cat :verb}}
"subir" {:synsem {:cat :verb}}
"suceder" {:synsem {:cat :verb}}
"sufrir" {:synsem {:cat :verb}}
"terminar" {:synsem {:cat :verb}}
"tirar" {:synsem {:cat :verb}}
"tomar" {:synsem {:cat :verb}}
"trabajar" {:synsem {:cat :verb}}
"tratar" {:synsem {:cat :verb}}
"usar" {:synsem {:cat :verb}}
"vender" {:synsem {:cat :verb}}
"vivir" {:synsem {:cat :verb}}

})
