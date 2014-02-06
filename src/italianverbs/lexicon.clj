(ns italianverbs.lexicon
  (:refer-clojure :exclude [get-in merge resolve find])
  (:require
   [clojure.set :refer (union)]
   [clojure.tools.logging :as log]
   [italianverbs.lexiconfn :refer (adjective 
                                   agreement-noun
                                   animal
                                   clear! 
                                   cat-of-pronoun
                                   common-noun
                                   countable-noun
                                   disjunctive-case-of-pronoun
                                   drinkable-noun
                                   feminine-noun
                                   human
                                   implied 
                                   intransitive lookup-in 
                                   masculine-noun
                                   modal
                                   noun pronoun-acc
                                   pronoun-noun proper-noun
                                   sentential-adverb
                                   subcat0 transitive unify
                                   verb
                                   verb-aux-type
                                   verb-subjective
                                             )]
   [italianverbs.lexicon-a-noi :refer (lexicon-a-noi)]
   [italianverbs.unify :refer (fail? serialize)]))

(declare lexicon)

(defn lookup [query]
  (lookup-in query lexicon))

(defn it [italian]
  "same as it but no type conversion of singleton sets to take the first member."
  (let [result
        (union (set (lookup {:italian italian}))
               (set (lookup {:italian {:infinitive italian}}))
               (set (lookup {:italian {:infinitive {:infinitive italian}}}))
               (set (lookup {:italian {:italian italian}}))
               (set (lookup {:italian {:irregular {:passato italian}}})))]
    result))

(def it1 it) ; backwards compatibility

(defn en [english]
  (lookup {:english english}))

(def andare-common
   {:italian {:infinitive "andare"
              :essere true
              :irregular {:present {:1sing "vado"
                                    :2sing "vai"
                                    :3sing "va"
                                    :1plur "andiamo"
                                    :2plur "andate"
                                    :3plur "vanno"}
                          :futuro {:1sing "andrò"
                                   :2sing "andrai"
                                   :3sing "andrà"
                                   :1plur "andremo"
                                   :2plur "andrete"
                                   :3plur "andranno"}}}
    :english {:infinitive "to go"
              :irregular {:past "went"
                          :past-participle "gone"}}
    :synsem {:essere true
             :sem {:subj {:animate true}
                   :activity false ;; because "I was going when (something happened) .." sounds weird.
                   :pred :andare
                   :discrete false
                   :motion false}}})

(def avere-common
  {:synsem {:essere false
            :cat :verb}
   :italian {:infinitive "avere"
             :irregular {:passato "avuto"
                         :present {:1sing "ho"
                                   :2sing "hai"
                                   :3sing "ha"
                                   :1plur "abbiamo"
                                   :2plur "avete"
                                   :3plur "hanno"}}}
   :english {:infinitive "to have"
             :irregular {:past "had"
                         :present {:1sing "have"
                                   :2sing "have"
                                   :3sing "has"
                                   :1plur "have"
                                   :2plur "have"
                                   :3plur "have"}}}})

(def essere-common
  {:synsem {:essere true}
   :italian {:infinitive "essere"
             :essere true
             :irregular {:present {:1sing "sono"
                                   :2sing "sei"
                                   :3sing "è"
                                   :1plur "siamo"
                                   :2plur "siete"
                                   :3plur "sono"}
                         :passato "stato"
                         :imperfetto {:1sing "ero"
                                      :2sing "eri"
                                      :3sing "era"
                                      :1plur "eravamo"
                                      :2plur "eravate"
                                      :3plur "erano"}
                         :futuro {:1sing "sarò"
                                  :2sing "sarai"
                                  :3sing "sarà"
                                  :1plur "saremo"
                                  :2plur "sarete"
                                  :3plur "saranno"}}}
   :english {:infinitive "to be"
             :irregular {:present {:1sing "am"
                                   :2sing "are"
                                   :3sing "is"
                                   :1plur "are"
                                    :2plur "are"
                                   :3plur "are"}
                         :past {:participle "been"
                                :1sing "was"
                                :2sing "were"
                                :3sing "was"
                                :1plur "were"
                                :2plur "were"
                                :3plur "were"}}}})

(def fare-common
  ;; factor out common stuff from all senses of "fare".
  {:synsem {:essere false}
   :italian {:infinitive "fare"
             :irregular {:passato "fatto"
                         :present {:1sing "facio"
                                   :2sing "fai"
                                   :3sing "fa"
                                   :1plur "facciamo"
                                   :2plur "fate"
                                   :3plur "fanno"}
                         :imperfetto {:1sing "facevo"
                                      :2sing "facevi"
                                      :3sing "faceva"
                                      :1plur "facevamo"
                                      :2plur "facevate"
                                      :3plur "facevano"}
                         :futuro {:1sing "farò"
                                  :2sing "farai"
                                  :3sing "farà"
                                  :1plur "faremo"
                                  :2plur "farete"
                                  :3plur "faranno"}}}})

(def venire-common
  {:italian {:infinitive "venire"
             :irregular {:passato "venuto"
                         :futuro  {:1sing "verrò"
                                   :2sing "verrai"
                                   :3sing "verrà"
                                   :1plur "verremo"
                                   :2plur "verrete"
                                   :3plur "verranno"}
                         :present {:1sing "vengo"
                                   :2sing "vieni"
                                   :3sing "viene"
                                   :1plur "veniamo"
                                   :2plur "venete"
                                   :3plur "vengono"}}}
   :english {:infinitive "to come"
             :irregular {:past "came"}}})


(clear!)

(def lexicon

  (concat

  ;; this filter is for debugging purposes to restrict lexicon to particular entries, if desired.
  ;; default shown is (not (nil? entry)) i.e. no restrictions except that an entry must be non-nil.
  ;;  (currently there is one nil below: "chiunque (anyone)").
  (filter (fn [entry]
            (or false
                (not (nil? entry))))

          ;; TODO: move this fn to lexiconfn: keep any code out of the lexicon proper.
          ;; this (map) adds, to each lexical entry, a copy of the serialized form of the entry.
          (map (fn [entry]
                 (if (fail? entry)
                   (log/warn (str "Ignoring this lexeme because (fail?=true): " entry))
                   ;; else, not fail, so add to lexicon.
                   (do
                     (log/debug (str "serializing entry: " entry))
                     (let [italian (get-in entry '(:italian))
                           entry
                           (conj
                            {:italian (if (string? italian)
                                        {:italian italian}
                                        italian)}
                    (dissoc
                     (if (not (= :none (get entry :serialized :none)))
                       (conj {:serialized (serialize entry)}
                             entry)
                       (conj {:serialized (serialize (dissoc entry :serialized))}
                         entry))
                     :italian))]
                       (log/debug (str "successfully serialized: " entry))
                       (implied entry)))))
               (list
                lexicon-a-noi


                )))))
