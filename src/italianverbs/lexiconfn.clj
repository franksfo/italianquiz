(ns italianverbs.lexiconfn
  (:refer-clojure :exclude [get-in merge resolve find])
  (:use [clojure.set])
  (:require
   [clojure.set :as set]
   [clojure.tools.logging :as log]
   [clojure.core :as core]
   [italianverbs.morphology :as morph]
   [italianverbs.unify :refer :all :exclude (unify)])) ;; exclude unify because we redefine it here using unifyc (copy each arg)

(defn unify [ & args]
  "like unify/unify, but unify/copy each argument before unifying."
  (do
    (log/debug (str "(lexfn)unify args: " args))
    (log/debug (str "(lexfn)unify first arg: " (first args)))
    (apply unifyc args)))

(defn cache-serialization [entry]
  "Copying ((unify/copy)ing) lexical entries during generation or parsing is done by serializing and then deserializing. 
storing a deserialized form of each lexical entry avoids the need to serialize every time."
  (if (fail? entry)
    ;; TODO: better diagnostics: entry is just :fail, which isn't very helpful.
    (log/warn (str "Ignoring this lexeme because (fail?=true): " entry))
    ;; else, not fail, so add to lexicon.
    (do
      (log/debug (str "Adding entry: " (morph/fo entry)))
      (let [italian (get-in entry '(:italiano))
            entry
            (conj
             {:italiano (if (string? italian)
                         {:italiano italian}
                         italian)}
             (dissoc
              (if (not (= :none (get entry :serialized :none)))
                (conj {:serialized (serialize entry)}
                      entry)
                (conj {:serialized (serialize (dissoc entry :serialized))}
                      entry))
              :italiano))]
        (log/debug (str "successfully serialized: " entry))
        entry))))

(defn encode-where-query [& where]
  "encode a query as a set of index queries."
  where)

(defn italian [lexeme]
  (get (nth lexeme 1) :lexicon))

(defn synsem [lexeme]
  (nth lexeme 1))

(defn english [lexeme]
  (get (nth lexeme 1) :english))

(def firstp
  {:person :1st})
(def secondp
  {:person :2nd})
(def thirdp
  {:person :3rd})
(def sing
  {:number :singular})
(def plural
  {:number :plural})
(def present
  {:cat :verb
   :infl :present})

;; TODO: move to morphology
(defn italian-pluralize [singular gender]
  (cond
   (= gender :masc)
   (replace #"([oe])$" "i" singular)
   (= gender :fem)
   (replace #"([a])$" "e" singular)))

;; TODO move to morphology
(defn english-pluralize [singular]
  (str (replace #"([sxz])$" "$1e" singular) "s"))

(defn sem-impl [input]
  "expand input feature structures with semantic (really cultural) implicatures, e.g., if human, then not buyable or edible"
  (cond
   (= input :top) input
   true
   (let [activity (if (= (get-in input '(:activity))
                         true)
                    {:animate false
                     :artifact false
                     :consumable false
                     :part-of-human-body false})
         animate (if (= (get-in input '(:animate))
                        true)
                   {:activity false
                    :artifact false
                    :mass false
                    :furniture false
                    :physical-object true
                    :part-of-human-body false
                    :drinkable false
                    :speakable false
                    :place false}{})
         artifact (if (= (get-in input '(:artifact))
                         true)
                    {:animate false
                     :activity false
                     :physical-object true}{})

         buyable (if (= (get-in input '(:buyable))
                        true)
                   {:human false
                    :part-of-human-body false})

         city (if (= (get-in input '(:city))
                     true)
                {:place true
                 :human false
                 :animate false
                 :legible false})

         clothing (if (= (get-in input '(:clothing))
                         true)
                    {:animate false
                     :place false
                     :physical-object true}{})


         consumable (if (= (get-in input '(:consumable)) true)
                      {:activity false
                       :buyable true
                       :furniture false
                       :legible false
                       :pet false
                       :physical-object true
                       :speakable false})

         consumable-false (if (= (get-in input '(:consumable)) false)
                            {:drinkable false
                             :edible false} {})

         drinkable
         ;; drinkables are always mass nouns.
         (if (= (get-in input '(:drinkable)) true)
           {:mass true})

         drinkable-xor-edible-1
         ;; things are either drinkable or edible, but not both (except for weird foods
         ;; like pudding or soup). (part 1: edible)
         (if (and (= (get-in input '(:edible)) true)
                  (= (get-in input '(:drinkable) :notfound) :notfound))
           {:drinkable false}{})

         drinkable-xor-edible-2
         ;; things are either drinkable or edible, but not both (except for weird foods
         ;; like pudding or soup). (part 2: drinkable)
         (if (and (= (get-in input '(:drinkable)) true)
                  (= (get-in input '(:edible) :notfound) :notfound))
           {:edible false})

         ;; qualities of foods and drinks.
         edible (if (or (= (get-in input '(:edible)) true)
                        (= (get-in input '(:drinkable)) true))
                  {:consumable true
                   :human false
                   :pet false
                   :place false
                   :speakable false
                   :legible false
                   :furniture false
                   :part-of-human-body false}{})

         furniture (if (= (get-in input '(:furniture))
                          true)
                     {:artifact true
                      :animate false
                      :buyable true
                      :drinkable false
                      :legible false
                      :edible false
                      :place false
                      :speakable false})

         human (if (= (get-in input '(:human))
                      true)
                 {:activity false
                  :buyable false
                  :physical-object true
                  :edible false
                  :animate true
                  :part-of-human-body false
                  :drinkable false
                  :speakable false
                  :place false}{})
         inanimate (if (= (get-in input '(:animate))
                           false)
                     {:human false
                      :part-of-human-body false}{})

         ;; legible(x) => artifact(x),drinkable(x,false),edible(x,false),human(x,false)
         legible
         (if (= (get-in input '(:legible)) true)
           {:artifact true
            :drinkable false
            :human false
            :furniture false
            :part-of-human-body false
            :edible false})

         material-false
         (if (= (get-in input '(:material)) :false)
           {:edible false
            :animate false
            :drinkable false
            :buyable false ; money can't buy me love..
            :visible false})

         non-places (if (or
                         (= (get-in input '(:legible)) true)
                         (= (get-in input '(:part-of-human-body)) true)
                         (= (get-in input '(:pred)) :fiore)
                         (= (get-in input '(:pred)) :scala))
                   {:place false})

         ;; artifact(x,false) => legible(x,false)
         not-legible-if-not-artifact
         (if (= (get-in input '(:artifact)) false)
           {:legible false})

         part-of-human-body
         (if (= (get-in input '(:part-of-human-body)) true)
           {:speakable false
            :buyable false
            :animate false
            :edible false
            :drinkable false
            :legible false
            :artifact false})

         ;; we don't eat pets (unless things get so desperate that they aren't pets anymore)
         pets (if (= (get-in input '(:pet))
                     true)
                {:edible false
                 :buyable true
                 :physical-object true
                 })

         place (if (= (get-in input '(:place))
                      true)
                 {:activity false
                  :animate false
                  :speakable false
                  :physical-object true
                  :drinkable false
                  :edible false
                  :legible false}{})

         ]
     (let [merged
           (if (= input :fail) :fail
               ;; don't need the features of merge (at least not yet), so use core/merge.
               (core/merge input animate artifact buyable city clothing consumable consumable-false drinkable
                           drinkable-xor-edible-1 drinkable-xor-edible-2
                           edible furniture human inanimate
                           legible material-false non-places
                           not-legible-if-not-artifact part-of-human-body pets place
                      ))]
       (log/debug (str "sem-impl so far: " merged))
       (if (not (= merged input)) ;; TODO: make this check more efficient: count how many rules were hit
         ;; rather than equality-check to see if merged has changed.
         (sem-impl merged) ;; we've added some new information: more implications possible from that.
         merged))))) ;; no more implications: return

(def subject (ref {:cat :noun}))
(def comp-sem (ref {:activity false
                    :discrete false}))

(def pronoun-acc (ref :acc))
(def pronoun-noun (ref :noun))
(def disjunctive-case-of-pronoun (ref :disj))
(def cat-of-pronoun (ref :noun))

(def subcat0 {:synsem {:subcat '()}})
(def subcat1 {:synsem {:subcat {:1 {:cat :top}
                                :2 '()}}})

(def sentential-adverb
  (let [sentential-sem (ref :top)]
    {:synsem {:cat :sent-modifier
              :sem {:subj sentential-sem}
              :subcat {:1 {:sem sentential-sem
                           :subcat '()}}}}))

;; TODO: all these X-common things are going away in favor of lexicon/italiano.
(def andare-common
   {:italiano {:infinitive "andare"
              :essere true
              :drop-e true
              :irregular {:present {:1sing "vado"
                                    :2sing "vai"
                                    :3sing "va"
                                    :1plur "andiamo"
                                    :2plur "andate"
                                    :3plur "vanno"}}}
    :english {:infinitive "to go"
              :irregular {:past "went"
                          :past-participle "gone"}}
    :synsem {:essere true
             :sem {:subj {:animate true}
                   :activity false ;; because "I was going when (something happened) .." sounds weird.
                   :pred :andare
                   :discrete false
                   :motion false}}})

;; TODO: deprecated; use lexicon/italiano.clj instead.
(def avere-common
  {:synsem {:essere false
            :cat :verb}
   :italiano {:infinitive "avere"
             :drop-e true
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
  (let [infl (ref :top)
        agr (ref :top)]
    {:synsem {:essere true
              :subcat {:1 {:agr agr}}
              :agr agr
              :infl infl}
     :italiano {:agr agr
               :futuro-stem "sar"
               :essere true
               :infinitive "essere"
               :infl infl
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
     :english {:agr agr
               :infinitive "to be"
               :infl infl
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
                                  :3plur "were"}}}}))

(def fare-common
  ;; factor out common stuff from all senses of "fare".
  {:synsem {:essere false}
   :italiano {:infinitive "fare"
             :futuro-stem "far"
             :irregular {:passato "fatto"
                         :present {:1sing "faccio"
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
                                      :3plur "facevano"}}}})

(def venire-common
  {:italiano {:infinitive "venire"
             :futuro-stem "verr"
             :irregular {:passato "venuto"
                         :present {:1sing "vengo"
                                   :2sing "vieni"
                                   :3sing "viene"
                                   :1plur "veniamo"
                                   :2plur "venete"
                                   :3plur "vengono"}}}
   :english {:infinitive "to come"
             :irregular {:past "came"}}})

;; "Y is Xer than Z".
(def comparative
  {:synsem {:sem {:comparative true}
            :cat :adjective
            :subcat {:1 {:cat :noun}
                     :2 {:cat :prep
                         :subcat '()}}}})

;; "Y is X."
;; TODO: put semantics here.
(def non-comparative-adjective
  subcat1)

(defn listify [m]
  (into {}
        (for [[k v] m]
          [k (cond (map? v)
                   (vec (list v))
                   (seq? v)
                   (vec v)
                   true
                   v)])))

;; http://stackoverflow.com/questions/1676891/mapping-a-function-on-the-values-of-a-map-in-clojure
;; http://stackoverflow.com/a/1677927
(defn map-function-on-map-vals [m f]
  (into {} (for [[k v] m] [k (f k v)])))

(defn phonize [a-map a-string]
  (let [common {:phrasal false}]
    (cond (or (vector? a-map) (seq? a-map))
          (map (fn [each-entry]
                 (phonize each-entry a-string))
               a-map)

          (and (map? a-map)
               (not (= :no-italiano (get-in a-map [:italiano] :no-italiano))))
          (unify {:italiano {:italiano a-string}}
                 common
                 a-map)

        true
        (unify a-map
               {:italiano a-string}
               common))))


;; TODO: need to regenerate :serialized for each exception.
(defn exception-generator [lexicon]
  (let [lexeme-kv (first lexicon)
        lexemes (second lexeme-kv)]
    (if lexeme-kv
      (let [
            result
            (if false
              nil ;; short-circuit

              (mapcat (fn [path-and-merge-fn]
                        (let [path (:path path-and-merge-fn)
                              merge-fn (:merge-fn path-and-merge-fn)]
                          ;; a lexeme-kv is a pair of a key and value. The key is a string (the word's surface form)
                          ;; and the value is a list of lexemes for that string.
                          (log/debug (str (first lexeme-kv) "looking at path: " path))
                          (mapcat (fn [lexeme]
                                    ;; this is where a unify/dissoc that supported
                                    ;; non-maps like :top and :fail, would be useful:
                                    ;; would not need the (if (not (fail? lexeme)..)) check
                                    ;; to avoid a difficult-to-understand "java.lang.ClassCastException: clojure.lang.Keyword cannot be cast to clojure.lang.IPersistentMap" error.
                                    (let [lexeme (cond (= lexeme :fail)
                                                       :fail
                                                       (= lexeme :top)
                                                       :top
                                                       true
                                                       (dissoc (copy lexeme) :serialized))]
                                      (if (not (= :none (get-in lexeme path :none)))
                                        (list {(get-in lexeme path :none)
                                               (merge lexeme (apply merge-fn (list lexeme)))}))))
                                  lexemes)))
                      [
                       ;; 1. past-tense exceptions
                       {:path [:italiano :passato]
                        :merge-fn
                        (fn [val]
                          {:italiano {:infl :past
                                      :italiano (get-in val [:italiano :passato] :nothing)}})}

                       ;; 2. present-tense exceptions
                       {:path [:italiano :present :1sing]
                        :merge-fn
                        (fn [val]
                          {:italiano {:infl :present
                                      :italiano (get-in val [:italiano :present :1sing] :nothing)
                                      :agr {:number :sing
                                            :person :1st}}})}
                       {:path [:italiano :present :2sing]
                        :merge-fn
                        (fn [val]
                          {:italiano {:infl :present
                                      :italiano (get-in val [:italiano :present :2sing] :nothing)
                                      :agr {:number :sing
                                            :person :2nd}}})}
                       
                       {:path [:italiano :present :3sing]
                        :merge-fn
                        (fn [val]
                          {:italiano {:infl :present
                                      :italiano (get-in val [:italiano :present :3sing] :nothing)
                                      :agr {:number :sing
                                            :person :3rd}}})}

                       {:path [:italiano :present :1plur]
                        :merge-fn
                        (fn [val]
                          {:italiano {:infl :present
                                      :italiano (get-in val [:italiano :present :1plur] :nothing)
                                      :agr {:number :plur
                                            :person :1st}}})}
                       {:path [:italiano :present :2plur]
                        :merge-fn
                        (fn [val]
                          {:italiano {:infl :present
                                      :italiano (get-in val [:italiano :present :2plur] :nothing)
                                      :agr {:number :plur
                                            :person :2nd}}})}
                       
                       {:path [:italiano :present :3plur]
                        :merge-fn
                        (fn [val]
                          {:italiano {:infl :present
                                      :italiano (get-in val [:italiano :present :3plur] :nothing)
                                      :agr {:number :plur
                                            :person :3rd}}})}

                       ]))]
            
            
        (if (not (empty? result))
          (concat result (exception-generator (rest lexicon)))
          (exception-generator (rest lexicon)))))))

(defn check-lexicon [lexicon]
  (let [check-one (fn [k v]
                    (let [result (fail? v)]
                      (if result 
                        (log/warn (str "fail found for: " k)))
                      (if result
                        (list k))))]
    (mapcat
     #(let [key %
            val (get lexicon %)]
        (if (seq? val) 
          (mapcat (fn [x] 
                    (check-one key x))
                  val)
          (check-one key val)))
     (keys lexicon))))
