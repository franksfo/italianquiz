(ns italianverbs.sandbox
  (:require
   [italianverbs.fs :as fs]
   [clojure.string :as string]
   [clojure.tools.logging :as log]))

(defn unify [& args]
  "like fs/unify, but fs/copy each argument before unifying."
  (apply fs/unify
         (map (fn [arg]
                (fs/copy arg))
              args)))

(def human {:human true
            :artifact false
            :legible false
            :edible false ;; sorry, cannibals..
            :animate true})
(def animal {:artifact false
             :legible false
             :animate true})
(def artifact {:artifact true
               :animate false})

(def edible {:edible true
             :legible false
             :human false}) ;; sorry again, cannibals..

(def infinitive-verb
  {:synsem {:cat :verb
            :infl :infinitive}})

(def np-1-lexicon
  (let [gender (ref :top)
        number (ref :top)
        agreement {:synsem {:gender gender
                            :number number}
                   :subcat {:1 {:gender gender
                                :number number}}}]
    (list
     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :number :sing
                         :gender :masc
                         :edible false
                         :human false
                         :animate false
                         :artifact true
                         :person :3rd}
                :subcat {:1 {:cat :det}}
                :italian "compito"
                :english "homework"})

     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :number :sing
                         :gender :masc
                         :edible true
                         :legible false
                         :animate false
                         :human false
                         :artifact true
                         :person :3rd}
                :subcat {:1 {:cat :det}}
                :italian "pane"
                :english "bread"})

     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :number :sing
                         :gender :fem
                         :legible false
                         :animate false
                         :human false
                         :edible true
                         :artifact true
                         :person :3rd}
                :subcat {:1 {:cat :det}}
                :italian "pasta"
                :english "pasta"})


     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :number :sing
                         :gender :fem
                         :human false
                         :legible false
                         :edible false
                         :artifact true
                         :person :3rd}
                :subcat {:1 {:cat :det}}
                :italian "scala"
                :english "ladder"})

     
     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :number :sing
                         :gender :masc
                         :person :3rd
                         }}
               {:synsem (fs/copy human)}
               {:subcat {:1 {:cat :det}}
                :italian "ragazzo"
                :english "guy"})

     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :number :sing
                         :gender :masc
                         :person :3rd
                         }}
               {:synsem (fs/copy human)}
               {:subcat {:1 {:cat :det}}
                :italian "dottore"
                :english "doctor"})

     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :number :sing
                         :gender :fem
                         :person :3rd
                         }}
               {:synsem (fs/copy human)}
               {:subcat {:1 {:cat :det}}
                :italian "professoressa"
                :english "professor"})

     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :number :sing
                         :gender :fem
                         :person :3rd}}
               {:synsem (fs/copy human)
                :subcat {:1 {:cat :det}}
                :italian "ragazza"
                :english "girl"})

     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :human false
                         :number :sing
                         :gender :masc
                         :animate false
                         :artifact true
                         :person :3rd}}
               {:synsem {:legible true}
                :subcat {:1 {:cat :det}}
                :italian "libro"
                :english "book"})

     
     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :human false
                         :number :sing
                         :gender :masc
                         :person :3rd}}
               {:synsem (fs/copy animal)
                :subcat {:1 {:cat :det}}
                :italian "cane"
                :english "dog"})

     (fs/unify (fs/copy agreement)
               {:synsem {:cat :noun
                         :human false
                         :number :sing
                         :gender :masc
                         :person :3rd}}
               {:synsem (fs/copy animal)
                :subcat {:1 {:cat :det}}
                :italian "gatto"
                :english "cat"})
     
     {:synsem {:cat :det
               :def :def
               :gender :masc
               :number :sing}
      :italian "il"
      :english "the"}
     {:synsem {:cat :det
               :def :indef
               :gender :masc
               :number :sing}
      :italian "un"
      :english "a"}

     {:synsem {:cat :det
               :def :def
               :gender :fem
               :number :sing}
      :italian "la"
      :english "the"}
     {:synsem {:cat :det
               :def :def
               :gender :fem
               :number :sing}
      :italian "una"
      :english "a"}

     {:synsem {:cat :det
               :def :def
               :gender :masc
               :number :plur}
      :italian "i"
      :english "the"}
     {:synsem {:cat :det
               :def :def
               :gender :fem
               :number :plur}
      :italian "le"
      :english "the"}

     )))

(def trans-finitizer
  (unify (let [obj (ref :top)
               subj (ref :top)
               italian-infinitive (ref :top)
               cat (ref :top)]
           {:root
            {:italian italian-infinitive
             :subcat {:1 obj
                      :2 subj}
             :synsem {:cat cat}}
            :subcat {:1 obj
                     :2 subj }
            :synsem {:subj subj
                     :obj obj
                     :cat cat
                     :infl :present}
            :italian {:agr subj
                      :infinitive italian-infinitive}})
         (let [subj (ref :top)]
           {:italian
            {:agr subj}})))

(def intrans-finitizer
  (unify (let [obj (ref :top)
               subj (ref :top)
               italian-infinitive (ref :top)
               cat (ref :top)]
           {:root
            {:italian italian-infinitive
             :subcat {:1 subj}
             :synsem {:cat cat}}
            :subcat {:1 subj }
            :synsem {:subj subj
                     :cat cat
                     :infl :present}
            :italian {:agr subj
                      :infinitive italian-infinitive}})
         (let [subj (ref :top)]
           {:italian
            {:agr subj}})))

(def vp-1-lexicon
  (let [verb-with-root
        (let [cat (ref :verb)
              subcat (ref :top)]
          {:synsem {:cat cat}
           :subcat subcat
           :root {:subcat subcat
                  :synsem {:cat cat}}})
        transitive
        (let [subj (ref :top)
              obj (ref :top)]
          {:synsem {:subj subj
                    :obj obj}
           :subcat {:1 obj
                    :2 subj}})

        intransitive
        (let [subj (ref :top)]
          {:synsem {:subj subj}
           :subcat {:1 subj
                    :2 :nil!}})

        finite-transitive
        (let [subj (ref :top)
              obj (ref :top)]
          (fs/unify
           (fs/copy verb-with-root)
           {:synsem {:infl :present}
            :root {:synsem {:subj subj
                            :obj obj}}}))
        
        regular-verb-inflection
        (let [agreement {:person :top
                         :number :top}]
          {:italian {:morph agreement}
           :subcat {:2 agreement}
           :root {:synsem {:cat :verb}}})

        fare
        (fs/unify
         (fs/copy transitive)
         {:italian {:infinitive "fare"
                    :irregular {
                                :present {:1sing "facio"
                                          :2sing "fai"
                                          :3sing "fa"
                                          :1plur "facciamo"
                                          :2plur "fate"
                                          :3plur "fanno"}}}
          :english "to do"
          :synsem {:cat :verb
                   :morph :irreg
                   :infl :infinitive
                   :subj {:human true}
                   :obj {:artifact true}}})

        dormire
        (fs/unify
         (fs/copy intransitive)
         (fs/copy infinitive-verb)
         {:italian "dormire"
          :english "to sleep"
          :synsem {:subj {:animate true}}})

        mangiare
        (fs/unify
         (fs/copy transitive)
         (fs/copy infinitive-verb)
         {:italian "mangiare"
          :english "to eat"
          :synsem {:subj {:animate true}
                   :obj edible}})
        
        leggere
        (fs/unify
         (fs/copy transitive)
         (fs/copy infinitive-verb)
         {:italian "leggere"
          :english "to read"
          :synsem {:subj {:human true}
                   :obj {:legible true}}})

        scrivere
        (fs/unify
         (fs/copy transitive)
         (fs/copy infinitive-verb)
         {:italian "scrivere"
          :english "to write"
          :synsem {:subj {:human true}
                   :obj {:legible true}}})

        ]
    (concat
     (list

      dormire
      (unify {:root dormire}
             intrans-finitizer)
      fare
      (unify {:root fare}
             trans-finitizer)
      leggere
      (unify {:root leggere}
             trans-finitizer)
      mangiare
      (unify {:root mangiare}
             trans-finitizer)

      scrivere
      (unify {:root scrivere}
             trans-finitizer)))))

(def vp-1-rules
  (list
   (let [vp-rule-1
         (let [comp-synsem (ref {:cat :noun :case :acc})
               comp (ref {:synsem comp-synsem :subcat :nil!})
               subj (ref {:cat :noun :case :nom})
               head-synsem (ref {:cat :verb
                                 :infl {:not :infinitive}
                                 :subj subj
                                 :obj comp-synsem})
               head (ref {:synsem head-synsem
                          :subcat {:1 comp-synsem
                                   :2 subj}})]
           {:comment "vp -> head comp"
            :head head
            :subcat {:1 subj}
            :synsem head-synsem
            :comp comp
            :1 head
            :2 comp})]
     vp-rule-1)))

(def pronouns
  (list {:synsem {:cat :noun
                  :case :nom
                  :human true
                  :artifact false ;; <- :human true => artifact :false
                  :person :1st
                  :number :sing}
         :subcat :nil!
         :italian "io"}
        {:synsem {:cat :noun
                  :case :nom
                  :human true
                  :artifact false ;; <- :human true => artifact :false
                  :person :2nd
                  :number :sing}
         :subcat :nil!
         :italian "tu"}
        {:synsem {:cat :noun
                  :case :nom
                  :human true
                  :artifact false ;; <- :human true => artifact :false
                  :person :3rd
                  :gender :masc
                  :number :sing}
         :subcat :nil!
         :italian "lui"}
        {:synsem {:cat :noun
                  :case :nom
                  :human true
                  :artifact false ;; <- :human true => artifact :false
                  :person :3rd
                  :gender :fem
                  :number :sing}
         :subcat :nil!
         :english "she"
         :italian "lei"}
        {:synsem {:cat :noun
                  :case :nom
                  :human true
                  :artifact false ;; <- :human true => artifact :false
                  :person :1st
                  :number :plur}
         :subcat :nil!
         :english "we"
         :italian "noi"}
        {:synsem {:cat :noun
                  :case :nom
                  :human true
                  :artifact false ;; <- :human true => artifact :false
                  :person :2nd
                  :number :plur}
         :subcat :nil!
         :italian "voi"}
        {:synsem {:cat :noun
                  :case :nom
                  :human true
                  :artifact false ;; <- :human true => artifact :false
                  :person :3rd
                  :number :plur}
         :subcat :nil!
         :italian "loro"}))

(def sentence-rules
  (let [subcatted (ref {:cat :noun})
        head-synsem (ref {:cat :verb
                          :infl {:not :infinitive}
                          :subj subcatted
                          })
        comp (ref {:synsem subcatted :subcat :nil!})
        head (ref {:synsem head-synsem
                   :subcat {:1 subcatted
                            :2 :nil!
                            }})]
    (list
     {:comment "s -> np vp"
      :subcat :nil!
      :head head
      :comp comp
      :1 comp
      :2 head
      :synsem head-synsem
      })))

(def np-1-rules 
  (let [np-rule-1 ;; NP -> Comp Head
        (let [comp-synsem (ref {:cat :det})
              comp (ref {:synsem comp-synsem})
              head-synsem (ref {:cat :noun})
              head (ref {:synsem head-synsem
                         :subcat {:1 comp-synsem}})]
          {:comment "np -> det noun"
           :head head
           :subcat :nil!
           :synsem head-synsem
           :comp comp
           :1 comp
           :2 head})]
    (list np-rule-1)))

(def lexicon (concat vp-1-lexicon np-1-lexicon pronouns))

(defn lookup-in [query collection]
  "find all members of the collection that matches with query successfully."
  (if (= (.size collection) 0)
    nil
    (let [result (fs/match query (fs/copy (first collection)))]
      (if (not (fs/fail? result))
        (cons (first collection) (lookup-in query (rest collection)))
        (lookup-in query (rest collection))))))

(defn lookup [query]
  (lookup-in query lexicon))

(defn it [italian]
  (lookup {:italian italian}))

(def rules (concat np-1-rules vp-1-rules sentence-rules))

(def np (nth rules 0))
(def vp (nth rules 1))
(def s (nth rules 2))

(defn conjugate [arg]
  (cond (nil? arg) ""
        (= (type arg) java.lang.String)
        arg
        (and (or (= (type arg)
                    clojure.lang.PersistentArrayMap)
                 (= (type arg)
                    clojure.lang.PersistentHashMap))
             (contains? (set (keys arg)) :1)
             (contains? (set (keys arg)) :2))
        (let [result1 (conjugate (:1 arg))
              result2 (conjugate (:2 arg))]
          (if (and (= (type result1) java.lang.String)
                   (= (type result2) java.lang.String))
            (string/join " " (list result1 result2))
            {:1 result1
             :2 result2}))

        (and (or (= (type arg)
                    clojure.lang.PersistentArrayMap)
                 (= (type arg)
                    clojure.lang.PersistentHashMap))
             (contains? (set (keys arg)) :agr)
             (contains? (set (keys arg)) :infinitive)
             (not (= java.lang.String (type (fs/get-in arg '(:infinitive))))))
        ;; irregular present-tense (e.g. "fare")
        (let [root (fs/get-in arg '(:infinitive))
              person (fs/get-in arg '(:agr :person))
              number (fs/get-in arg '(:agr :number))
              present (fs/get-in arg '(:infinitive :irregular :present))]
          (cond
           (and (= person :1st)
                (= number :sing))
           (fs/get-in present '(:1sing))
           (and (= person :2nd)
                (= number :sing))
           (fs/get-in present '(:2sing))
           (and (= person :3rd)
                (= number :sing))
           (fs/get-in present '(:3sing))
           (and (= person :1st)
                (= number :plur))
           (fs/get-in present '(:1plur))
           (and (= person :2nd)
                (= number :plur))
           (fs/get-in present '(:2plur))
           (and (= person :3rd)
                (= number :plur))
           (fs/get-in present '(:3plur))
           :else arg))  ;(str "[unknown conjugation:root=" root ";person=" person ";number=" number "]:" root)))

        (= (type arg) clojure.lang.Keyword)
        (str "cannot conjugate: " arg)
        
        :else
        ;; assume a map with keys (:root and :agr).
        (let [root (fs/get-in arg '(:infinitive))
              root (if (nil? root) "(nil)" root)
              root (if (not (= (type root) java.lang.String))
                      (fs/get-in arg '(:infinitive :infinitive))
                      root)
              person (fs/get-in arg '(:agr :person))
              number (fs/get-in arg '(:agr :number))
              stem (string/replace root #"[iae]re$" "")
              are-type (re-find #"are$" root)
              ere-type (re-find #"ere$" root)
              ire-type (re-find #"ire$" root)
              last-stem-char-is-i (re-find #"i$" stem)]
          (cond

           (and (= person :1st) (= number :sing))
           (str stem "o")

           (and (= person :2nd) (= number :sing)
                last-stem-char-is-i)
           (str stem)

           (and (= person :2nd) (= number :sing))
           (str stem "i")

           (and (= person :3rd) (= number :sing) (or ire-type ere-type))
           (str stem "e")

           (and (= person :3rd) (= number :sing) are-type)
           (str stem "a") 

           (and (= person :1st) (= number :plur)
                last-stem-char-is-i)
           (str stem "amo")

           (and (= person :1st) (= number :plur))
           (str stem "iamo")

           (and (= person :2nd) (= number :plur) are-type)
           (str stem "ate")

           (and (= person :2nd) (= number :plur) ere-type)
           (str stem "ete")

           (and (= person :2nd) (= number :plur) ire-type)
           (str stem "ite")

           (and (= person :3rd) (= number :plur))
           (str stem "ano")
           :else arg))))

(defn get-italian [a b]
  (let [conjugated-a (conjugate a)
        conjugated-b (if (not (nil? b)) (conjugate b) "")]
    (if (and
         (= (type conjugated-a) java.lang.String)
         (= (type conjugated-b) java.lang.String))
      (string/trim (str conjugated-a " " conjugated-b))
      {:1 conjugated-a
       :2 conjugated-b})))

(defn en [english]
  (lookup {:english english}))

(defn finitize [infinitive]
  (unify {:root infinitive}
         trans-finitizer))

(def mangiare-finite
  (unify {:root (first (it "mangiare"))}
         trans-finitizer))

(defn get-in [map path]
  (fs/get-in map path))

(defn over [parent child]
  (cond

   (= (type child) java.lang.String)
   (over parent (it child))
   
   (or (= (type parent) clojure.lang.LazySeq)
       (= (type parent) clojure.lang.PersistentList))
   (flatten
    (mapcat (fn [each-parent]
              (over each-parent child))
            parent))
   (or (= (type child) clojure.lang.LazySeq)
       (= (type child) clojure.lang.PersistentList)
       (= (type child) clojure.lang.Cons))
      (remove (fn [result]
             (or (fs/fail? result)
                 (nil? result)))
              (flatten
               (map (fn [each-child]
                      (let [parent parent
                            child each-child]
                        (over parent child)))
                    child)))

   :else ; both parent and child are non-lists.
   (let [result
         (let [
               ;; "as": find where to attach child (:1 or :b), depending on value of current left child (:a)'s :italian.
               ;; if (:1 :italian) is nil, the parent has no :a-child, so attach new child there at :a.
               ;; Otherwise, an :a-child exists for the parent, so attach new child at :b.
               as (if (nil?
                       (fs/get-in parent '(:1 :italian)))
                    :1
                    :2)
               unified (unify parent
                              {as child})
               italian
               (get-italian
                (fs/get-in unified '(:1 :italian))
                (fs/get-in unified '(:2 :italian)))
               ]
           (merge ;; use merge so that we overwrite the value for :italian.
            unified
            {:italian italian}))]
     (if (not (fs/fail? result))
       result))))

(defn regular-sentence []
  (let [ilragazzo (over (over np "il") "ragazzo")
        lapasta (over (over np "la") "pasta")
        myvp (over (over vp mangiare-finite) lapasta)
        sentence (over (over s ilragazzo) myvp)]
    sentence))

(defn lots-of-sentences-1 []
  (over
   (over s
         lexicon)
   (over
    (over vp lexicon)
    (over (over np lexicon) lexicon))))

(defn lots-of-sentences-2 []
  (over
   (over s
         (over (over np lexicon) lexicon))
   (over
    (over vp lexicon)
    (over (over np lexicon) lexicon))))

(defn lots-of-sentences []
  (concat
   (lots-of-sentences-1)
   (lots-of-sentences-2)))

(defn formattare [expressions]
;; "format a bunch of expressions (feature-structures) showing just the italian."
  (if (or (= (type expressions) clojure.lang.PersistentArrayMap)
          (= (type expressions) clojure.lang.PersistentHashMap)
          (= (type expressions) clojure.lang.PersistentTreeMap))
    (formattare (list expressions))
    
    (map (fn [expr] (string/capitalize
                     (string/trim
                      (str
                       (string/trim
                        (get-italian
                         (let [italian (fs/get-in expr '(:italian))]
                           (cond
                            
                            (and (or (= (type italian)
                                        clojure.lang.PersistentArrayMap)
                                     (= (type italian)
                                        clojure.lang.PersistentHashMap))
                                 (not (nil? (fs/get-in italian '(:1 :infinitive :infinitive)))))
                            (string/join " " (list (fs/get-in italian '(:1 :infinitive :infinitive))
                                                   "(finite)"
                                                   (get-italian (fs/get-in italian '(:2)) "")))
                            
                            (and (or (= (type italian)
                                        clojure.lang.PersistentArrayMap)
                                     (= (type italian)
                                        clojure.lang.PersistentHashMap))
                                 (not (nil? (fs/get-in italian '(:1 :infinitive)))))
                            (string/join " " (list (get-italian (fs/get-in italian '(:1 :infinitive)) "")
                                                   "(finite)"
                                                   (get-italian (fs/get-in italian '(:2)) "")))
                            
                            
                            
                            (and (or (= (type italian)
                                        clojure.lang.PersistentArrayMap)
                                     (= (type italian)
                                        clojure.lang.PersistentHashMap))
                                 (not (nil? (fs/get-in italian '(:irregular)))))
                            (str (fs/get-in italian '(:infinitive)) " (finite)")
                            
                            (and (or (= (type italian)
                                        clojure.lang.PersistentArrayMap)
                                     (= (type italian)
                                        clojure.lang.PersistentHashMap))
                                 (not (nil? (fs/get-in italian '(:infinitive))))
                                 (= java.lang.String (type (fs/get-in italian '(:infinitive)))))
                            (str (fs/get-in italian '(:infinitive)) " (finite)")
                            
                            (and (or (= (type italian)
                                        clojure.lang.PersistentArrayMap)
                                     (= (type italian)
                                        clojure.lang.PersistentHashMap))
                                 (not (nil? (fs/get-in italian '(:infinitive)))))
                            (fs/get-in italian '(:infinitive :infinitive))
                           
                           :else
                           italian))
                         ""))
                       "."
                       (if (fs/get-in expr '(:comment)) (str " (" (fs/get-in expr '(:comment)) ")"))))))
         expressions)))
